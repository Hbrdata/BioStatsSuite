# =============================================================================
# utils_q_param.R
# 连续型变量描述性统计 + 组间/组内检验函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_q_param_server 返回的参数
#   - 构建标准宽表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象
# =============================================================================

#' 连续型变量描述性统计 + 组间/组内检验
#'
#' @param inds              数据框对象
#' @param data_cond         数据筛选条件
#' @param denominator_data  分母数据框（可选），若为 NULL 则使用 inds
#' @param denominator_cond  分母数据筛选条件（表头 N=XX）
#' @param group_c          分组描述："分组变量名|组名1/组名2/..."
#' @param varlist          分析变量描述："变量名|变量标签"
#' @param rowtotal         是否输出合计列（1=是，0=否）
#' @param pairt            是否进行配对t检验（1=是，0=否）
#' @param outyn            是否立即出表（1=是，0=返回数据框）
#' @param test_between     是否进行组间检验（1=是，0=否）
#' @param title            表格标题
#' @param footnote         底注内容
#'
#' @return flextable 对象（outyn=1）或 data.frame（outyn=0）
#' @noRd
q_param <- function(inds, data_cond, denominator_data = NULL, denominator_cond = "TRUE", group_c, varlist,
                    rowtotal, pairt, outyn = 1, test_between, title, footnote) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  data_cond <- .normalize_filter_expr(data_cond)
  denominator_cond <- .normalize_filter_expr(denominator_cond)

  group_spec <- .parse_group_c(group_c)
  grpvar <- group_spec$var
  grpnames <- group_spec$levels
  grp_num <- group_spec$n

  var_spec <- .parse_varlist_spec(varlist)
  ana_var <- var_spec$var
  ana_label <- var_spec$label

  # ============================================================
  # Step 2：筛选分析数据
  # ============================================================
  data_0 <- .filter_by_expr(inds, data_cond)
  data_0 <- data_0 |>
    dplyr::filter(.data[[grpvar]] %in% grpnames) |>
    dplyr::mutate(!!grpvar := factor(.data[[grpvar]], levels = grpnames))

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(ana_var, grpvar))) |>
    stats::setNames(c("var_0", "group_0"))

  if (nrow(d_0) == 0) {
    warning("筛选后数据为空，请检查数据筛选条件和分组条件是否正确。")
    all_groups <- c(grpnames, "合计")
    empty_out <- data.frame(.label = ana_label, check.names = FALSE)
    for (gn in all_groups) empty_out[[make.names(gn)]] <- ""
    if (test_between == 1) { empty_out[[".stat"]] <- NA_character_; empty_out[[".pval"]] <- NA_character_ }
    return(empty_out)
  }

  # ============================================================
  # Step 3：筛选分母数据（用于表头 N=XX）
  # ============================================================
  den_source <- if (!is.null(denominator_data)) denominator_data else inds
  data_1 <- .filter_by_expr(den_source, denominator_cond)
  data_1 <- data_1 |>
    dplyr::filter(.data[[grpvar]] %in% grpnames) |>
    dplyr::mutate(!!grpvar := factor(.data[[grpvar]], levels = grpnames))

  group_order <- c(grpnames, "合计")
  n_by_group <- .group_denominator_n(data_1, grpvar, grpnames)
  n_total <- nrow(data_1)
  n_lookup <- stats::setNames(c(n_by_group, n_total), group_order)

  # ============================================================
  # Step 4：描述性统计
  # ============================================================
  .desc_stats <- function(df) {
    df |>
      dplyr::summarise(
        n        = sum(!is.na(var_0)),
        missing  = sum(is.na(var_0)),
        mean_val = mean(var_0, na.rm = TRUE),
        sd_val   = stats::sd(var_0, na.rm = TRUE),
        med_val  = stats::median(var_0, na.rm = TRUE),
        q1_val   = stats::quantile(var_0, probs = 0.25, type = 2, na.rm = TRUE),
        q3_val   = stats::quantile(var_0, probs = 0.75, type = 2, na.rm = TRUE),
        min_val  = min(var_0, na.rm = TRUE),
        max_val  = max(var_0, na.rm = TRUE),
        .groups  = "drop"
      ) |>
      dplyr::mutate(
        N_Missing    = paste0(n, "(", missing, ")"),
        Mean_SD      = paste0(.format_num2(mean_val), "(", .format_num2(sd_val), ")"),
        Median_Q1_Q3 = paste0(.format_num2(med_val),
                               "(", .format_num2(q1_val),
                               ",", .format_num2(q3_val), ")"),
        Min_Max      = paste0(.format_num2(min_val), ",", .format_num2(max_val))
      ) |>
      dplyr::select(dplyr::any_of("group_0"), n, N_Missing, Mean_SD, Median_Q1_Q3, Min_Max)
  }

  s_by_group <- d_0 |>
    dplyr::group_by(group_0) |>
    .desc_stats() |>
    dplyr::mutate(group_0 = as.character(group_0))

  s_total <- d_0 |>
    .desc_stats() |>
    dplyr::mutate(group_0 = "合计", .before = 1)

  s_all <- dplyr::bind_rows(s_by_group, s_total)
  s_all_ordered <- s_all[match(group_order, s_all$group_0), ]

  # ============================================================
  # Step 5：组间检验
  # ============================================================
  if (test_between == 1) {
    d_0$group_0 <- factor(d_0$group_0, levels = grpnames)
    if (grp_num >= 3) {
      aov_res   <- summary(stats::aov(var_0 ~ group_0, data = d_0))
      test_stat <- paste0(sprintf("%.2f", aov_res[[1]][["F value"]][1]), "（方差检验）")
      test_p    <- sprintf("%.4f", aov_res[[1]][["Pr(>F)"]][1])
    } else if (grp_num == 2) {
      tt        <- stats::t.test(var_0 ~ group_0, var.equal = TRUE, data = d_0)
      test_stat <- paste0(sprintf("%.2f", tt$statistic[[1]]), "（独立样本t检验）")
      test_p    <- sprintf("%.4f", tt$p.value)
    } else {
      test_stat <- NA_character_
      test_p    <- NA_character_
    }
  } else {
    test_stat <- NA_character_
    test_p    <- NA_character_
  }

  # ============================================================
  # Step 6：组内配对 t 检验
  # ============================================================
  if (pairt == 1) {
    pairt_results <- vapply(grpnames, function(g) {
      grp_data <- d_0 |> dplyr::filter(group_0 == g)
      tt <- stats::t.test(grp_data$var_0, alternative = "two.sided")
      t_val <- sprintf("%.2f", tt$statistic[[1]])
      p_val <- tt$p.value
      p_str <- if (p_val < 0.0001) "<.0001" else sprintf("%.4f", p_val)
      paste0(t_val, "(", p_str, ")")
    }, character(1))
  }

  # ============================================================
  # Step 7：构造宽格式输出
  # ============================================================
  col_name_label  <- ".label"
  col_name_groups <- make.names(group_order, unique = TRUE)

  make_stat_row <- function(lbl, col) {
    vals <- as.character(s_all_ordered[[col]])
    c(lbl, vals)
  }

  label_row   <- c(ana_label, rep("", length(group_order)))
  n_missing_r <- make_stat_row("N(Missing)", "N_Missing")
  mean_sd_r   <- make_stat_row("Mean(SD)", "Mean_SD")
  median_r    <- make_stat_row("Median(Q1,Q3)", "Median_Q1_Q3")
  minmax_r    <- make_stat_row("Min,Max", "Min_Max")

  all_rows <- list(label_row, n_missing_r, mean_sd_r, median_r, minmax_r)

  if (pairt == 1) {
    pairt_row <- c("配对t检验(P值)", as.character(pairt_results), "")
    all_rows  <- c(all_rows, list(pairt_row))
  }

  out_df        <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
  names(out_df) <- c(col_name_label, col_name_groups)

  if (test_between == 1) {
    out_df[[".stat"]] <- NA_character_
    out_df[[".pval"]] <- NA_character_
    out_df[[".stat"]][1] <- test_stat
    out_df[[".pval"]][1] <- test_p
  }

  if (rowtotal == 0) {
    total_col <- col_name_groups[length(col_name_groups)]
    out_df    <- out_df[, names(out_df) != total_col, drop = FALSE]
  }

  if (outyn == 0) return(invisible(out_df))

  # ============================================================
  # Step 8：构造 varlist，调用 report_table()
  # ============================================================
  col_headers <- vapply(group_order, function(g) {
    paste0(g, "\n(n = ", n_lookup[g], ")")
  }, character(1))

  parts <- paste0(col_name_label, "/")

  if (rowtotal == 1) {
    parts <- c(parts, paste0(col_name_groups, "/", col_headers))
  } else {
    parts <- c(parts, paste0(col_name_groups[-length(col_name_groups)], "/",
                              col_headers[-length(col_headers)]))
  }

  if (test_between == 1) {
    parts <- c(parts, ".stat/统计量", ".pval/P值")
  }

  varlist_str <- paste(parts, collapse = "|")

  ft <- report_table(
    data       = out_df,
    varlist    = varlist_str,
    title      = title,
    footnote   = footnote,
    headerjust = "center",
    col1just   = "left",
    columnjust = "center",
    autoaddnum = "yes",
    bold_rows  = 1L
  )

  .attach_report_attrs(
    ft,
    varlist_str,
    title,
    footnote,
    list(headerjust = "center", columnjust = "center", col1just = "left", bold_rows = 1L)
  )
}
