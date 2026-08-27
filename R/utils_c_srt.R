# =============================================================================
# utils_c_srt.R
# 分类变量频率描述 + 秩和检验函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_c_srt_server 返回的参数
#   - 构建标准宽表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象
#
# 累积缓存：使用 .GlobalEnv$.table_out1（outyn=0 时累积，outyn=1 时出表并清空）
# =============================================================================

#' 分类变量频率描述 + 秩和检验
#'
#' @param inds          数据框对象
#' @param data_cond     数据筛选条件
#' @param varlist       分析变量描述："变量名|变量标签|值1=标签1/值2=标签2/..."
#' @param group_c       分组描述："分组变量名|组名1/组名2/..."
#' @param coltotal      是否输出合计行（1=是，0=否）
#' @param rowtotal      是否输出合计列（1=是，0=否）
#' @param outyn         是否立即出表（1=是，0=仅累积）
#' @param test_between  是否进行组间检验（1=秩和检验，0=否）
#' @param test_in       是否进行组内符号秩检验（1=是，0=否）
#' @param table_title   表格标题
#' @param ftnote        底注内容
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
c_srt <- function(inds, data_cond, varlist, group_c, coltotal, rowtotal,
                  outyn = 1, test_between, test_in, table_title, ftnote) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar    <- trimws(grp_parts[1])
  grpnames  <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grpnames  <- grpnames[nchar(grpnames) > 0]
  grp_num   <- length(grpnames)

  vl_parts  <- strsplit(varlist, "|", fixed = TRUE)[[1]]
  ana_var   <- trimws(vl_parts[1])
  ana_label <- trimws(vl_parts[2])
  cat_str   <- if (length(vl_parts) >= 3) trimws(vl_parts[3]) else ""

  cat_items  <- strsplit(cat_str, "/", fixed = TRUE)[[1]]
  cat_items  <- cat_items[nchar(trimws(cat_items)) > 0]
  cat_vals   <- vapply(cat_items, function(x) {
    trimws(strsplit(x, "=", fixed = TRUE)[[1]][1])
  }, character(1))
  cat_labels <- vapply(cat_items, function(x) {
    trimws(strsplit(x, "=", fixed = TRUE)[[1]][2])
  }, character(1))
  cat_num    <- length(cat_vals)

  # ============================================================
  # Step 2：筛选数据
  # ============================================================
  data_0 <- inds
  data_0 <- data_0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(ana_var, grpvar))) |>
    stats::setNames(c("var_0", "group_0")) |>
    dplyr::mutate(
      grp_cd = match(group_0, grpnames),
      cat_cd = match(as.character(var_0), cat_vals)
    )

  # ============================================================
  # Step 3：计算各组分母 N
  # ============================================================
  n_denom <- d_0 |>
    dplyr::group_by(grp_cd) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  denom_vec <- stats::setNames(n_denom$n, as.character(n_denom$grp_cd))
  n_total   <- sum(n_denom$n)

  # ============================================================
  # Step 4：计算频数矩阵
  # ============================================================
  freq_df <- d_0 |>
    dplyr::filter(!is.na(cat_cd)) |>
    dplyr::count(cat_cd, grp_cd, name = "freq") |>
    tidyr::complete(cat_cd = seq_len(cat_num), grp_cd = seq_len(grp_num),
                    fill = list(freq = 0L))

  grp_totals <- d_0 |>
    dplyr::filter(!is.na(cat_cd)) |>
    dplyr::count(grp_cd, name = "freq") |>
    dplyr::mutate(cat_cd = 999L)

  # ============================================================
  # Step 5：构造宽格式输出
  # ============================================================
  .fmt_cell <- function(n, denom) {
    pct <- if (!is.na(denom) && denom > 0) sprintf("%.2f", n / denom * 100) else "0.00"
    paste0(n, "(", pct, ")")
  }

  # 各分类行
  cat_rows <- lapply(seq_len(cat_num), function(ci) {
    grp_cells <- vapply(seq_len(grp_num), function(gi) {
      n_ij <- freq_df$freq[freq_df$cat_cd == ci & freq_df$grp_cd == gi]
      n_ij <- if (length(n_ij) == 0) 0L else as.integer(n_ij)
      .fmt_cell(n_ij, denom_vec[as.character(gi)])
    }, character(1))

    n_row   <- sum(freq_df$freq[freq_df$cat_cd == ci])
    pct_row <- if (n_total > 0) sprintf("%.2f", n_row / n_total * 100) else "0.00"
    total_cell <- paste0(n_row, "(", pct_row, ")")

    c(paste0("  ", cat_labels[ci], "(%)"), grp_cells, total_cell)
  })

  # 合计行
  total_row_cells <- vapply(seq_len(grp_num), function(gi) {
    n_gi    <- as.integer(grp_totals$freq[grp_totals$grp_cd == gi])
    n_gi    <- if (length(n_gi) == 0) 0L else n_gi
    missing <- as.integer(denom_vec[as.character(gi)]) - n_gi
    paste0(n_gi, "(", missing, ")")
  }, character(1))
  total_all <- sum(grp_totals$freq)
  miss_t    <- n_total - total_all
  total_row <- c("  合计(Missing)", total_row_cells, paste0(total_all, "(", miss_t, ")"))

  header_row <- c(ana_label, rep("", grp_num + 1))
  all_rows   <- c(list(header_row), cat_rows)
  if (coltotal == 1) all_rows <- c(all_rows, list(total_row))

  # ============================================================
  # Step 6：组内符号秩检验
  # ============================================================
  sign_rank_row <- NULL
  if (!is.null(test_in) && test_in == 1) {
    sr_cells <- vapply(seq_len(grp_num), function(gi) {
      grp_data <- d_0 |> dplyr::filter(grp_cd == gi, !is.na(var_0))
      if (nrow(grp_data) == 0) return("NA")
      x <- suppressWarnings(as.numeric(grp_data$var_0))
      x <- x[!is.na(x)]
      x <- x[x != 0]
      n <- length(x)
      if (n == 0) return("NA")
      sr <- tryCatch(
        stats::wilcox.test(x, mu = 0, alternative = "two.sided",
                           exact = FALSE, correct = FALSE),
        error = function(e) NULL
      )
      if (is.null(sr)) return("NA")
      V <- sr$statistic[[1]]
      S <- V - n * (n + 1) / 4
      s_val <- sprintf("%.2f", S)
      p_raw <- sr$p.value
      p_str <- if (p_raw < 0.0001) "<.0001" else sprintf("%.4f", p_raw)
      paste0(s_val, "(", p_str, ")")
    }, character(1))
    sign_rank_row <- c("  符号秩(P值)", sr_cells, "")
  }

  if (!is.null(sign_rank_row)) all_rows <- c(all_rows, list(sign_rank_row))

  out_df <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
  col_name_label  <- ".label"
  col_name_groups <- make.names(grpnames, unique = TRUE)
  col_name_total  <- ".total"
  names(out_df) <- c(col_name_label, col_name_groups, col_name_total)

  # ============================================================
  # Step 7：组间检验（秩和）
  # ============================================================
  stat_str <- NA_character_
  pval_str <- NA_character_

  if (!is.null(test_between) && test_between == 1 && grp_num >= 2) {
    d_rank <- d_0 |> dplyr::filter(!is.na(cat_cd))
    if (grp_num == 2) {
      wt <- tryCatch(
        stats::wilcox.test(cat_cd ~ grp_cd, data = d_rank,
                           alternative = "two.sided", exact = FALSE),
        error = function(e) NULL
      )
      if (!is.null(wt)) {
        stat_str <- paste0(sprintf("%.2f", wt$statistic[[1]]), "(Wilcoxon秩和检验)")
        pval_str <- if (wt$p.value < 0.0001) "<.0001" else sprintf("%.4f", wt$p.value)
      }
    } else {
      kt <- tryCatch(
        stats::kruskal.test(cat_cd ~ grp_cd, data = d_rank),
        error = function(e) NULL
      )
      if (!is.null(kt)) {
        stat_str <- paste0(sprintf("%.2f", kt$statistic[[1]]), "(Kruskal-Wallis H检验)")
        pval_str <- if (kt$p.value < 0.0001) "<.0001" else sprintf("%.4f", kt$p.value)
      }
    }
  }

  if (!is.na(stat_str)) {
    out_df[[".stat"]] <- NA_character_
    out_df[[".pval"]] <- NA_character_
    out_df[[".stat"]][1] <- stat_str
    out_df[[".pval"]][1] <- pval_str
  }

  # ============================================================
  # Step 8：累积到全局缓存
  # ============================================================
  if (!exists(".table_out1", envir = globalenv())) {
    assign(".table_out1", NULL, envir = globalenv())
  }
  .GlobalEnv$.table_out1 <- dplyr::bind_rows(.GlobalEnv$.table_out1, out_df)

  if (outyn != 1) return(invisible(NULL))

  # ============================================================
  # Step 9：outyn=1 → 构造 varlist，调用 report_table()
  # ============================================================
  final_df <- .GlobalEnv$.table_out1
  .GlobalEnv$.table_out1 <- NULL

  # 表头标签
  grp_col_labels <- vapply(seq_len(grp_num), function(i) {
    n_i <- as.integer(denom_vec[as.character(i)])
    n_i <- if (is.na(n_i)) 0L else n_i
    paste0(grpnames[i], "$(N=", n_i, ")")
  }, character(1))
  total_n_lbl <- paste0("合计$(N=", n_total, ")")

  varlist_parts <- c(
    paste0(col_name_label, "/"),
    paste0(col_name_groups, "/", grp_col_labels)
  )
  if (rowtotal == 1) {
    varlist_parts <- c(varlist_parts, paste0(col_name_total, "/", total_n_lbl))
  }
  if (!is.na(stat_str) && ".stat" %in% names(final_df)) {
    varlist_parts <- c(varlist_parts, ".stat/统计量", ".pval/P值")
  }
  varlist_str <- paste(varlist_parts, collapse = "|")

  # 加粗行：变量标签首行 + 各指标首行
  bold_idx <- which(!startsWith(final_df[[col_name_label]], "  ") &
                    !is.na(final_df[[col_name_label]]) &
                    nchar(trimws(final_df[[col_name_label]])) > 0)

  ft <- report_table(
    data       = final_df,
    varlist    = varlist_str,
    title      = table_title,
    footnote   = ftnote,
    headerjust = "center",
    col1just   = "left",
    columnjust = "center",
    autoaddnum = "yes",
    bold_rows  = if (length(bold_idx) > 0) bold_idx else NULL
  )

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- table_title
  attr(ft, "hbr_footnote") <- ftnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left",
    bold_rows = if (length(bold_idx) > 0) bold_idx else NULL
  )

  ft
}
