# =============================================================================
# utils_c_cmh.R
# CMH 卡方检验及定性数据分类描述函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_c_cmh_server 返回的参数
#   - 构建标准宽表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象
#
# 累积缓存：使用 .ccmh_env 私有环境（outyn=0 时累积，outyn=1 时出表并清空）
# =============================================================================

# 私有累积环境：存储叠加表格，不污染 .GlobalEnv
.ccmh_env <- new.env(parent = emptyenv())
.ccmh_env$table_out <- NULL

#' CMH 卡方检验及定性数据分类描述
#'
#' @param inds      数据框对象
#' @param data_cond 数据筛选条件（R 表达式字符串，如 "fas=='是'"）
#' @param group     分组描述："分组变量名|组名1/组名2/..."
#' @param varlist   分析变量描述："变量名|变量标签|值1=标签1/值2=标签2/..."
#' @param cmhvar    CMH 分层变量名（一般为中心号）
#' @param method    CMH 分析方法：1=非零相关，2=行均值得分差值，3=一般关联
#' @param alpha     置信水平，默认 0.05
#' @param outp      是否输出统计量和P值列（1=是，0=否）
#' @param outn      是否在表头显示 N=XX（1=是，0=否）
#' @param coltotal  是否输出合计行（1=是，0=否）
#' @param rowtotal  是否输出合计列（1=是，0=否）
#' @param title     表格标题
#' @param footnote  底注内容
#' @param outyn     是否立即出表（1=是，0=仅累积）
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
c_cmh <- function(inds, data_cond, group, varlist, cmhvar, method = 1,
                  alpha = 0.05, outp = 1, outn = 1, coltotal = 1,
                  rowtotal = 0, title = NULL, footnote = NULL, outyn = 1) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  group_parts <- stringr::str_split(group, "\\|", simplify = TRUE)
  grp_var     <- trimws(group_parts[1])
  grp_names   <- stringr::str_split(trimws(group_parts[2]), "/", simplify = TRUE)
  grp_names   <- grp_names[nchar(trimws(grp_names)) > 0]
  grp_num     <- length(grp_names)

  varlist_parts <- stringr::str_split(varlist, "\\|", n = 3, simplify = TRUE)
  ana_var       <- trimws(varlist_parts[1])
  ana_label     <- trimws(varlist_parts[2])
  cat_list_str  <- trimws(varlist_parts[3])

  cat_defs  <- stringr::str_split(cat_list_str, "/", simplify = TRUE)
  cat_defs  <- cat_defs[nchar(trimws(cat_defs)) > 0]
  cat_num   <- length(cat_defs)

  cat_cont  <- character(cat_num)
  cat_label <- character(cat_num)
  for (i in seq_len(cat_num)) {
    parts        <- stringr::str_split(cat_defs[i], "=", n = 2, simplify = TRUE)
    cat_cont[i]  <- trimws(parts[1])
    cat_label[i] <- trimws(parts[2])
  }
  # 去掉分类值两端引号，统一为字符串用于 match
  cat_cont_clean <- stringr::str_remove_all(cat_cont, "['\"]")

  # ============================================================
  # Step 2：筛选数据集
  # ============================================================
  if (!is.null(data_cond) && nchar(trimws(data_cond)) > 0 && trimws(data_cond) != "TRUE") {
    s0 <- inds |> dplyr::filter(!!rlang::parse_expr(data_cond))
  } else {
    s0 <- inds
  }
  s0 <- s0 |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(grp_var), as.character),
      .grpcd    = match(as.character(.data[[grp_var]]), grp_names),
      .catorder = match(as.character(.data[[ana_var]]),  cat_cont_clean)
    )

  # ============================================================
  # Step 3：计算分母（各组总人数）
  # ============================================================
  d_raw <- s0 |> dplyr::filter(!is.na(.grpcd)) |> dplyr::count(.grpcd, name = "D")

  den_n_vec <- stats::setNames(integer(grp_num), seq_len(grp_num))
  for (i in seq_len(grp_num)) {
    v <- d_raw$D[d_raw$.grpcd == i]
    if (length(v) > 0) den_n_vec[i] <- v
  }
  den_n_total <- sum(den_n_vec)

  # ============================================================
  # Step 4：计算分子（各组各分类频数）
  # ============================================================
  n_raw <- s0 |>
    dplyr::filter(!is.na(.catorder), !is.na(.grpcd)) |>
    dplyr::count(.catorder, .grpcd, name = "N")

  n_valid_by_grp <- n_raw |>
    dplyr::group_by(.grpcd) |>
    dplyr::summarise(N_valid = sum(N), .groups = "drop")

  # 合计行（catorder=999）：N = 该组总人数 D
  n_total_row <- d_raw |> dplyr::mutate(.catorder = 999L, N = D) |>
    dplyr::select(.catorder, .grpcd, N)
  n_all       <- dplyr::bind_rows(n_raw, n_total_row)

  # ============================================================
  # Step 5：构造完整骨架并合并频数、分母、百分比
  # ============================================================
  cat_frame <- tibble::tibble(
    .catorder = c(seq_len(cat_num), 999L),
    .catlabel = c(cat_label, "合计")
  )

  skeleton <- tidyr::crossing(
    .catorder = c(seq_len(cat_num), 999L),
    .grpcd    = seq_len(grp_num)
  )

  nd <- skeleton |>
    dplyr::left_join(n_all,          by = c(".catorder", ".grpcd")) |>
    dplyr::left_join(d_raw,          by = ".grpcd") |>
    dplyr::left_join(n_valid_by_grp, by = ".grpcd") |>
    dplyr::mutate(
      N       = tidyr::replace_na(N,       0L),
      D       = tidyr::replace_na(D,       0L),
      N_valid = tidyr::replace_na(N_valid, 0L),
      P  = dplyr::if_else(D > 0, N / D * 100, 0),
      NP = dplyr::if_else(
        .catorder == 999L,
        paste0(D,  "(", D - N_valid, ")"),
        paste0(N,  "(", formatC(P, format = "f", digits = 2), ")")
      )
    )

  # 合计列（全体受试者）
  n_tot_cat     <- s0 |> dplyr::filter(!is.na(.catorder)) |>
    dplyr::count(.catorder, name = "N_tot")
  n_valid_total <- sum(n_tot_cat$N_tot)
  n_total_col   <- dplyr::bind_rows(n_tot_cat,
    tibble::tibble(.catorder = 999L, N_tot = as.integer(den_n_total)))

  nd <- nd |>
    dplyr::left_join(n_total_col, by = ".catorder") |>
    dplyr::mutate(
      N_tot  = tidyr::replace_na(N_tot, 0L),
      P_tot  = ifelse(den_n_total > 0, as.numeric(N_tot) / den_n_total * 100, 0),
      NP_tot = ifelse(
        .catorder == 999L,
        paste0(den_n_total, "(", den_n_total - n_valid_total, ")"),
        paste0(N_tot, "(", formatC(P_tot, format = "f", digits = 2), ")")
      )
    )

  # ============================================================
  # Step 6：CMH 卡方检验
  #
  #   公式完全对应 SAS PROC FREQ CMH 统计量：
  #     Q = A' V^{-1} A
  #   method=1（非零相关，df=1）：整数行/列得分
  #   method=2（行均值得分差，df=c-1）：整数列得分
  #   method=3（一般关联，df=(r-1)(c-1)）：不使用得分
  # ============================================================
  stat_str <- NA_character_
  p_str    <- NA_character_

  if (grp_num > 1) {
    n_cats_obs <- dplyr::n_distinct(s0$.catorder[!is.na(s0$.catorder)])

    if (n_cats_obs <= 1) {
      stat_val <- 0; p_val <- 1
    } else {
      cmh_data <- s0 |>
        dplyr::filter(!is.na(.catorder), !is.na(.grpcd), !is.na(.data[[cmhvar]])) |>
        dplyr::mutate(
          .cmhvar_f   = factor(as.character(.data[[cmhvar]])),
          .grpcd_f    = factor(.grpcd,    levels = seq_len(grp_num)),
          .catorder_f = factor(.catorder, levels = seq_len(cat_num))
        )

      # ── 自定义 CMH 统计量（与 SAS PROC FREQ 完全一致）──────────────────
      .cmh_sas <- function(tbl_3d, method) {
        K <- dim(tbl_3d)[1]; R <- dim(tbl_3d)[2]; C <- dim(tbl_3d)[3]

        A_vec <- numeric((R - 1) * (C - 1))
        V_mat <- matrix(0, (R - 1) * (C - 1), (R - 1) * (C - 1))

        for (k in seq_len(K)) {
          n_k <- tbl_3d[k, , ]; N_k <- sum(n_k)
          if (N_k < 2) next
          r_k <- rowSums(n_k); c_k <- colSums(n_k)
          mu_k <- outer(r_k, c_k) / N_k

          if (method == 3) {
            a_k <- (n_k - mu_k)[seq_len(R-1), seq_len(C-1), drop = FALSE]
            A_vec <- A_vec + as.vector(a_k)
            p_r <- r_k / N_k; p_c <- c_k / N_k
            Vr  <- (diag(p_r) - outer(p_r, p_r))[seq_len(R-1), seq_len(R-1), drop = FALSE]
            Vc  <- (diag(p_c) - outer(p_c, p_c))[seq_len(C-1), seq_len(C-1), drop = FALSE]
            V_mat <- V_mat + N_k^2 / (N_k - 1) * kronecker(Vr, Vc)

          } else if (method == 2) {
            sc <- seq_len(C)
            mean_tot <- sum(c_k * sc) / N_k
            mean_grp <- rowSums(sweep(n_k, 2, sc, "*")) / r_k
            mean_grp[is.nan(mean_grp)] <- 0
            a_k   <- (mean_grp * r_k / N_k - r_k / N_k * mean_tot)[seq_len(R - 1)]
            A_vec <- A_vec + a_k
            sc2_bar <- sum(c_k * sc^2) / N_k; var_c <- sc2_bar - mean_tot^2
            p_r <- r_k / N_k
            Vr  <- (diag(p_r) - outer(p_r, p_r))[seq_len(R-1), seq_len(R-1), drop = FALSE]
            V_mat <- V_mat + N_k^2 / (N_k - 1) * var_c * Vr / N_k

          } else {
            # method=1，非零相关，df=1
            sr <- seq_len(R); sc <- seq_len(C)
            mean_r_tot <- sum(r_k * sr) / N_k; mean_c_tot <- sum(c_k * sc) / N_k
            a_k   <- sum(outer(sr - mean_r_tot, sc - mean_c_tot) * n_k) / N_k
            A_vec <- A_vec + a_k
            var_r <- sum(r_k * (sr - mean_r_tot)^2) / N_k
            var_c <- sum(c_k * (sc - mean_c_tot)^2) / N_k
            V_mat <- V_mat + N_k / (N_k - 1) * var_r * var_c / N_k
          }
        }

        tryCatch({
          Q  <- as.numeric(t(A_vec) %*% solve(V_mat) %*% A_vec)
          df <- if (method == 3) (R-1)*(C-1) else if (method == 2) C-1 else 1L
          list(statistic = Q, p.value = pchisq(Q, df = df, lower.tail = FALSE))
        }, error = function(e) list(statistic = 0, p.value = 1))
      }
      # ────────────────────────────────────────────────────────────────────

      res <- tryCatch({
        tbl_3d <- xtabs(~ .cmhvar_f + .grpcd_f + .catorder_f, data = cmh_data)
        .cmh_sas(tbl_3d, method)
      }, error = function(e) {
        warning(paste("CMH检验失败，已填充默认值。原因：", conditionMessage(e)))
        list(statistic = 0, p.value = 1)
      })

      stat_val <- res$statistic
      p_val    <- res$p.value
    }

    # 格式化统计量和 P 值
    method_lbl <- switch(as.character(method),
      "1" = "CMH(非零相关)",
      "2" = "CMH(行均值得分差值)",
      "3" = "CMH(一般关联)",
      "CMH"
    )
    stat_str <- paste0(sprintf("%.2f", stat_val), "(", method_lbl, ")")
    p_str    <- if (p_val <= 0.0001) "<.0001" else sprintf("%.4f", p_val)
  }

  # ============================================================
  # Step 7：转宽格式，构造输出数据框
  # ============================================================
  wide_np <- nd |>
    dplyr::select(.catorder, .grpcd, NP) |>
    tidyr::pivot_wider(id_cols = .catorder, names_from = .grpcd,
                values_from = NP, names_prefix = "NP_",
                values_fill = "0(0.00)")

  result <- cat_frame |>
    dplyr::left_join(wide_np, by = ".catorder")

  for (i in seq_len(grp_num)) {
    col <- paste0("NP_", i)
    if (!col %in% names(result)) result[[col]] <- "0(0.00)"
  }

  result <- result |>
    dplyr::left_join(nd |> dplyr::select(.catorder, NP_tot) |> dplyr::distinct(), by = ".catorder") |>
    dplyr::mutate(
      .label = dplyr::if_else(
        .catorder == 999L,
        paste0("  ", .catlabel, "(Missing)"),
        paste0("  ", .catlabel, "(%)")
      )
    )

  if (coltotal == 0) result <- result |> dplyr::filter(.catorder != 999L)

  # ── 构造首行（指标标签行，含统计量）──────────────────────────────────
  np_cols   <- paste0("NP_", seq_len(grp_num))
  first_row <- tibble::tibble(.catorder = 0L, .catlabel = ana_label, .label = ana_label)
  for (col in np_cols) first_row[[col]] <- ""
  first_row[["NP_tot"]] <- ""
  if (grp_num > 1) {
    first_row[[".stat"]]  <- if (!is.na(stat_str)) stat_str else ""
    first_row[[".pval"]]  <- if (!is.na(p_str))    p_str    else ""
  }

  if (grp_num > 1) {
    result[[".stat"]] <- ""
    result[[".pval"]] <- ""
  }

  out_raw <- dplyr::bind_rows(first_row, result)

  # ── 确定要保留的列 ────────────────────────────────────────────────────
  keep_cols <- c(".label", np_cols)
  if (rowtotal == 1) keep_cols <- c(keep_cols, "NP_tot")
  if (grp_num > 1 && outp == 1) keep_cols <- c(keep_cols, ".stat", ".pval")

  out_df <- out_raw |> dplyr::select(dplyr::all_of(keep_cols))

  # ============================================================
  # Step 8：累积叠加（私有环境）
  # ============================================================
  if (is.null(.ccmh_env$table_out)) {
    .ccmh_env$table_out <- out_df
  } else {
    .ccmh_env$table_out <- dplyr::bind_rows(.ccmh_env$table_out, out_df)
  }

  # ============================================================
  # Step 9：outyn=0 → 静默返回，等待后续叠加
  # ============================================================
  if (outyn == 0) return(invisible(NULL))

  # ============================================================
  # Step 10：构造 varlist 字符串，委托 report_table() 输出
  # ============================================================
  final_df <- .ccmh_env$table_out

  # 各组列标题
  grp_col_labels <- vapply(seq_len(grp_num), function(i) {
    n_i <- as.integer(den_n_vec[as.character(i)])
    n_i <- if (is.na(n_i)) 0L else n_i
    if (outn == 1) paste0(grp_names[i], "\n(N=", n_i, ")") else grp_names[i]
  }, character(1))

  total_lbl <- if (outn == 1) paste0("合计\n(N=", den_n_total, ")") else "合计"

  # 构造 varlist 字符串
  vl_parts <- c(
    ".label/",                           # 首列：空表头
    paste0(np_cols, "/", grp_col_labels) # 各组列
  )
  if (rowtotal == 1) {
    vl_parts <- c(vl_parts, paste0("NP_tot/", total_lbl))
  }
  if (grp_num > 1 && outp == 1 && ".stat" %in% names(final_df)) {
    vl_parts <- c(vl_parts, ".stat/统计量", ".pval/P值")
  }
  varlist_str <- paste(vl_parts, collapse = "|")

  # 加粗行：各指标的标签首行（.label 不以"  "开头，即未缩进的行）
  bold_idx <- which(
    !startsWith(final_df[[".label"]], "  ") &
    !is.na(final_df[[".label"]]) &
    nchar(trimws(final_df[[".label"]])) > 0
  )

  ft <- report_table(
    data        = final_df,
    varlist     = varlist_str,
    title       = title,
    footnote    = footnote,
    headerjust  = "center",
    col1just    = "left",
    columnjust  = "center",
    autoaddnum  = "yes",
    bold_rows   = if (length(bold_idx) > 0) bold_idx else NULL
  )

  # 输出后清空私有累积数据
  .ccmh_env$table_out <- NULL

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left",
    bold_rows = if (length(bold_idx) > 0) bold_idx else NULL
  )

  return(invisible(ft))
}
