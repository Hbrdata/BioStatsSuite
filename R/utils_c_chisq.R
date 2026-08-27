# =============================================================================
# utils_c_chisq.R
# 卡方检验及定性数据分类描述函数（R 包内部版本）
#
# 计算分类变量的统计量（结果变量为无序变量）：
#   表格左方（组内）为：符号秩；
#   表格右方（组间）为：卡方 / Fisher / McNemar / Bowker。
#
# 累积逻辑：每次调用将本次计算结果追加到全局累积表 .c_chisq_accum。
#   outyn=0：只累加，不输出表格；
#   outyn=1：将累积表一次性传给 report_table() 输出，然后清空累积表。
# =============================================================================

#' 卡方检验及分类变量描述
#'
#' @param inds         数据框对象
#' @param data_cond    数据筛选条件（R 表达式字符串）
#' @param group        分组描述："分组变量名|组名1/组名2/..."
#' @param varlist      分析变量描述："变量名|变量标签|分类值1=分类标签1/分类值2=分类标签2/..."
#' @param coltotal     是否输出"合计"行（1=是，0=否）
#' @param rowtotal     是否输出"合计"列（0=否，1=是）
#' @param outn         是否在表头显示 N=XX（1=是，0=否）
#' @param outp         是否输出统计量和P值列（1=是，0=否）
#' @param title        表格标题
#' @param footnote     表格脚注
#' @param test_between 组间比较：1=卡方；2=Fisher；3=McNemar/Bowker；4=自动；NULL=不做
#' @param test_in      组内比较：1=符号秩；NULL=不做
#' @param outyn        0=只累加不输出；1=输出累积表并清空
#'
#' @return outyn=0 时 invisibly 返回本次数据框；outyn=1 时返回 flextable
#' @noRd
c_chisq <- function(inds, data_cond, group, varlist,
                    coltotal = 1, rowtotal = 0, outn = 1, outp = 1,
                    title = NULL, footnote = NULL,
                    test_between = NULL, test_in = NULL, outyn = 0) {

  # ============================================================
  # 内部辅助函数
  # ============================================================

  as_flag <- function(x, default = 1L) {
    if (is.null(x) || length(x) == 0)                              return(default)
    if (length(x) == 1 && is.na(x))                                return(default)
    if (is.character(x) && length(x) == 1 && !nzchar(trimws(x)))  return(default)
    if (is.logical(x)) return(if (isTRUE(x[1])) 1L else 0L)
    v <- suppressWarnings(as.integer(x[1]))
    if (is.na(v)) return(default)
    if (v != 0L) 1L else 0L
  }

  as_int_or_null <- function(x) {
    if (is.null(x) || length(x) == 0)                              return(NULL)
    if (is.character(x) && length(x) == 1 && !nzchar(trimws(x)))  return(NULL)
    if (length(x) == 1 && is.na(x))                                return(NULL)
    v <- suppressWarnings(as.integer(x[1]))
    if (is.na(v)) NULL else v
  }

  # P 值格式化
  fmt_p <- function(p) {
    if (is.null(p) || is.na(p)) return("NA")
    if (p < 0.0001) return("<0.0001")
    formatC(p, format = "f", digits = 4)
  }

  # 去除首尾引号
  strip_q <- function(x) gsub("^[\'\"`]|[\'\"`]$", "", trimws(x))

  # -- 参数标准化 --
  coltotal     <- as_flag(coltotal,  1L)
  rowtotal     <- as_flag(rowtotal,  0L)
  outn         <- as_flag(outn,      1L)
  outp         <- as_flag(outp,      1L)
  outyn        <- as_flag(outyn,     0L)
  test_between <- as_int_or_null(test_between)
  test_in      <- as_int_or_null(test_in)

  # ============================================================
  # 1. 解析 group
  # ============================================================
  group_parts <- strsplit(group, "|", fixed = TRUE)[[1]]
  grp_var     <- trimws(group_parts[1])
  grp_names   <- trimws(strsplit(group_parts[2], "/", fixed = TRUE)[[1]])
  grp_names   <- grp_names[nchar(grp_names) > 0]
  grp_num     <- length(grp_names)

  # ============================================================
  # 2. 解析 varlist
  # ============================================================
  varlist_parts <- strsplit(varlist, "|", fixed = TRUE)[[1]]
  ana_var       <- trimws(varlist_parts[1])
  ana_label     <- trimws(varlist_parts[2])
  cat_list_str  <- if (length(varlist_parts) >= 3) trimws(varlist_parts[3]) else ""

  cat_defs <- strsplit(cat_list_str, "/", fixed = TRUE)[[1]]
  cat_defs <- cat_defs[nchar(cat_defs) > 0]
  cat_num  <- length(cat_defs)

  cat_cont  <- character(cat_num)
  cat_label <- character(cat_num)
  for (i in seq_len(cat_num)) {
    parts        <- strsplit(cat_defs[i], "=", fixed = TRUE)[[1]]
    cat_cont[i]  <- strip_q(trimws(parts[1]))
    cat_label[i] <- if (length(parts) >= 2) trimws(parts[2]) else cat_cont[i]
  }

  # ============================================================
  # 3. 筛选数据集
  # ============================================================
  s0 <- inds
  if (!is.null(data_cond) && nzchar(trimws(data_cond))) {
    s0 <- s0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  }

  # ============================================================
  # 4. 生成组别序号 .grpcd
  # ============================================================
  s0 <- s0 |>
    dplyr::mutate(.grpcd = match(as.character(.data[[grp_var]]), grp_names))

  # ============================================================
  # 5. 生成分类序号 .catorder
  # ============================================================
  s0 <- s0 |>
    dplyr::mutate(.catorder = match(strip_q(as.character(.data[[ana_var]])), cat_cont))

  # ============================================================
  # 6. 计算频数矩阵
  # ============================================================
  freq_mat   <- matrix(0L, nrow = cat_num, ncol = grp_num)
  valid_mask <- !is.na(s0$.catorder) & !is.na(s0$.grpcd)
  cat_v      <- s0$.catorder[valid_mask]
  grp_v      <- s0$.grpcd[valid_mask]

  for (i in seq_len(cat_num)) {
    for (j in seq_len(grp_num)) {
      freq_mat[i, j] <- sum(cat_v == i & grp_v == j)
    }
  }

  row_totals <- rowSums(freq_mat)
  col_totals <- colSums(freq_mat)
  grand_n    <- sum(freq_mat)

  # ============================================================
  # 7. 各组分母（各组全部进入分析集的人数）
  # ============================================================
  d_vec <- integer(grp_num + 1)
  for (i in seq_len(grp_num)) {
    d_vec[i] <- sum(s0$.grpcd == i, na.rm = TRUE)
  }
  d_vec[grp_num + 1] <- sum(!is.na(s0$.grpcd))

  # ============================================================
  # 8. 组间检验（test_between）
  # ============================================================
  stat_str <- NA_character_
  p_str    <- NA_character_

  if (!is.null(test_between) && grp_num > 1) {

    if (sum(row_totals > 0) <= 1) {
      stat_str <- "NA"
      p_str    <- "NA"
    } else {
      ct <- t(freq_mat)

      tryCatch({

        if (test_between == 1L) {
          r        <- suppressWarnings(stats::chisq.test(ct, correct = FALSE))
          stat_str <- paste0(formatC(as.numeric(r$statistic), format = "f", digits = 2),
                             "(卡方检验)")
          p_str    <- fmt_p(r$p.value)

        } else if (test_between == 2L) {
          sim      <- prod(dim(ct)) > 4
          r        <- suppressWarnings(stats::fisher.test(ct, simulate.p.value = sim, B = 2000))
          stat_str <- "(Fisher精确检验)"
          p_str    <- fmt_p(r$p.value)

        } else if (test_between == 3L) {
          if (grp_num == 2 && cat_num == 2) {
            sq       <- matrix(c(freq_mat[1,1], freq_mat[2,1],
                                 freq_mat[1,2], freq_mat[2,2]), nrow = 2)
            r        <- suppressWarnings(stats::mcnemar.test(sq, correct = FALSE))
            stat_str <- paste0(formatC(as.numeric(r$statistic), format = "f", digits = 2),
                               "(McNemar检验)")
            p_str    <- fmt_p(r$p.value)
          } else if (grp_num == cat_num && grp_num > 2) {
            k        <- min(grp_num, cat_num)
            sq       <- t(freq_mat[seq_len(k), seq_len(k)])
            r        <- suppressWarnings(stats::mcnemar.test(sq, correct = FALSE))
            stat_str <- paste0(formatC(as.numeric(r$statistic), format = "f", digits = 2),
                               "(Bowker检验)")
            p_str    <- fmt_p(r$p.value)
          } else {
            stat_str <- "NA"; p_str <- "NA"
          }

        } else if (test_between == 4L) {
          exp_tbl    <- suppressWarnings(stats::chisq.test(ct, correct = FALSE)$expected)
          n_lt5      <- sum(exp_tbl < 5, na.rm = TRUE)
          n_cells    <- length(exp_tbl)
          n_sample   <- sum(ct)
          use_fisher <- (n_cells > 0 && n_lt5 / n_cells > 0.2) || (n_sample < 40)

          if (use_fisher) {
            sim      <- prod(dim(ct)) > 4
            r        <- suppressWarnings(stats::fisher.test(ct, simulate.p.value = sim, B = 2000))
            stat_str <- "(Fisher精确检验)"
            p_str    <- fmt_p(r$p.value)
          } else {
            r        <- suppressWarnings(stats::chisq.test(ct, correct = FALSE))
            stat_str <- paste0(formatC(as.numeric(r$statistic), format = "f", digits = 2),
                               "(卡方检验)")
            p_str    <- fmt_p(r$p.value)
          }
        }

      }, error = function(e) {
        warning("组间检验失败，已填充NA。原因：", conditionMessage(e))
        stat_str <<- "NA"
        p_str    <<- "NA"
      })
    }
  }

  # ============================================================
  # 9. 组内检验（test_in=1 -> 符号秩检验）
  # ============================================================
  wilcox_np <- NULL
  if (!is.null(test_in) && test_in == 1L) {
    np_sign <- character(grp_num + 1)
    for (i in seq_len(grp_num)) {
      grp_data <- s0$.catorder[s0$.grpcd == i & !is.na(s0$.grpcd) & !is.na(s0$.catorder)]
      if (length(grp_data) > 0 && length(unique(grp_data)) > 1) {
        r_w        <- suppressWarnings(stats::wilcox.test(grp_data, mu = 0, exact = FALSE))
        np_sign[i] <- paste0(formatC(as.numeric(r_w$statistic), format = "f", digits = 2),
                             "(", fmt_p(r_w$p.value), ")")
      } else {
        np_sign[i] <- "NA"
      }
    }
    np_sign[grp_num + 1] <- ""
    wilcox_np <- np_sign
  }

  # ============================================================
  # 10. 构造本次变量的数据行
  # ============================================================
  np_cols <- paste0("NP_", seq_len(grp_num))

  fmt_np <- function(n, d) {
    if (d == 0) return(paste0(n, "(0.00)"))
    paste0(n, "(", formatC(n / d * 100, format = "f", digits = 2), ")")
  }
  fmt_np_total <- function(valid_n, d) paste0(d, "(", d - valid_n, ")")

  make_row <- function(label, np_vec, np_tot_val, stat_val = "", pvalue_val = "") {
    row <- data.frame(
      label_col = label,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(np_cols)) row[[np_cols[i]]] <- np_vec[i]
    row[["NP_tot"]]  <- np_tot_val
    row[["stat_"]]   <- stat_val
    row[["pvalue_"]] <- pvalue_val
    names(row)[1]    <- ".label"
    row
  }

  rows_list <- list()

  # 变量标签行
  rows_list[[1]] <- make_row(
    label      = ana_label,
    np_vec     = rep("", grp_num),
    np_tot_val = "",
    stat_val   = if (!is.na(stat_str)) stat_str else "",
    pvalue_val = if (!is.na(p_str))    p_str    else ""
  )

  # 各分类行
  for (i in seq_len(cat_num)) {
    np_i <- vapply(seq_len(grp_num),
                   function(j) fmt_np(freq_mat[i, j], d_vec[j]),
                   character(1))
    rows_list[[length(rows_list) + 1]] <- make_row(
      label      = paste0("  ", cat_label[i], "(%)"),
      np_vec     = np_i,
      np_tot_val = fmt_np(row_totals[i], d_vec[grp_num + 1])
    )
  }

  # 合计行
  if (coltotal == 1) {
    np_tot_row <- vapply(seq_len(grp_num),
                         function(j) fmt_np_total(col_totals[j], d_vec[j]),
                         character(1))
    rows_list[[length(rows_list) + 1]] <- make_row(
      label      = "  合计(Missing)",
      np_vec     = np_tot_row,
      np_tot_val = fmt_np_total(grand_n, d_vec[grp_num + 1])
    )
  }

  # 符号秩行
  if (!is.null(wilcox_np)) {
    rows_list[[length(rows_list) + 1]] <- make_row(
      label      = "  符号秩(P值)",
      np_vec     = wilcox_np[seq_len(grp_num)],
      np_tot_val = wilcox_np[grp_num + 1]
    )
  }

  c1 <- dplyr::bind_rows(rows_list)

  # ============================================================
  # 11. 累加到全局 .c_chisq_accum
  # ============================================================
  if (!exists(".c_chisq_accum", envir = .GlobalEnv) ||
      !is.data.frame(get(".c_chisq_accum", envir = .GlobalEnv))) {
    assign(".c_chisq_accum", c1[0, ], envir = .GlobalEnv)
  }

  accum <- get(".c_chisq_accum", envir = .GlobalEnv)
  assign(".c_chisq_accum", dplyr::bind_rows(accum, c1), envir = .GlobalEnv)

  # ============================================================
  # 12. outyn=0：只累加，不输出
  # ============================================================
  if (outyn == 0) {
    return(invisible(c1))
  }

  # ============================================================
  # 13. outyn=1：取出累积表，调用 report_table() 输出，然后清空
  # ============================================================
  table_out <- get(".c_chisq_accum", envir = .GlobalEnv)

  data_cols <- np_cols
  if (rowtotal == 1)            data_cols <- c(data_cols, "NP_tot")
  if (grp_num > 1 && outp == 1) data_cols <- c(data_cols, "stat_", "pvalue_")

  table_out <- table_out |> dplyr::select(dplyr::all_of(c(".label", data_cols)))

  # 表头标签
  make_hdr <- function(lbl, n) {
    if (outn == 1) paste0(lbl, "$(N=", n, ")") else lbl
  }
  grp_hdrs  <- mapply(make_hdr, grp_names, d_vec[seq_len(grp_num)], SIMPLIFY = TRUE)
  total_hdr <- make_hdr("合计", d_vec[grp_num + 1])

  # 拼装 report_table varlist 字符串
  vl_parts <- ".label/指标"
  for (i in seq_len(grp_num)) {
    vl_parts <- paste0(vl_parts, "|", np_cols[i], "/", grp_hdrs[i])
  }
  if (rowtotal == 1) {
    vl_parts <- paste0(vl_parts, "|NP_tot/", total_hdr)
  }
  if (grp_num > 1 && outp == 1) {
    vl_parts <- paste0(vl_parts, "|stat_/统计量|pvalue_/P值")
  }

  # 找出变量标签行行号（NP_1 列为空字符串）
  bold_row_idx <- which(table_out[[np_cols[1]]] == "")

  # 清空累积表
  assign(".c_chisq_accum", NULL, envir = .GlobalEnv)

  # 调用 report_table() 完成渲染
  report_table(
    data        = table_out,
    varlist     = vl_parts,
    title       = title,
    footnote    = footnote,
    bold_rows   = bold_row_idx,
    outyn       = 1
  )
}
