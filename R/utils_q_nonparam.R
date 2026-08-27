# =============================================================================
# utils_q_nonparam.R
# 定量数据非参数/参数统计描述及检验函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_q_nonparam_server 返回的参数（inds, data_cond, denominator_data 等）
#   - 内部构建标准宽表（.label / _np1~N / _np999），与 q_pairt、q_describe 共用格式
#   - 调用 report_table() 统一出表样式（三线表、自动编号等）
#   - 返回 flextable 对象，供 Shiny 结果面板显示和报告导出
#
# 累积缓存：与 q_pairt、q_describe 共用全局变量 .table_out1
#   outyn=0 时累积，outyn=1 时统一出表并清空
# =============================================================================

#' 定量数据非参数/参数统计描述及检验
#'
#' @param inds            数据框对象（分析数据）
#' @param data_cond       数据筛选条件（R 表达式字符串）
#' @param denominator_data 数据框对象（分母数据，用于表头 N=XX）
#' @param group           分组变量及组别名称，格式："分组变量名|组别1/组别2/..."
#' @param varlist         分析变量及标签，格式："变量名|变量标签"
#' @param test_in         组内检验方法；NULL（默认）不进行；1：配对t检验；2：Wilcoxon符号秩检验
#' @param test_between    组间检验方法；NULL（默认）不进行；1：参数检验；2：非参数检验
#' @param outp            是否输出统计量和P值列；1（默认）输出；0 不输出
#' @param outn            是否输出 N=XX 标签；1（默认）输出；0 不输出
#' @param rowtotal        是否输出合计列；0（默认）不输出；1 输出
#' @param topleftlabel    表格左上角指标列标签，默认 ""
#' @param title           表格标题，默认 NULL
#' @param footnote        表格底注，默认 NULL
#' @param outyn           叠加输出控制；0：仅累积；1（默认）：出表并清空缓存
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
q_nonparam <- function(inds,
                       data_cond,
                       denominator_data,
                       group,
                       varlist,
                       test_in       = NULL,
                       test_between  = NULL,
                       outp          = 1,
                       outn          = 1,
                       rowtotal      = 0,
                       topleftlabel  = "",
                       title         = NULL,
                       footnote      = NULL,
                       oneline       = NULL,
                       outyn         = 1) {

  # ============================================================
  # 内部工具：格式化 P 值（对应 SAS PVALUE6.4）
  # ============================================================
  .fmt_p <- function(p) {
    if (is.na(p)) return("")
    if (p > 0 && p <= 0.0001) return("<0.0001")
    formatC(p, format = "f", digits = 4)
  }

  # ============================================================
  # 1. 解析 group 参数
  # ============================================================
  grp_parts <- strsplit(group, "|", fixed = TRUE)[[1]]
  grp_var   <- trimws(grp_parts[1])
  grp_names <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grp_names <- grp_names[grp_names != ""]
  grp_num   <- length(grp_names)

  # ============================================================
  # 2. 解析 varlist 参数
  # ============================================================
  var_parts <- strsplit(varlist, "|", fixed = TRUE)[[1]]
  ana_var   <- trimws(var_parts[1])
  ana_label <- trimws(var_parts[2])

  # ============================================================
  # 3. 处理分母数据（用于 N=XX 表头）
  # ============================================================
  den_data <- denominator_data
  den_data <- den_data |>
    dplyr::mutate(.grpcd = match(as.character(.data[[grp_var]]), grp_names))

  den_n_vec   <- vapply(seq_len(grp_num),
                        function(i) sum(den_data$.grpcd == i, na.rm = TRUE),
                        integer(1))
  den_n_total <- sum(den_n_vec)

  # ============================================================
  # 4. 处理分析数据
  # ============================================================
  s0 <- inds
  if (!is.null(data_cond) && nchar(trimws(data_cond)) > 0) {
    s0 <- s0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  }
  s0 <- s0 |>
    dplyr::mutate(.grpcd = match(as.character(.data[[grp_var]]), grp_names))

  # ============================================================
  # 5. 描述性统计
  # ============================================================
  .desc_stats <- function(x) {
    fmt2 <- function(v) formatC(v, format = "f", digits = 2)
    c(
      paste0(sum(!is.na(x)), "(", sum(is.na(x)), ")"),
      paste0(fmt2(mean(x, na.rm = TRUE)), "(", fmt2(stats::sd(x, na.rm = TRUE)), ")"),
      paste0(fmt2(stats::median(x, na.rm = TRUE)), "(",
             fmt2(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE, type = 2)), ",",
             fmt2(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE, type = 2)), ")"),
      paste0(fmt2(min(x, na.rm = TRUE)), ",", fmt2(max(x, na.rm = TRUE)))
    )
  }

  row_labels <- c("  N(Missing)", "  Mean(SD)", "  Median(Q1,Q3)", "  Min,Max")

  grp_stats <- lapply(seq_len(grp_num), function(i) {
    x <- suppressWarnings(as.numeric(s0[[ana_var]][!is.na(s0$.grpcd) & s0$.grpcd == i]))
    if (length(x[!is.na(x)]) == 0) rep("", 4) else .desc_stats(x)
  })

  x_all       <- suppressWarnings(as.numeric(s0[[ana_var]][!is.na(s0$.grpcd)]))
  total_stats <- if (length(x_all[!is.na(x_all)]) == 0) rep("", 4) else .desc_stats(x_all)

  # ============================================================
  # 6. 组装宽表（列名体系与 q_pairt 完全一致：_np1/_np2/.../_np999）
  # ============================================================
  stats_df <- data.frame(.label = row_labels, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) stats_df[[paste0("_np", i)]] <- grp_stats[[i]]
  stats_df[["_np999"]] <- total_stats

  # ============================================================
  # 7. 【组内检验】对每组单独执行（H0: 均值/中位数 = 0）
  #
  #   对应 SAS：%if &test_in. ^= %then %do;（不为空则进入）
  #     proc univariate TestsForLocation 输出所有统计量，
  #     再用 where Testlab= 筛选对应行：
  #       test_in=1 -> where Testlab="t"  -> 单样本t检验
  #       test_in=2 -> where Testlab="S"  -> Wilcoxon符号秩检验
  #       其他值    -> where 不匹配 -> _stat1 为空 -> 静默跳过（+warning提示）
  #
  #   输出格式：统计量(P值)
  #   对应 SAS：_np=put(stat,8.2)||"("||put(pValue,PVALUE6.4)||")"
  # ============================================================
  if (!is.null(test_in)) {

    # 确定检验方法和行标签（对应 SAS _stat3 中 _label_ 赋值）
    if (test_in == 1) {
      intest_label <- "  配对t检验(P值)"
      .run_test_in <- function(x) stats::t.test(x, mu = 0)
    } else if (test_in == 2) {
      intest_label <- "  符号秩(P值)"
      # exact=TRUE：与SAS proc univariate 精确分布一致
      # 统计量：SAS输出的S = 正秩和 - n*(n+1)/4（centered），
      #         R wilcox.test返回V = 正秩和（未centered），
      #         需手动转换：S = V - n*(n+1)/4
      .run_test_in <- function(x) stats::wilcox.test(x, mu = 0, exact = TRUE)
    } else {
      # 对应 SAS where 条件不匹配 -> 静默跳过，但给出 warning 提示
      warning(sprintf(
        "q_nonparam: test_in=%s 不是有效值（1=配对t检验，2=符号秩检验），已跳过组内检验。",
        test_in
      ))
      .run_test_in <- NULL
    }

    if (!is.null(.run_test_in)) {
      intest_row <- data.frame(.label = intest_label, stringsAsFactors = FALSE)

      for (i in seq_len(grp_num)) {
        x   <- suppressWarnings(as.numeric(s0[[ana_var]][!is.na(s0$.grpcd) & s0$.grpcd == i]))
        x   <- x[!is.na(x)]
        col <- paste0("_np", i)
        intest_row[[col]] <- tryCatch({
          if (length(x) < 2) ".(.)  "
          else if (test_in == 2) {
            # 符号秩：剔除0值后 exact=TRUE，同时对齐统计量S和精确P值
            # 与SAS proc univariate 行为一致：剔除0值，精确分布
            x_nz <- x[x != 0]
            n_nz <- length(x_nz)
            if (n_nz < 1) ".(.)  "
            else {
              has_ties <- length(unique(abs(x_nz))) < length(x_nz)
              res <- stats::wilcox.test(x_nz, mu = 0,
                                        exact   = !has_ties,  # 有ties时自动退回正态近似
                                        correct = has_ties)   # 有ties时加连续性校正，与SAS一致
              # centered S = V - n_nz*(n_nz+1)/4，对齐SAS Testlab="S"
              stat <- res$statistic - n_nz * (n_nz + 1) / 4
              paste0(formatC(stat, format = "f", digits = 2),
                     "(", .fmt_p(res$p.value), ")")
            }
          } else {
            res  <- .run_test_in(x)
            paste0(formatC(res$statistic, format = "f", digits = 2),
                   "(", .fmt_p(res$p.value), ")")
          }
        }, error = function(e) ".(.)  ")
      }

      # 合计列
      x_tot <- x_all[!is.na(x_all)]
      intest_row[["_np999"]] <- tryCatch({
        if (length(x_tot) < 2) ".(.)  "
        else if (test_in == 2) {
          x_nz <- x_tot[x_tot != 0]
          n_nz <- length(x_nz)
          if (n_nz < 1) ".(.)  "
          else {
            has_ties <- length(unique(abs(x_nz))) < length(x_nz)
            res <- stats::wilcox.test(x_nz, mu = 0,
                                      exact   = !has_ties,  # 有ties时自动退回正态近似
                                      correct = has_ties)   # 有ties时加连续性校正，与SAS一致
            stat <- res$statistic - n_nz * (n_nz + 1) / 4
            paste0(formatC(stat, format = "f", digits = 2),
                   "(", .fmt_p(res$p.value), ")")
          }
        } else {
          res  <- .run_test_in(x_tot)
          paste0(formatC(res$statistic, format = "f", digits = 2),
                 "(", .fmt_p(res$p.value), ")")
        }
      }, error = function(e) ".(.)  ")

      stats_df <- dplyr::bind_rows(stats_df, intest_row)
    }
  }

  # ============================================================
  # 8. 【组间检验】计算统计量和 P 值（展示在变量标签首行右侧）
  # ============================================================
  stat_val_str <- ""
  p_val_str    <- ""

  if (!is.null(test_between) && grp_num > 1) {
    x_grp <- s0 |>
      dplyr::filter(!is.na(.grpcd)) |>
      dplyr::mutate(.y = suppressWarnings(as.numeric(.data[[ana_var]]))) |>
      dplyr::filter(!is.na(.y))

    tryCatch({
      if (test_between == 1) {
        # 参数检验：2组独立t检验 / >=3组单因素方差分析
        if (grp_num == 2) {
          g1 <- x_grp |> dplyr::filter(.grpcd == 1) |> dplyr::pull(.y)
          g2 <- x_grp |> dplyr::filter(.grpcd == 2) |> dplyr::pull(.y)
          res <- stats::t.test(g1, g2, var.equal = TRUE)
          stat_val_str <- paste0(formatC(res$statistic, format = "f", digits = 2), "(独立样本t检验)")
          p_val_str    <- .fmt_p(res$p.value)
        } else {
          res <- stats::aov(as.formula(".y ~ factor(.grpcd)"), data = x_grp)
          sm  <- summary(res)[[1]]
          stat_val_str <- paste0(formatC(sm[["F value"]][1], format = "f", digits = 2), "(方差分析)")
          p_val_str    <- .fmt_p(sm[["Pr(>F)"]][1])
        }
      } else if (test_between == 2) {
        # 非参数检验：2组Wilcoxon秩和 / >=3组Kruskal-Wallis H检验
        if (grp_num == 2) {
          g1 <- x_grp |> dplyr::filter(.grpcd == 1) |> dplyr::pull(.y)
          g2 <- x_grp |> dplyr::filter(.grpcd == 2) |> dplyr::pull(.y)
          # exact=FALSE + correct=FALSE：与SAS proc npar1way默认一致（正态近似，无连续性校正）
          # 统计量对齐SAS Z_WIL：从wilcox.test的W推算Z值
          # Z = (W - n1*n2/2) / sqrt(n1*n2*(n1+n2+1)/12)
          res  <- stats::wilcox.test(g1, g2, exact = FALSE, correct = FALSE)
          n1   <- length(g1); n2 <- length(g2)
          z_wil <- (res$statistic - n1 * n2 / 2) /
                    sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
          stat_val_str <- paste0(formatC(z_wil, format = "f", digits = 2), "(Wilcoxon秩和检验)")
          p_val_str    <- .fmt_p(res$p.value)
        } else {
          res <- stats::kruskal.test(.y ~ factor(.grpcd), data = x_grp)
          stat_val_str <- paste0(formatC(res$statistic, format = "f", digits = 2), "(Kruskal-Wallis H检验)")
          p_val_str    <- .fmt_p(res$p.value)
        }
      } else {
        # 其他值：静默跳过，给出 warning 提示
        warning(sprintf(
          "q_nonparam: test_between=%s 不是有效值（1=参数检验，2=非参数检验），已跳过组间检验。",
          test_between
        ))
      }
    }, error = function(e) {
      warning(paste("组间检验失败：", conditionMessage(e)))
      stat_val_str <<- "—"
      p_val_str    <<- "—"
    })
  }

  # ============================================================
  # 9. 拼首行（变量标签行，含组间检验结果）并合并
  #    对应 SAS _firstrow + set _table_out
  # ============================================================
  first_row <- data.frame(.label = ana_label, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) first_row[[paste0("_np", i)]] <- ""
  first_row[["_np999"]] <- ""

  if (!is.null(test_between) && grp_num > 1 && outp == 1) {
    first_row[["_stat"]]   <- stat_val_str
    first_row[["_pvalue"]] <- p_val_str
    stats_df[["_stat"]]   <- ""
    stats_df[["_pvalue"]] <- ""
  }

  result_df <- dplyr::bind_rows(first_row, stats_df)

  # 单组别：合并 _np1 至 _np999（与 q_pairt 逻辑一致）
  if (grp_num == 1) {
    result_df[["_np999"]] <- result_df[["_np1"]]
    result_df[["_np1"]]   <- NULL
  }

  # ============================================================
  # 10. 追加至全局缓存（与 q_pairt 共享 .table_out1）
  # ============================================================
  if (!exists(".table_out1", envir = globalenv())) {
    assign(".table_out1", NULL, envir = globalenv())
  }
  .GlobalEnv$.table_out1 <- dplyr::bind_rows(.GlobalEnv$.table_out1, result_df)

  # ============================================================
  # 11. outyn=0：不出表，直接返回
  # ============================================================
  if (outyn != 1) return(invisible(NULL))

  # ============================================================
  # 12. outyn=1：拼 varlist 字符串，调用 report_table() 出表
  #     对应 SAS：%report_table(data=_table_out, varlist=...)
  # ============================================================
  out_df <- .GlobalEnv$.table_out1
  .GlobalEnv$.table_out1 <- NULL

  # 确保所有目标列存在（多次叠加时某次可能缺列）
  need_cols <- c(
    if (grp_num > 1) paste0("_np", seq_len(grp_num)),
    if (rowtotal == 1 || grp_num == 1) "_np999",
    if (!is.null(test_between) && grp_num > 1 && outp == 1) c("_stat", "_pvalue")
  )
  for (col in need_cols) {
    if (!col %in% names(out_df)) out_df[[col]] <- ""
  }

  # ---- 构建 varlist 字符串 ----
  varlist_str <- paste0(".label/", topleftlabel)

  if (grp_num > 1) {
    for (i in seq_len(grp_num)) {
      lbl <- grp_names[i]
      if (outn == 1) lbl <- paste0(lbl, "$(N=", den_n_vec[i], ")")
      varlist_str <- paste0(varlist_str, "|_np", i, "/", lbl)
    }
  }

  if (grp_num == 1 || rowtotal == 1) {
    lbl <- "合计"
    if (outn == 1) lbl <- paste0(lbl, "$(N=", den_n_total, ")")
    varlist_str <- paste0(varlist_str, "|_np999/", lbl)
  }

  if (!is.null(test_between) && grp_num > 1 && outp == 1) {
    varlist_str <- paste0(varlist_str, "|_stat/统计量|_pvalue/P值")
  }

  # ---- 调用 report_table() ----
  ft <- report_table(
    data         = out_df,
    varlist      = varlist_str,
    title        = title,
    footnote     = footnote,
    headerjust   = "center",
    columnjust   = "center",
    col1just     = "left",
    oneline      = oneline,
    autoaddnum   = "yes",
    bold_rows    = 1L
  )

  # 附着原始参数，供导出管线 ft_to_report_table() 使用
  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left", bold_rows = 1L
  )

  ft
}
