# =============================================================================
# utils_q_pairt.R
# 定量数据描述性统计及配对t检验函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_q_pairt_server 返回的参数（inds, data_cond, denominator_data 等）
#   - 内部构建标准宽表（.label / _np1~N / _np999），与 q_nonparam、q_describe 共用格式
#   - 调用 report_table() 统一出表样式（三线表、自动编号等）
#   - 返回 flextable 对象，供 Shiny 结果面板显示和报告导出
#
# 累积缓存：与 q_nonparam、q_describe 共用全局变量 .table_out1
#   outyn=0 时累积，outyn=1 时统一出表并清空
# =============================================================================

#' 定量数据描述性统计及配对t检验
#'
#' @param inds            数据框对象（分析数据）
#' @param data_cond       数据筛选条件（R 表达式字符串）
#' @param denominator_data 数据框对象（分母数据，用于表头 N=XX）
#' @param group           分组变量及组别名称，格式："分组变量名|组别1/组别2/..."
#' @param varlist         分析变量及标签，格式："变量名|变量标签"
#' @param title           表格标题，默认 NULL
#' @param footnote        表格底注，默认 NULL
#' @param outn            是否输出 N=XX 标签；1（默认）：输出；0：不输出
#' @param topleftlabel    表格左上角指标列标签，默认 ""
#' @param rowtotal        是否输出合计列；1：输出；0（默认）：不输出
#' @param pairt           是否进行配对t检验；1：进行；NULL 或 0：不进行
#' @param oneline         非 NULL 时所有列等宽，传给 report_table()；默认 NULL
#' @param outyn           叠加输出控制；0：仅累积；1（默认）：出表并清空缓存
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
q_pairt <- function(inds,
                    data_cond,
                    denominator_data,
                    group,
                    varlist,
                    title        = NULL,
                    footnote     = NULL,
                    outn         = 1,
                    topleftlabel = "",
                    rowtotal     = 0,
                    pairt        = NULL,
                    oneline      = NULL,
                    outyn        = 1) {

  # ============================================================
  # 内部辅助：P 值格式化（对应 SAS PVALUE6.4）
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
  # 5. 描述性统计计算（对应 SAS proc report + data 步格式化）
  # ============================================================
  .desc_stats <- function(x) {
    fmt2 <- function(v) formatC(v, format = "f", digits = 2)
    c(
      paste0(sum(!is.na(x)), "(", sum(is.na(x)), ")"),
      paste0(fmt2(mean(x, na.rm = TRUE)),   "(", fmt2(stats::sd(x, na.rm = TRUE)), ")"),
      paste0(fmt2(stats::median(x, na.rm = TRUE)), "(",
             fmt2(stats::quantile(x, 0.25, na.rm = TRUE, type = 2)), ",",
             fmt2(stats::quantile(x, 0.75, na.rm = TRUE, type = 2)), ")"),
      paste0(fmt2(min(x, na.rm = TRUE)), ",", fmt2(max(x, na.rm = TRUE)))
    )
  }

  row_labels <- c("  N(Missing)", "  Mean(SD)", "  Median(Q1,Q3)", "  Min,Max")

  grp_stats <- lapply(seq_len(grp_num), function(i) {
    x <- suppressWarnings(as.numeric(
      s0[[ana_var]][!is.na(s0$.grpcd) & s0$.grpcd == i]
    ))
    if (length(x[!is.na(x)]) == 0) rep("", 4) else .desc_stats(x)
  })

  x_all       <- suppressWarnings(as.numeric(s0[[ana_var]][!is.na(s0$.grpcd)]))
  total_stats <- if (length(x_all[!is.na(x_all)]) == 0) rep("", 4) else .desc_stats(x_all)

  # ============================================================
  # 6. 组装宽表（对应 SAS proc transpose 后结构）
  # ============================================================
  stats_df <- data.frame(.label = row_labels, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) {
    stats_df[[paste0("_np", i)]] <- grp_stats[[i]]
  }
  stats_df[["_np999"]] <- total_stats

  # ============================================================
  # 7. 配对 t 检验（对应 SAS proc univariate TestsForLocation）
  # ============================================================
  if (!is.null(pairt) && pairt == 1) {
    t_row <- data.frame(.label = "  配对t检验(P值)", stringsAsFactors = FALSE)

    for (i in seq_len(grp_num)) {
      x <- suppressWarnings(as.numeric(
        s0[[ana_var]][!is.na(s0$.grpcd) & s0$.grpcd == i]
      ))
      x <- x[!is.na(x)]
      t_row[[paste0("_np", i)]] <- tryCatch({
        if (length(x) < 2) ".(.)  "
        else {
          t_res <- stats::t.test(x, mu = 0)
          paste0(formatC(t_res$statistic, format = "f", digits = 2),
                 "(", .fmt_p(t_res$p.value), ")")
        }
      }, error = function(e) ".(.)  ")
    }

    x_tot <- x_all[!is.na(x_all)]
    t_row[["_np999"]] <- tryCatch({
      if (length(x_tot) < 2) ".(.)  "
      else {
        t_res <- stats::t.test(x_tot, mu = 0)
        paste0(formatC(t_res$statistic, format = "f", digits = 2),
               "(", .fmt_p(t_res$p.value), ")")
      }
    }, error = function(e) ".(.)  ")

    stats_df <- dplyr::bind_rows(stats_df, t_row)
  }

  # ============================================================
  # 8. 拼首行（变量标签行）并合并（对应 SAS _firstrow + set）
  # ============================================================
  first_row <- data.frame(.label = ana_label, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) first_row[[paste0("_np", i)]] <- ""
  first_row[["_np999"]] <- ""

  result_df <- dplyr::bind_rows(first_row, stats_df)

  # 单组别：_np1 -> _np999（对应 SAS 单组别处理逻辑）
  if (grp_num == 1) {
    result_df[["_np999"]] <- result_df[["_np1"]]
    result_df[["_np1"]]   <- NULL
  }

  # ============================================================
  # 9. 累加至全局缓存（对应 SAS _table_out 叠加逻辑）
  # ============================================================
  if (!exists(".table_out1", envir = globalenv())) {
    assign(".table_out1", NULL, envir = globalenv())
  }
  .GlobalEnv$.table_out1 <- dplyr::bind_rows(.GlobalEnv$.table_out1, result_df)

  # ============================================================
  # 10. outyn=0：不出表，直接返回
  # ============================================================
  if (outyn != 1) return(invisible(NULL))

  # ============================================================
  # 11. outyn=1：拼 varlist 字符串，调用 report_table() 出表
  #     对应 SAS 中 %report_table(data=_table_out, varlist=...) 调用
  # ============================================================
  out_df <- .GlobalEnv$.table_out1
  .GlobalEnv$.table_out1 <- NULL

  # 确保所有目标列存在（多次叠加时某次可能缺列）
  need_cols <- c(
    if (grp_num > 1) paste0("_np", seq_len(grp_num)),
    if (rowtotal == 1 || grp_num == 1) "_np999"
  )
  for (col in need_cols) {
    if (!col %in% names(out_df)) out_df[[col]] <- ""
  }

  # ---- 构建 varlist 字符串（格式："列名/列标题|..."）----
  # 对应 SAS：varlist=_label_/&topleftlabel.|_np1/组名1 $ (N=XX)|...|_np999/合计 $ (N=XX)
  # report_table 中用 "$" 换行，此处将 N=XX 追加为换行的第二行内容

  varlist_str <- paste0(".label/", topleftlabel)

  if (grp_num > 1) {
    for (i in seq_len(grp_num)) {
      lbl <- grp_names[i]
      if (outn == 1) lbl <- paste0(lbl, "$(N=", den_n_vec[i], ")")
      varlist_str <- paste0(varlist_str, "|_np", i, "/", lbl)
    }
  }

  # 合计列逻辑：如果是单组别，或者 rowtotal=1，则输出 _np999
  if (grp_num == 1 || rowtotal == 1) {
    lbl <- "合计"
    if (outn == 1) lbl <- paste0(lbl, "$(N=", den_n_total, ")")
    varlist_str <- paste0(varlist_str, "|_np999/", lbl)
  }

  # ---- 调用 report_table() ----
  ft <- report_table(
    data     = out_df,
    varlist  = varlist_str,
    title    = title,
    footnote = footnote,
    oneline  = oneline
  )

  # 附着原始参数，供导出管线 ft_to_report_table() 使用
  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "left", columnjust = "left", col1just = "left"
  )

  ft
}
