# =============================================================================
# utils_q_describe.R
# 连续型变量描述性统计函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_q_describe_server 返回的参数（inds, data_cond, var_name 等）
#   - 内部构建标准宽表（.label / _np1~N / _np999），与 q_pairt、c_describe_m 共用格式
#   - 调用 report_table() 统一出表样式（三线表、自动编号等）
#   - 返回 flextable 对象，供 Shiny 结果面板显示和报告导出
#
# 累积缓存：与 q_pairt、c_describe_m 共用全局变量 .table_out1
#   outyn=0 时累积，outyn=1 时统一出表并清空
# =============================================================================

#' 连续型变量描述性统计
#'
#' @param inds              数据框对象
#' @param data_cond         数据筛选条件（R 表达式字符串）
#' @param denominator_data  分母数据框（可选），若为 NULL 则使用 inds
#' @param denominator_cond  分母数据筛选条件（R 表达式字符串）
#' @param var_name    分析变量名
#' @param var_label   变量显示标签
#' @param group_name  分组变量名
#' @param group_cond  分组条件（字符向量）
#' @param table_title 表格标题
#' @param ftnote      底注内容
#' @param totalyn     是否输出合计列（1=是，0=否）
#' @param outyn       是否立即出表（1=是，0=仅累积）
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
q_describe <- function(inds,
                       data_cond,
                       denominator_data = NULL,
                       denominator_cond = "TRUE",
                       var_name,
                       var_label,
                       group_name,
                       group_cond,
                       table_title,
                       ftnote,
                       totalyn,
                       outyn = 1) {

  # ============================================================
  # 内部辅助：描述性统计计算
  # ============================================================
  .desc_stats <- function(x) {
    n_val    <- sum(!is.na(x))
    miss_val <- sum(is.na(x))
    c(
      paste0(n_val, "(", miss_val, ")"),
      paste0(.format_num2(mean(x, na.rm = TRUE)), "(", .format_num2(stats::sd(x, na.rm = TRUE)), ")"),
      paste0(.format_num2(stats::median(x, na.rm = TRUE)), "(",
             .format_num2(stats::quantile(x, 0.25, type = 2, na.rm = TRUE)), ",",
             .format_num2(stats::quantile(x, 0.75, type = 2, na.rm = TRUE)), ")"),
      paste0(.format_num2(min(x, na.rm = TRUE)), ",", .format_num2(max(x, na.rm = TRUE)))
    )
  }

  row_labels <- c("  N(Missing)", "  Mean(SD)", "  Median(Q1,Q3)", "  Min,Max")

  grp_num <- length(group_cond)

  # ============================================================
  # Step 1：筛选数据
  # ============================================================
  data_0 <- .filter_by_expr(inds, data_cond)
  data_0 <- data_0 |> dplyr::filter(.data[[group_name]] %in% group_cond)

  d_0 <- data_0 |> dplyr::select(dplyr::all_of(c(var_name, group_name)))
  d_1 <- stats::setNames(d_0, c("var_0", "group_0"))
  d_1$group_0 <- factor(d_1$group_0, levels = group_cond)

  # ============================================================
  # Step 2：计算各组 N（用于表头 N=XX）
  # ============================================================
  # 分母数据：优先使用 denominator_data，否则回退到 inds
  den_source <- if (!is.null(denominator_data)) denominator_data else inds
  den_filtered <- .filter_by_expr(den_source, denominator_cond)
  den_filtered <- den_filtered |> dplyr::filter(.data[[group_name]] %in% group_cond)
  den_n_vec <- .group_denominator_n(den_filtered, group_name, group_cond)
  den_n_total <- sum(den_n_vec, na.rm = TRUE)

  # ============================================================
  # Step 3：描述性统计计算
  # ============================================================
  grp_stats <- lapply(group_cond, function(g) {
    x <- suppressWarnings(as.numeric(d_1$var_0[d_1$group_0 == g]))
    if (length(x[!is.na(x)]) == 0) rep("", 4) else .desc_stats(x)
  })

  x_all       <- suppressWarnings(as.numeric(d_1$var_0))
  total_stats <- if (length(x_all[!is.na(x_all)]) == 0) rep("", 4) else .desc_stats(x_all)

  # ============================================================
  # Step 4：组装宽表（.label / _np1~N / _np999）
  # ============================================================
  stats_df <- data.frame(.label = row_labels, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) {
    stats_df[[paste0("_np", i)]] <- grp_stats[[i]]
  }
  stats_df[["_np999"]] <- total_stats

  # 首行（变量标签行）
  first_row <- data.frame(.label = var_label, stringsAsFactors = FALSE)
  for (i in seq_len(grp_num)) first_row[[paste0("_np", i)]] <- ""
  first_row[["_np999"]] <- ""

  result_df <- dplyr::bind_rows(first_row, stats_df)

  # 单组别处理：_np1 合并到 _np999，删除 _np1
  if (grp_num == 1) {
    result_df[["_np999"]] <- result_df[["_np1"]]
    result_df[["_np1"]]   <- NULL
  }

  # ============================================================
  # Step 5：累积到全局缓存 .table_out1
  # ============================================================
  .append_table_cache(value = result_df)

  if (outyn != 1) return(invisible(NULL))

  # ============================================================
  # Step 6：outyn=1 → 构造 varlist，调用 report_table() 出表，清空缓存
  # ============================================================
  out_df <- .consume_table_cache()

  # 确保所有目标列存在，NA 替换为空字符串
  need_cols <- c(
    ".label",
    if (grp_num > 1L) paste0("_np", seq_len(grp_num)),
    "_np999"
  )
  out_df <- .ensure_report_columns(out_df, need_cols)

  # 构建 varlist 字符串
  varlist_str <- paste0(".label/", table_title)

  if (grp_num > 1L) {
    for (i in seq_len(grp_num)) {
      lbl <- group_cond[i]
      lbl <- paste0(lbl, "$(N=", den_n_vec[i], ")")
      varlist_str <- paste0(varlist_str, "|_np", i, "/", lbl)
    }
    if (totalyn == 1) {
      lbl <- paste0("合计$(N=", den_n_total, ")")
      varlist_str <- paste0(varlist_str, "|_np999/", lbl)
    }
  } else {
    lbl <- paste0("合计$(N=", den_n_total, ")")
    varlist_str <- paste0(varlist_str, "|_np999/", lbl)
  }

  # 调用 report_table() 出表
  ft <- report_table(
    data     = out_df,
    varlist  = varlist_str,
    title    = table_title,
    footnote = ftnote
  )

  .attach_report_attrs(ft, varlist_str, table_title, ftnote)
}
