# =============================================================================
# utils_c_describe.R
# 分类变量描述性统计函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_c_describe_server 返回的参数
#   - 内部构建标准宽表（.label / _np1~N / _np999）
#   - 调用 report_table() 统一出表样式（三线表、自动编号等）
#   - 返回 flextable 对象，供 Shiny 结果面板显示和报告导出
#
# 累积缓存：使用 .table_out1（outyn=0 时累积，outyn=1 时出表并清空）
#   缓存读写通过 .append_table_cache() / .consume_table_cache() 包装。
# =============================================================================

#' 分类变量描述性统计
#'
#' @param inds              数据框对象
#' @param data_cond         数据筛选条件（R 表达式字符串）
#' @param denominator_data  分母数据框（可选），若为 NULL 则使用 inds
#' @param denominator_cond  分母数据筛选条件
#' @param varlist           分析变量描述："变量名|变量标签|值1=标签1/值2=标签2/..."
#' @param group_c           分组描述："分组变量名|组名1/组名2/..."
#' @param coltotal          是否输出合计行（1=是，0=否）
#' @param rowtotal          是否输出合计列（1=是，0=否）
#' @param outyn             是否立即出表（1=是，0=仅累积）
#' @param table_title       表格标题
#' @param ftnote            底注内容
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
c_describe <- function(inds, data_cond, denominator_data = NULL, denominator_cond = "TRUE", varlist, group_c,
                       coltotal, rowtotal, outyn = 1, table_title, ftnote) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  group_spec <- .parse_group_c(group_c)
  grpvar <- group_spec$var
  grpnames <- group_spec$levels
  grp_num <- group_spec$n

  var_spec <- .parse_varlist_spec(varlist, category = TRUE)
  ana_var <- var_spec$var
  ana_label <- var_spec$label
  cat_vals <- var_spec$cat_values
  cat_labels <- var_spec$cat_labels
  cat_num <- var_spec$cat_n

  # ============================================================
  # Step 2：筛选数据（分子）
  # ============================================================
  data_0 <- .filter_by_expr(inds, data_cond)
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(ana_var, grpvar))) |>
    stats::setNames(c("var_0", "group_0")) |>
    dplyr::mutate(
      grp_cd = match(group_0, grpnames),
      cat_cd = match(as.character(var_0), cat_vals)
    )

  # ============================================================
  # Step 3：筛选分母数据
  # ============================================================
  den_source <- if (!is.null(denominator_data)) denominator_data else inds
  data_1 <- .filter_by_expr(den_source, denominator_cond)
  data_1 <- data_1 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  # 各组分母 N
  denom_n <- .group_denominator_n(data_1, grpvar, grpnames, missing_as_zero = FALSE)
  total_n <- sum(denom_n, na.rm = TRUE)

  # ============================================================
  # Step 4：计算频数矩阵（cat_cd x grp_cd）
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

  total_all <- sum(grp_totals$freq)

  # ============================================================
  # Step 5：构造宽格式输出
  # ============================================================
  # 各分类行
  cat_rows <- lapply(seq_len(cat_num), function(ci) {
    grp_cells <- vapply(seq_len(grp_num), function(gi) {
      n_ij <- freq_df$freq[freq_df$cat_cd == ci & freq_df$grp_cd == gi]
      n_ij <- if (length(n_ij) == 0) 0L else as.integer(n_ij)
      .format_n_pct(n_ij, denom_n[gi])
    }, character(1))

    n_row   <- sum(freq_df$freq[freq_df$cat_cd == ci])
    total_cell <- .format_n_pct(n_row, total_n)

    c(paste0("  ", cat_labels[ci], "(%)"), grp_cells, total_cell)
  })

  # 合计行
  total_row_cells <- vapply(seq_len(grp_num), function(gi) {
    n_gi    <- as.integer(grp_totals$freq[grp_totals$grp_cd == gi])
    n_gi    <- if (length(n_gi) == 0) 0L else n_gi
    missing <- denom_n[gi] - n_gi
    paste0(n_gi, "(", missing, ")")
  }, character(1))
  miss_total <- total_n - total_all
  total_row  <- c("  合计(Missing)", total_row_cells,
                   paste0(total_all, "(", miss_total, ")"))

  # 变量标签首行
  header_row <- c(ana_label, rep("", grp_num + 1))

  all_rows <- c(list(header_row), cat_rows)
  if (coltotal == 1) all_rows <- c(all_rows, list(total_row))

  out_df <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
  col_name_label  <- ".label"
  col_name_groups <- paste0("_np", seq_len(grp_num))
  names(out_df) <- c(col_name_label, col_name_groups, "_np999")

  # 单组别：去掉 _np1
  if (grp_num == 1L) {
    out_df[["_np999"]] <- out_df[["_np1"]]
    out_df[["_np1"]]   <- NULL
  }

  # ============================================================
  # Step 6：累积到全局缓存
  # ============================================================
  .append_table_cache(value = out_df)

  if (outyn != 1) return(invisible(NULL))

  # ============================================================
  # Step 7：outyn=1 → 构造 varlist，调用 report_table()，清空缓存
  # ============================================================
  out_df <- .consume_table_cache()

  # 确保所有目标列存在
  need_cols <- c(".label", if (grp_num > 1L) paste0("_np", seq_len(grp_num)), "_np999")
  out_df <- .ensure_report_columns(out_df, need_cols)

  # 构建 varlist 字符串
  varlist_str <- paste0(".label/")

  if (grp_num > 1L) {
    for (i in seq_len(grp_num)) {
      lbl <- paste0(grpnames[i], "$(N=", denom_n[i], ")")
      varlist_str <- paste0(varlist_str, "|_np", i, "/", lbl)
    }
    if (rowtotal == 1) {
      lbl <- paste0("合计$(N=", total_n, ")")
      varlist_str <- paste0(varlist_str, "|_np999/", lbl)
    }
  } else {
    lbl <- paste0("合计$(N=", total_n, ")")
    varlist_str <- paste0(varlist_str, "|_np999/", lbl)
  }

  ft <- report_table(
    data     = out_df,
    varlist  = varlist_str,
    title    = table_title,
    footnote = ftnote
  )

  .attach_report_attrs(ft, varlist_str, table_title, ftnote)
}
