# =============================================================================
# utils_analysis_common.R
# 分析工具函数通用内部辅助。
# =============================================================================

#' 空值合并：NULL / NA / 空白字符串都回退到默认值
#'
#' `%||%` 只在 NULL 时回退，而 Shiny 文本输入未填写时返回 ""，
#' 会导致下游拼出空文件名等问题，故单独提供该 helper。
#' @noRd
.blank_default <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- x[1]
  if (is.na(x)) return(default)
  if (is.character(x) && nchar(trimws(x)) == 0) return(default)
  x
}

#' 生成默认报告文件名（不含扩展名）
#' @noRd
.default_report_name <- function(prefix = "report") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

#' 清洗文件名，移除路径分隔符等非法字符
#' @noRd
.sanitize_filename <- function(x, default = NULL) {
  default <- default %||% .default_report_name()
  x <- .blank_default(x, default)
  x <- gsub("[\\\\/:*?\"<>|]+", "_", as.character(x))
  x <- gsub("\\.docx$", "", x, ignore.case = TRUE)
  x <- trimws(x)
  if (nchar(x) == 0) default else x
}

#' 标准化筛选表达式
#' @noRd
.normalize_filter_expr <- function(x, default = "TRUE") {
  if (is.null(x) || !is.character(x) || length(x) == 0 || nchar(trimws(x[1])) == 0) {
    return(default)
  }
  x[1]
}

#' 解析分组描述："分组变量|组1/组2/..."
#' @noRd
.parse_group_c <- function(group_c) {
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar <- trimws(grp_parts[1])
  grpnames <- if (length(grp_parts) >= 2) {
    trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  } else {
    character(0)
  }
  grpnames <- grpnames[nchar(grpnames) > 0]

  list(
    var = grpvar,
    levels = grpnames,
    n = length(grpnames)
  )
}

#' 解析变量描述："变量名|变量标签|值1=标签1/值2=标签2/..."
#' @noRd
.parse_varlist_spec <- function(varlist, category = FALSE) {
  vl_parts <- strsplit(varlist, "|", fixed = TRUE)[[1]]
  ana_var <- trimws(vl_parts[1])
  ana_label <- if (length(vl_parts) >= 2) trimws(vl_parts[2]) else ana_var

  out <- list(
    var = ana_var,
    label = ana_label,
    raw_categories = if (length(vl_parts) >= 3) trimws(vl_parts[3]) else ""
  )

  if (isTRUE(category)) {
    cat_items <- strsplit(out$raw_categories, "/", fixed = TRUE)[[1]]
    cat_items <- cat_items[nchar(trimws(cat_items)) > 0]
    cat_pairs <- strsplit(cat_items, "=", fixed = TRUE)
    out$cat_values <- vapply(cat_pairs, function(x) trimws(x[1]), character(1))
    out$cat_labels <- vapply(cat_pairs, function(x) {
      if (length(x) >= 2) trimws(x[2]) else trimws(x[1])
    }, character(1))
    out$cat_n <- length(out$cat_values)
  }

  out
}

#' 按表达式筛选数据
#' @noRd
.filter_by_expr <- function(data, cond) {
  cond <- .normalize_filter_expr(cond)
  data |> dplyr::filter(!!rlang::parse_expr(cond))
}

#' 按分组水平计算分母 N
#' @noRd
.group_denominator_n <- function(data, group_var, group_levels, missing_as_zero = TRUE) {
  if (isTRUE(missing_as_zero)) {
    return(as.integer(table(factor(data[[group_var]], levels = group_levels))[group_levels]))
  }
  as.integer(table(data[[group_var]])[group_levels])
}

#' 格式化百分比单元格
#' @noRd
.format_n_pct <- function(n, denom) {
  pct <- if (!is.na(denom) && denom > 0) sprintf("%.2f", n / denom * 100) else "0.00"
  paste0(n, "(", pct, ")")
}

#' 格式化两位小数
#' @noRd
.format_num2 <- function(x) {
  formatC(x, format = "f", digits = 2)
}

#' 确保 report_table 输入列存在且 NA 为空字符串
#' @noRd
.ensure_report_columns <- function(out_df, cols) {
  for (col in cols) {
    if (!col %in% names(out_df)) {
      out_df[[col]] <- ""
    } else {
      out_df[[col]] <- ifelse(is.na(out_df[[col]]), "", out_df[[col]])
    }
  }
  out_df
}

#' 添加报告导出所需 flextable 属性
#' @noRd
.attach_report_attrs <- function(ft, varlist, title, footnote,
                                 styling_params = list(headerjust = "left", columnjust = "left", col1just = "left")) {
  attr(ft, "hbr_varlist") <- varlist
  attr(ft, "hbr_title") <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- styling_params
  ft
}

#' 累积表格缓存
#' @noRd
.append_table_cache <- function(name = ".table_out1", value) {
  if (!exists(name, envir = globalenv())) {
    assign(name, NULL, envir = globalenv())
  }
  assign(name, dplyr::bind_rows(get(name, envir = globalenv()), value), envir = globalenv())
  invisible(NULL)
}

#' 取出并清空表格缓存
#' @noRd
.consume_table_cache <- function(name = ".table_out1") {
  value <- if (exists(name, envir = globalenv())) get(name, envir = globalenv()) else NULL
  assign(name, NULL, envir = globalenv())
  value
}
