# =============================================================================
# utils_crosstable.R
# 实验室交叉表函数（R 包内部版本）
#
# 对应 SAS 宏 %crosstable，通过 report_table() 输出三线表。
# =============================================================================

#' 实验室交叉表
#'
#' @param inds         数据框对象
#' @param data_cond    数据筛选条件
#' @param group_c      分组描述："分组变量名|组名1/组名2/..."
#' @param missing      缺失值替换内容
#' @param row_colvar   行列变量描述："行变量/行标签|列变量/列标签"
#' @param format       分类格式："值1=标签1|值2=标签2|..."
#' @param table_title  表格标题
#' @param footnote     底注内容
#'
#' @return flextable 对象
#' @noRd
c_crosstable <- function(inds, data_cond, group_c, missing, row_colvar,
                         format, table_title, footnote) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar    <- trimws(grp_parts[1])
  grpnames  <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grpnames  <- grpnames[nchar(grpnames) > 0]
  grp_num   <- length(grpnames)

  rc_parts <- strsplit(row_colvar, "|", fixed = TRUE)[[1]]
  row_var   <- trimws(strsplit(rc_parts[1], "/", fixed = TRUE)[[1]][1])
  row_label <- trimws(strsplit(rc_parts[1], "/", fixed = TRUE)[[1]][2])
  col_var   <- trimws(strsplit(rc_parts[2], "/", fixed = TRUE)[[1]][1])
  col_label <- trimws(strsplit(rc_parts[2], "/", fixed = TRUE)[[1]][2])

  cat_items <- strsplit(format, "|", fixed = TRUE)[[1]]
  cat_items <- cat_items[nchar(trimws(cat_items)) > 0]

  .split_first_eq <- function(x) {
    pos <- regexpr("=", x, fixed = TRUE)[1]
    c(
      gsub('^"|"$|^\'|\'$', "", trimws(substr(x, 1, pos - 1))),
      trimws(substr(x, pos + 1, nchar(x)))
    )
  }
  cat_pairs  <- lapply(cat_items, .split_first_eq)
  cat_vals   <- vapply(cat_pairs, `[[`, character(1), 1)
  cat_labels <- vapply(cat_pairs, `[[`, character(1), 2)
  cat_num    <- length(cat_vals)

  missing_val <- if (!is.null(missing)) as.character(missing) else NA_character_

  # ============================================================
  # Step 2：筛选数据
  # ============================================================
  data_0 <- inds
  data_0 <- data_0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(row_var, col_var, grpvar))) |>
    stats::setNames(c("row_0", "col_0", "group_0")) |>
    dplyr::mutate(
      row_0 = dplyr::if_else(
        (is.na(row_0) | trimws(as.character(row_0)) == "") & !is.na(missing_val),
        missing_val, as.character(row_0)),
      col_0 = dplyr::if_else(
        (is.na(col_0) | trimws(as.character(col_0)) == "") & !is.na(missing_val),
        missing_val, as.character(col_0)),
      row_cd = match(row_0, cat_vals),
      col_cd = match(col_0, cat_vals),
      grp_cd = match(group_0, grpnames)
    ) |>
    dplyr::filter(!is.na(row_cd), !is.na(col_cd), !is.na(grp_cd))

  # ============================================================
  # Step 3：按组计算交叉频数
  # ============================================================
  result_blocks <- vector("list", grp_num)

  for (g in seq_len(grp_num)) {
    grp_data <- d_0 |> dplyr::filter(grp_cd == g)

    freq_df <- grp_data |>
      dplyr::count(col_cd, row_cd, name = "freq") |>
      tidyr::complete(col_cd = seq_len(cat_num), row_cd = seq_len(cat_num),
                      fill = list(freq = 0L))

    col_totals <- freq_df |>
      dplyr::group_by(row_cd) |>
      dplyr::summarise(total = sum(freq), .groups = "drop")
    denom <- stats::setNames(col_totals$total, as.character(col_totals$row_cd))

    row_totals <- freq_df |>
      dplyr::group_by(col_cd) |>
      dplyr::summarise(total = sum(freq), .groups = "drop")

    grand_n <- sum(as.integer(denom), na.rm = TRUE)

    .fmt_np <- function(n, den) {
      n   <- as.integer(n)
      den <- as.integer(den)
      if (is.na(n))   n   <- 0L
      if (is.na(den)) den <- 0L
      if (den == 0L) return("0(0.00%)")
      pct <- sprintf("%.2f", n / den * 100)
      paste0(n, "(", pct, "%)")
    }

    # 各列变量分类行
    data_rows <- lapply(seq_len(cat_num), function(col_i) {
      row_i_vals <- vapply(seq_len(cat_num), function(row_i) {
        n <- freq_df$freq[freq_df$col_cd == col_i & freq_df$row_cd == row_i]
        n <- if (length(n) == 0) 0L else as.integer(n)
        .fmt_np(n, denom[as.character(row_i)])
      }, character(1))

      rn  <- row_totals$total[row_totals$col_cd == col_i]
      rn  <- if (length(rn) == 0) 0L else as.integer(rn)
      rpc <- if (grand_n > 0) sprintf("%.2f", rn / grand_n * 100) else "0.00"

      c(paste0("  ", cat_labels[col_i]), row_i_vals, paste0(rn, "(", rpc, "%)"))
    })

    # 合计行
    total_vals <- vapply(seq_len(cat_num), function(row_i) {
      as.character(as.integer(denom[as.character(row_i)]))
    }, character(1))
    total_row <- c("  合计", total_vals, as.character(grand_n))

    block_rows <- c(data_rows, list(total_row))

    # 多组别：插入组名行
    if (grp_num > 1) {
      grp_header <- c(grpnames[g], rep("", cat_num + 1))
      block_rows <- c(list(grp_header), block_rows)
    }

    block_df <- as.data.frame(do.call(rbind, block_rows), stringsAsFactors = FALSE)
    result_blocks[[g]] <- block_df
  }

  out_df <- do.call(rbind, result_blocks)
  rownames(out_df) <- NULL

  col_header_names <- c(col_label, cat_labels, "合计")
  names(out_df) <- col_header_names

  # ============================================================
  # Step 4：构造 varlist，调用 report_table()
  # ============================================================
  col_names_safe <- make.names(col_header_names, unique = TRUE)
  names(out_df)  <- col_names_safe

  varlist_str <- paste(
    mapply(function(safe, orig) paste0(safe, "/", orig),
           col_names_safe, col_header_names),
    collapse = "|"
  )

  # 多重表头
  doubleheader <- list(
    list(label = "",        cols = col_names_safe[1]),
    list(label = row_label, cols = col_names_safe[2:(cat_num + 1)]),
    list(label = "",        cols = col_names_safe[cat_num + 2])
  )

  # 加粗行：组名行
  if (grp_num > 1) {
    block_size <- cat_num + 2L
    bold_idx <- seq(1L, grp_num * block_size, by = block_size)
  } else {
    bold_idx <- integer(0)
  }

  ft <- report_table(
    data         = out_df,
    varlist      = varlist_str,
    title        = table_title,
    footnote     = footnote,
    headerjust   = "center",
    col1just     = "left",
    columnjust   = "center",
    autoaddnum   = "yes",
    doubleheader = doubleheader,
    bold_rows    = bold_idx
  )

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- table_title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left",
    doubleheader = doubleheader,
    bold_rows = if (length(bold_idx) > 0) bold_idx else NULL
  )

  ft
}
