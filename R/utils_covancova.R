# =============================================================================
# utils_covancova.R
# 协方差分析（ANCOVA）函数（R 包内部版本）
#
# 输出两张表格：
#   表 1（因素分析）：各因素的 F 值和 P 值
#   表 2（组间比较）：各组 LSMean 及 95%CI，以及组对比较
# =============================================================================

#' 协方差分析
#'
#' @param inds       数据框对象
#' @param data_cond  数据筛选条件
#' @param group_c    分组描述："分组变量名|组名1/组名2/..."
#' @param varlist    变量描述："因变量/标签|中心变量/标签|基线变量/标签"
#' @param title1     表 1 标题
#' @param title2     表 2 标题
#' @param footnote1  表 1 底注
#' @param footnote2  表 2 底注
#'
#' @return list(table1=flextable, table2=flextable)
#' @noRd
covancova <- function(inds, data_cond, group_c, varlist,
                      title1, title2, footnote1, footnote2) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar    <- trimws(grp_parts[1])
  grpnames  <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grpnames  <- grpnames[nchar(grpnames) > 0]

  vl_parts <- strsplit(varlist, "|", fixed = TRUE)[[1]]

  ana_var   <- trimws(strsplit(vl_parts[1], "/")[[1]][1])
  ana_label <- trimws(strsplit(vl_parts[1], "/")[[1]][2])

  site_var   <- trimws(strsplit(vl_parts[2], "/")[[1]][1])
  site_label <- trimws(strsplit(vl_parts[2], "/")[[1]][2])

  base_var   <- trimws(strsplit(vl_parts[3], "/")[[1]][1])
  base_label <- trimws(strsplit(vl_parts[3], "/")[[1]][2])

  # ============================================================
  # Step 2：筛选数据
  # ============================================================
  data_0 <- inds
  data_0 <- data_0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(ana_var, site_var, base_var, grpvar))) |>
    stats::setNames(c("anavar_0", "siteno_0", "base_0", "group_0")) |>
    dplyr::mutate(
      siteno_0 = as.factor(siteno_0),
      group_0  = factor(group_0, levels = grpnames)
    )

  # ============================================================
  # Step 3：拟合协方差模型（III类平方和）
  # ============================================================
  ancova_model <- stats::lm(anavar_0 ~ siteno_0 + group_0 + base_0, data = d_0)
  anova_res    <- car::Anova(ancova_model, type = "III")
  emm_res      <- emmeans::emmeans(ancova_model, ~ group_0)
  pairs_res    <- pairs(emm_res)

  # ============================================================
  # Step 4：构造表 1（因素分析）
  # ============================================================
  anova_df <- as.data.frame(anova_res)

  .fmt_p <- function(p) {
    if (is.na(p)) return(NA_character_)
    if (p < 0.0001) "<.0001" else sprintf("%.4f", p)
  }

  factor_map <- list(
    siteno_0 = list(label = site_label, var = "siteno_0"),
    group_0  = list(label = "治疗",     var = "group_0"),
    base_0   = list(label = base_label, var = "base_0")
  )

  factor_rows <- lapply(names(factor_map), function(v) {
    fm  <- factor_map[[v]]
    row <- anova_df[rownames(anova_df) == v, , drop = FALSE]
    f_v <- if (nrow(row) > 0 && !is.na(row[["F value"]])) sprintf("%.2f", row[["F value"]]) else NA_character_
    p_v <- if (nrow(row) > 0) .fmt_p(row[["Pr(>F)"]]) else NA_character_
    c(NA_character_, fm$label, f_v, p_v)
  })

  header_row_t1 <- c(ana_label, NA_character_, NA_character_, NA_character_)
  t1_rows       <- c(list(header_row_t1), factor_rows)
  t1_df         <- as.data.frame(do.call(rbind, t1_rows), stringsAsFactors = FALSE)
  names(t1_df)  <- c(".label", ".factor", ".F", ".P")

  # ============================================================
  # Step 5：构造表 2（组间比较）
  # ============================================================
  emm_df    <- as.data.frame(emm_res)
  pairs_df  <- as.data.frame(pairs_res)

  emm_rows <- lapply(seq_len(nrow(emm_df)), function(i) {
    c(as.character(emm_df$group_0[i]),
      sprintf("%.2f", emm_df$emmean[i]),
      sprintf("%.2f", emm_df$lower.CL[i]),
      sprintf("%.2f", emm_df$upper.CL[i]))
  })

  pairs_rows <- lapply(seq_len(nrow(pairs_df)), function(i) {
    se_i  <- pairs_df$SE[i]
    est_i <- pairs_df$estimate[i]
    c(as.character(pairs_df$contrast[i]),
      sprintf("%.2f", est_i),
      sprintf("%.2f", est_i - 1.96 * se_i),
      sprintf("%.2f", est_i + 1.96 * se_i))
  })

  header_row_t2 <- c(ana_label, NA_character_, NA_character_, NA_character_)
  t2_rows       <- c(list(header_row_t2), emm_rows, pairs_rows)
  t2_df         <- as.data.frame(do.call(rbind, t2_rows), stringsAsFactors = FALSE)
  names(t2_df)  <- c(".label", ".lsmean", ".cil", ".ciu")

  # ============================================================
  # Step 6：调用 report_table() 出表
  # ============================================================
  varlist_t1 <- ".label/指标|.factor/因素|.F/F值|.P/P值"

  ft1 <- report_table(
    data       = t1_df,
    varlist    = varlist_t1,
    title      = title1,
    footnote   = footnote1,
    headerjust = "center",
    col1just   = "left",
    columnjust = "center",
    autoaddnum = "yes",
    bold_rows  = 1L
  )
  attr(ft1, "hbr_varlist")  <- varlist_t1
  attr(ft1, "hbr_title")    <- title1
  attr(ft1, "hbr_footnote") <- footnote1
  attr(ft1, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left", bold_rows = 1L
  )

  varlist_t2 <- ".label/治疗水平及差值|.lsmean/LSMean|.cil/95% CIL|.ciu/95% CIU"

  ft2 <- report_table(
    data       = t2_df,
    varlist    = varlist_t2,
    title      = title2,
    footnote   = footnote2,
    headerjust = "center",
    col1just   = "left",
    columnjust = "center",
    autoaddnum = "yes",
    bold_rows  = 1L
  )
  attr(ft2, "hbr_varlist")  <- varlist_t2
  attr(ft2, "hbr_title")    <- title2
  attr(ft2, "hbr_footnote") <- footnote2
  attr(ft2, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left", bold_rows = 1L
  )

  list(table1 = ft1, table2 = ft2)
}
