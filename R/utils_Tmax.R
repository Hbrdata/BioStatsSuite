# =============================================================================
# utils_Tmax.R
# Tmax 非参数检验及描述统计函数（R 包内部版本）
#
# 处理 BE 试验中 Tmax 的非参数检验（Wilcoxon 符号秩检验），
# 生成包含制剂、N、中位数、极值、统计量及 P 值的 flextable 表格。
#
# 与 Shiny 工作流集成：
#   - 接收 mod_Tmax_server 返回的参数
#   - 构建描述统计表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象
# =============================================================================

#' Tmax 非参数检验及描述统计
#'
#' @param inds        数据框对象
#' @param data_cond   数据筛选条件（R 表达式字符串）
#' @param subject     受试者唯一标识变量名
#' @param formulation 制剂变量名
#' @param Tmax        Tmax 变量名，默认 "Tmax"
#' @param T_val       受试制剂在 formulation 中的值，默认 "T"
#' @param R_val       参比制剂在 formulation 中的值，默认 "R"
#' @param Tlabel      受试制剂标签，默认 "受试制剂"
#' @param Rlabel      参比制剂标签，默认 "参比制剂"
#' @param title       表格标题
#' @param footnote    底注内容
#'
#' @return flextable 对象
#' @noRd
Tmax <- function(inds, data_cond, subject, formulation, Tmax = "Tmax",
                 T_val = "T", R_val = "R",
                 Tlabel = "受试制剂", Rlabel = "参比制剂",
                 title = NULL, footnote = NULL) {

  # ============================================================
  # 1. 筛选数据
  # ============================================================
  if (!is.null(data_cond) && nchar(trimws(data_cond)) > 0 && trimws(data_cond) != "TRUE") {
    s0 <- inds |> dplyr::filter(!!rlang::parse_expr(data_cond))
  } else {
    s0 <- inds
  }

  # ============================================================
  # 2. 描述性统计量计算
  # ============================================================
  summary_df <- s0 |>
    dplyr::filter(!!rlang::sym(formulation) %in% c(T_val, R_val)) |>
    dplyr::group_by(!!rlang::sym(formulation)) |>
    dplyr::summarise(
      N      = dplyr::n(),
      Median = format(round(median(!!rlang::sym(Tmax), na.rm = TRUE), 2), nsmall = 2),
      Min    = format(round(min(!!rlang::sym(Tmax), na.rm = TRUE), 2), nsmall = 2),
      Max    = format(round(max(!!rlang::sym(Tmax), na.rm = TRUE), 2), nsmall = 2),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # 排序逻辑：T 居上，R 居下
      min_max = paste0(Min, "-", Max),
      order = ifelse(!!rlang::sym(formulation) == T_val, 1, 2),
      flabel = ifelse(!!rlang::sym(formulation) == T_val, Tlabel, Rlabel)
    ) |>
    dplyr::arrange(order)

  # ============================================================
  # 3. 非参数检验（精确对标 SAS PROC UNIVARIATE Wilcoxon 符号秩检验）
  #
  #   SAS PROC UNIVARIATE 算法细节（已通过数据验证）：
  #   Step 1: 计算差值 D = T - R
  #   Step 2: 丢弃零差值（Wilcoxon 方法，非 Pratt）
  #   Step 3: 对非零差值的绝对值排秩，Ties 取平均秩
  #   Step 4: S = W+ - n_eff*(n_eff+1)/4
  #   Step 5: P 值用全枚举精确法（在所有 2^n_eff 种符号组合下，
  #             统计 |W+ - E(W+)| >= 观测值的比例）
  # ----------------------------------------------------------------
  data_t <- s0 |>
    dplyr::filter(!!rlang::sym(formulation) == T_val) |>
    dplyr::select(!!rlang::sym(subject), T_res = !!rlang::sym(Tmax))
  data_r <- s0 |>
    dplyr::filter(!!rlang::sym(formulation) == R_val) |>
    dplyr::select(!!rlang::sym(subject), R_res = !!rlang::sym(Tmax))
  merged_pair <- dplyr::inner_join(data_t, data_r, by = subject)
  merged_pair$diff <- merged_pair$T_res - merged_pair$R_res

  test_res <- tryCatch({
    # Step 1: 去除零差值
    nonzero <- merged_pair[merged_pair$diff != 0, ]
    n_eff   <- nrow(nonzero)

    if (n_eff == 0) {
      # 全部差值为零，无法检验
      return(list(stat = "0.00", p = "1.0000"))
    }

    # Step 2: 对绝对差值排秩（平均秩处理 Ties）
    ranks  <- rank(abs(nonzero$diff), ties.method = "average")
    W_plus <- sum(ranks[nonzero$diff > 0])
    E_W    <- n_eff * (n_eff + 1) / 4
    S_sas  <- W_plus - E_W

    # Step 3: 全枚举精确 P 值（两侧检验）
    obs_dev <- abs(W_plus - E_W)
    signs_mat <- as.matrix(expand.grid(rep(list(c(1, 0)), n_eff)))  # 1=正，0=负
    all_W_plus <- as.numeric(signs_mat %*% ranks)
    p_val <- mean(abs(all_W_plus - E_W) >= obs_dev - 1e-10)

    list(
      stat = format(round(S_sas, 2), nsmall = 2),
      p    = if (p_val < 0.0001) "<.0001" else formatC(p_val, format = "f", digits = 4)
    )
  }, error = function(e) {
    list(stat = "0.00", p = "1.0000")
  })

  # ============================================================
  # 4. 组织最终结果数据框
  # ============================================================
  out_df <- summary_df |>
    dplyr::mutate(
      Stat = ifelse(order == 1, test_res$stat, ""), # 仅在第一行显示统计量
      P    = ifelse(order == 1, test_res$p, "")     # 仅在第一行显示 P 值
    ) |>
    dplyr::select(flabel, N, Median, min_max, Stat, P)

  # ============================================================
  # 5. 调用 report_table() 构建输出
  # ============================================================
  varlist_str <- paste0(
    "flabel/制剂|N/N|Median/Median|min_max/Min-Max|Stat/统计量|P/P"
  )

  ft <- report_table(
    data     = out_df,
    varlist  = varlist_str,
    title    = title,
    footnote = footnote
  )

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left"
  )

  return(ft)
}
