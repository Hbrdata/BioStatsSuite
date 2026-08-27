# =============================================================================
# utils_lifetest.R
# 生存分析表格函数（R 包内部版本）
#
# 对时间-事件数据进行 Kaplan-Meier 生存分析，
# 输出指定时间点的生存率/失效率、分位数、删失率，并进行 Log-Rank 检验。
# =============================================================================

#' 生存分析表格
#'
#' @param inds         数据框对象
#' @param data_cond    数据筛选条件
#' @param group_c      分组描述："分组变量名|组名1/组名2/..."
#' @param censor       删失变量名（1=事件发生，0=删失）
#' @param time_label   时间描述："时间变量名|时间变量标签"
#' @param timelist     时间点列表（数值向量）
#' @param type         0=生存率，1=失效率
#' @param topleftlabel 第一列列标题
#' @param title        表格标题
#' @param footnote     底注内容
#'
#' @return flextable 对象
#' @noRd
lifetest <- function(inds, data_cond, group_c, censor, time_label, timelist,
                     type, topleftlabel, title, footnote) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  grp_parts <- strsplit(group_c, "|", fixed = TRUE)[[1]]
  grpvar    <- trimws(grp_parts[1])
  grpnames  <- trimws(strsplit(grp_parts[2], "/", fixed = TRUE)[[1]])
  grpnames  <- grpnames[nchar(grpnames) > 0]
  grp_num   <- length(grpnames)

  tl_parts <- strsplit(time_label, "|", fixed = TRUE)[[1]]
  time_var <- trimws(tl_parts[1])
  time_lbl <- trimws(tl_parts[2])

  # ============================================================
  # Step 2：筛选数据
  # ============================================================
  data_0 <- inds
  data_0 <- data_0 |> dplyr::filter(!!rlang::parse_expr(data_cond))
  data_0 <- data_0 |> dplyr::filter(.data[[grpvar]] %in% grpnames)

  d_0 <- data_0 |>
    dplyr::select(dplyr::all_of(c(time_var, censor, grpvar))) |>
    stats::setNames(c("time_0", "censor_0", "group_0")) |>
    dplyr::mutate(grp_cd = match(group_0, grpnames))

  # ============================================================
  # Step 3：KM 生存分析 + Log-Rank 检验
  # ============================================================
  surv_obj    <- survival::Surv(d_0$time_0, d_0$censor_0)
  fit_summary <- summary(
    survival::survfit(surv_obj ~ grp_cd, conf.type = "log-log", data = d_0),
    times = timelist
  )
  logrank <- survival::survdiff(surv_obj ~ grp_cd, data = d_0)
  lr_stat <- sprintf("%.2f", logrank$chisq)
  lr_p    <- sprintf("%.4f", 1 - stats::pchisq(logrank$chisq, df = length(logrank$n) - 1))

  # ============================================================
  # Step 4：整理各时间点估计值
  # ============================================================
  surv_df <- data.frame(
    grp_cd = as.integer(sub(".*=(\\d+)$", "\\1", as.character(fit_summary$strata))),
    time   = fit_summary$time,
    surv   = fit_summary$surv,
    lower  = fit_summary$lower,
    upper  = fit_summary$upper,
    stringsAsFactors = FALSE
  )

  if (type == 1) {
    surv_df <- surv_df |>
      dplyr::mutate(
        surv_disp  = sprintf("%.2f", (1 - surv)  * 100),
        lower_disp = sprintf("%.2f", (1 - upper) * 100),
        upper_disp = sprintf("%.2f", (1 - lower) * 100)
      )
  } else {
    surv_df <- surv_df |>
      dplyr::mutate(
        surv_disp  = sprintf("%.2f", surv  * 100),
        lower_disp = sprintf("%.2f", lower * 100),
        upper_disp = sprintf("%.2f", upper * 100)
      )
  }

  surv_df <- surv_df |>
    dplyr::mutate(dplyr::across(c(surv_disp, lower_disp, upper_disp),
                                ~ dplyr::if_else(is.na(.x), "0.00", .x))) |>
    dplyr::mutate(cell = paste0(surv_disp, "(", lower_disp, ",", upper_disp, ")"))

  # ============================================================
  # Step 5：计算分位数及删失率
  # ============================================================
  fit_obj <- survival::survfit(surv_obj ~ grp_cd, conf.type = "log-log", data = d_0)
  fit_tbl <- data.frame(summary(fit_obj)$table)
  quant   <- stats::quantile(fit_obj, probs = c(0.25, 0.5, 0.75), conf.int = TRUE)

  .fmt_quant <- function(q_mat, l_mat, u_mat, col_name, i) {
    q <- q_mat[i, col_name]
    if (is.na(q)) return("NA")
    l <- ifelse(is.na(l_mat[i, col_name]), ".", sprintf("%.2f", l_mat[i, col_name]))
    u <- ifelse(is.na(u_mat[i, col_name]), ".", sprintf("%.2f", u_mat[i, col_name]))
    paste0(sprintf("%.2f", q), "(", l, ",", u, ")")
  }

  q25 <- vapply(seq_len(grp_num), function(i)
    .fmt_quant(quant$quantile, quant$lower, quant$upper, "25", i), character(1))
  q50 <- vapply(seq_len(grp_num), function(i)
    .fmt_quant(quant$quantile, quant$lower, quant$upper, "50", i), character(1))
  q75 <- vapply(seq_len(grp_num), function(i)
    .fmt_quant(quant$quantile, quant$lower, quant$upper, "75", i), character(1))

  cens_rate <- sprintf("%.2f", ((fit_tbl$records - fit_tbl$events) / fit_tbl$records) * 100)

  # ============================================================
  # Step 6：构造宽格式输出
  # ============================================================
  n_per_grp <- d_0 |>
    dplyr::group_by(grp_cd) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(grp_cd)

  grp_col_labels <- vapply(seq_len(grp_num), function(i) {
    n_i <- n_per_grp$n[n_per_grp$grp_cd == i]
    n_i <- if (length(n_i) == 0) 0L else n_i
    paste0(grpnames[i], "$(N=", n_i, ")")
  }, character(1))

  col_name_label  <- ".label"
  col_name_groups <- make.names(grpnames, unique = TRUE)

  header_row <- c(
    paste0(time_lbl, "（Log-Rank=", lr_stat, ",P=", lr_p, "）"),
    rep("", grp_num)
  )

  time_rows <- lapply(seq_along(timelist), function(t_i) {
    t_val <- timelist[t_i]
    cells <- vapply(seq_len(grp_num), function(g) {
      row <- surv_df |> dplyr::filter(grp_cd == g, time == t_val)
      if (nrow(row) == 0) "—" else row$cell[1]
    }, character(1))
    c(as.character(t_val), cells)
  })

  q25_row  <- c("25%分位数(95%CI)", q25)
  q50_row  <- c("50%分位数(95%CI)", q50)
  q75_row  <- c("75%分位数(95%CI)", q75)
  cens_row <- c("删失率(%)", cens_rate)

  all_rows <- c(list(header_row), time_rows, list(q25_row, q50_row, q75_row, cens_row))

  out_df        <- as.data.frame(do.call(rbind, all_rows), stringsAsFactors = FALSE)
  names(out_df) <- c(col_name_label, col_name_groups)

  # ============================================================
  # Step 7：构造 varlist，调用 report_table()
  # ============================================================
  varlist_parts <- c(
    paste0(col_name_label, "/", topleftlabel),
    paste0(col_name_groups, "/", grp_col_labels)
  )
  varlist_str <- paste(varlist_parts, collapse = "|")

  ft <- report_table(
    data       = out_df,
    varlist    = varlist_str,
    title      = title,
    footnote   = footnote,
    headerjust = "center",
    col1just   = "left",
    columnjust = "center",
    autoaddnum = "yes",
    bold_rows  = 1L
  )

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left", bold_rows = 1L
  )

  ft
}
