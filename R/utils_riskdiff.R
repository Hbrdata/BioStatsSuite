# =============================================================================
# utils_riskdiff.R
# 率差分析函数（R 包内部版本）
#
# 与 Shiny 工作流集成：
#   - 接收 mod_riskdiff_server 返回的参数
#   - 构建率差表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象
#
# 累积缓存：使用 .riskdiff_env 私有环境（outyn=0 时累积，outyn=1 时出表并清空）
# =============================================================================

# 私有累积环境：存储叠加表格，不污染 .GlobalEnv
.riskdiff_env <- new.env(parent = emptyenv())
.riskdiff_env$table_out <- NULL

#' 率差分析
#'
#' @param inds      数据框对象
#' @param data_cond 数据筛选条件（R 表达式字符串）
#' @param group     分组描述："分组变量名|组名1/组名2/..."
#' @param varlist   分析变量描述："变量名|变量标签|分类1=标签1/分类2=标签2/..."
#' @param cmhvar    分层变量名（非空时使用 MH 加权公共率差法）
#' @param alpha     置信水平，默认 0.05
#' @param method    计算方法："MH"=Wald 正态近似，"Newcombe"=Newcombe-Wilson
#' @param title     表格标题
#' @param footnote  底注内容
#' @param outyn     是否立即出表（1=是，0=仅累积）
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn=0，仅累积）
#' @noRd
riskdiff <- function(inds, data_cond, group, varlist, cmhvar = "",
                     alpha = 0.05, method = "MH",
                     title = NULL, footnote = NULL, outyn = 1) {

  # ============================================================
  # Step 1：解析参数
  # ============================================================
  # group: "变量名|组名1/组名2/..."
  grp_parts  <- stringr::str_split(group, "\\|", simplify = TRUE)
  grp_var    <- trimws(grp_parts[1])
  grp_names  <- stringr::str_split(trimws(grp_parts[2]), "/", simplify = TRUE)
  grp_names  <- grp_names[nchar(trimws(grp_names)) > 0]
  grp_num    <- length(grp_names)
  percent    <- (1 - alpha) * 100

  # varlist: "变量名|变量标签|分类1=标签1/分类2=标签2/..."
  vl_parts   <- stringr::str_split(varlist, "\\|", simplify = TRUE)
  ana_var    <- trimws(vl_parts[1])
  ana_label  <- trimws(vl_parts[2])
  cat_str    <- if (ncol(vl_parts) >= 3) trimws(vl_parts[3]) else ""
  cat_items  <- stringr::str_split(cat_str, "/")[[1]]
  cat_items  <- cat_items[nchar(trimws(cat_items)) > 0]
  cat_values <- stringr::str_remove_all(
    trimws(stringr::str_extract(cat_items, "^[^=]+")), "^['\"]|['\"]$"
  )

  # ============================================================
  # Step 2：筛选数据集
  # ============================================================
  if (!is.null(data_cond) && nchar(trimws(data_cond)) > 0 && trimws(data_cond) != "TRUE") {
    s0 <- inds |> dplyr::filter(!!rlang::parse_expr(data_cond))
  } else {
    s0 <- inds
  }

  s0 <- s0 |>
    dplyr::mutate(
      .grpcd    = match(.data[[grp_var]], grp_names),
      .catorder = match(as.character(.data[[ana_var]]), cat_values)
    )

  # ============================================================
  # Step 3：遍历所有两两组别组合，计算率差及置信区间
  # ============================================================
  use_cmhvar <- nchar(trimws(cmhvar)) > 0
  stat_list  <- list()
  stat_idx   <- 0L

  for (s in seq_len(grp_num - 1)) {
    for (x in seq(s + 1, grp_num)) {

      stat_idx  <- stat_idx + 1L
      label_str <- paste0(grp_names[s], "-", grp_names[x])
      s1        <- s0 |> dplyr::filter(.grpcd %in% c(s, x))

      # ---- 检查是否可计算率差 ----
      cat_per_grp <- s1 |>
        dplyr::filter(!is.na(.catorder), !is.na(.grpcd)) |>
        dplyr::group_by(.grpcd) |>
        dplyr::summarise(n_cat = dplyr::n_distinct(.catorder), .groups = "drop")

      can_compute <- nrow(cat_per_grp) == 2 && all(cat_per_grp$n_cat > 1)

      if (!can_compute) {
        stat_list[[stat_idx]] <- tibble::tibble(
          .label      = label_str,
          .method     = paste0("率差(", percent, "% CI)"),
          .riskdiffci = "NA"
        )
        next
      }

      # ---- 构造列联表 ----
      tbl <- s1 |>
        dplyr::filter(!is.na(.catorder), !is.na(.grpcd)) |>
        dplyr::mutate(.pos = dplyr::if_else(.catorder == 1L, 1L, 0L)) |>
        dplyr::group_by(.grpcd) |>
        dplyr::summarise(a = sum(.pos), n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(b = n - a, p = dplyr::if_else(n > 0, a / n, 0))

      row_s <- tbl |> dplyr::filter(.grpcd == s)
      row_x <- tbl |> dplyr::filter(.grpcd == x)

      a1 <- row_s$a[1]; n1 <- row_s$n[1]; a2 <- row_s$b[1]; p1 <- row_s$p[1]
      a3 <- row_x$a[1]; n2 <- row_x$n[1]; a4 <- row_x$b[1]; p2 <- row_x$p[1]

      # ----------------------------------------------------------
      # 3a. 考虑分层：MH 加权公共率差法（Sato 1989 方差）
      #     对应 SAS PROC FREQ RISKDIFF(common) CL=MH
      # ----------------------------------------------------------
      if (use_cmhvar) {
        z_val       <- qnorm(1 - alpha / 2)
        s1_clean    <- s1 |> dplyr::filter(!is.na(.catorder), !is.na(.grpcd))
        strata_vals <- unique(s1_clean[[trimws(cmhvar)]])

        mh_num <- 0; mh_den <- 0; sato_p <- 0; sato_q <- 0

        for (sv in strata_vals) {
          stratum <- s1_clean |> dplyr::filter(.data[[trimws(cmhvar)]] == sv)

          str_tbl <- stratum |>
            dplyr::mutate(.pos = dplyr::if_else(.catorder == 1L, 1L, 0L)) |>
            dplyr::group_by(.grpcd) |>
            dplyr::summarise(a = sum(.pos), n = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(b = n - a, p = dplyr::if_else(n > 0, a / n, 0))

          if (!all(c(s, x) %in% str_tbl$.grpcd)) next

          r_s <- str_tbl |> dplyr::filter(.grpcd == s)
          r_x <- str_tbl |> dplyr::filter(.grpcd == x)

          ns1 <- r_s$n[1]; ns2 <- r_x$n[1]; Ns <- ns1 + ns2
          as1 <- r_s$a[1]; as3 <- r_x$a[1]
          ps1 <- r_s$p[1]; ps2 <- r_x$p[1]

          if (Ns == 0) next

          ws     <- ns1 * ns2 / Ns
          mh_num <- mh_num + ws * (ps1 - ps2)
          mh_den <- mh_den + ws

          # Sato (1989) 方差分量
          Ph     <- (ns1^2 * as3 - ns2^2 * as1 + ns1 * ns2 * (ns2 - ns1) / 2) / Ns^2
          Qh     <- (as1 * (ns2 - as3) + as3 * (ns1 - as1)) / (2 * Ns)
          sato_p <- sato_p + Ph
          sato_q <- sato_q + Qh
        }

        if (mh_den == 0) {
          stat_list[[stat_idx]] <- tibble::tibble(
            .label      = label_str,
            .method     = paste0("率差(", percent, "% CI)"),
            .riskdiffci = "NA"
          )
          next
        }

        Value   <- mh_num / mh_den
        var_mh  <- (Value * sato_p + sato_q) / mh_den^2
        se_rd   <- sqrt(abs(var_mh))
        LowerCL <- Value - z_val * se_rd
        UpperCL <- Value + z_val * se_rd

      # ----------------------------------------------------------
      # 3b. 不考虑分层：Newcombe-Wilson 混合评分法
      # ----------------------------------------------------------
      } else if (toupper(trimws(method)) == "NEWCOMBE") {
        z_val   <- qnorm(1 - alpha / 2)
        l1 <- (2*a1 + z_val^2 - z_val*sqrt(z_val^2 + 4*a1*a2/n1)) / (2*(n1+z_val^2))
        l2 <- (2*a3 + z_val^2 - z_val*sqrt(z_val^2 + 4*a3*a4/n2)) / (2*(n2+z_val^2))
        u1 <- (2*a1 + z_val^2 + z_val*sqrt(z_val^2 + 4*a1*a2/n1)) / (2*(n1+z_val^2))
        u2 <- (2*a3 + z_val^2 + z_val*sqrt(z_val^2 + 4*a3*a4/n2)) / (2*(n2+z_val^2))
        Value   <- p1 - p2
        LowerCL <- Value - sqrt((p1 - l1)^2 + (u2 - p2)^2)
        UpperCL <- Value + sqrt((p2 - l2)^2 + (u1 - p1)^2)

      # ----------------------------------------------------------
      # 3c. 不考虑分层：Wald 正态近似法（默认，对应 SAS PROC FREQ RISKDIFF）
      # ----------------------------------------------------------
      } else {
        z_val   <- qnorm(1 - alpha / 2)
        Value   <- p1 - p2
        se_rd   <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
        LowerCL <- Value - z_val * se_rd
        UpperCL <- Value + z_val * se_rd
      }

      fmt    <- function(v) formatC(v * 100, format = "f", digits = 2)
      rd_str <- paste0(fmt(Value), "(", fmt(LowerCL), ",", fmt(UpperCL), ")")

      stat_list[[stat_idx]] <- tibble::tibble(
        .label      = label_str,
        .method     = paste0("率差(", percent, "% CI)"),
        .riskdiffci = rd_str
      )
    }
  }

  # ============================================================
  # Step 4：合并结果：指标标签首行 + 各比较对结果行
  # ============================================================
  stat_df   <- dplyr::bind_rows(stat_list)
  first_row <- tibble::tibble(
    .label      = ana_label,
    .method     = NA_character_,
    .riskdiffci = NA_character_
  )
  c1 <- dplyr::bind_rows(first_row, stat_df)
  # 非首行缩进两格（指标名行不缩进）
  if (nrow(c1) > 1) {
    c1$.label[-1] <- paste0("  ", stat_df$.label)
  }

  # ============================================================
  # Step 5：累积叠加（私有环境）
  # ============================================================
  if (is.null(.riskdiff_env$table_out)) {
    .riskdiff_env$table_out <- c1
  } else {
    .riskdiff_env$table_out <- dplyr::bind_rows(.riskdiff_env$table_out, c1)
  }

  # ============================================================
  # Step 6：outyn=0 → 静默返回，等待后续叠加
  # ============================================================
  if (outyn == 0) {
    return(invisible(NULL))
  }

  # ============================================================
  # Step 7：outyn=1 → 委托 report_table() 写入文档
  # ============================================================
  out_df <- .riskdiff_env$table_out

  # 加粗行：.method 为 NA 的行（即各指标的标签首行）
  bold_idx <- which(is.na(out_df$.method))

  # 构造 varlist 字符串：三列固定格式
  varlist_str <- paste(
    ".label/指标",
    ".method/检验方法",
    paste0(".riskdiffci/率差(", percent, "% CI)"),
    sep = "|"
  )

  ft <- report_table(
    data        = out_df,
    varlist     = varlist_str,
    title       = title,
    footnote    = footnote,
    headerjust  = "center",
    col1just    = "left",
    columnjust  = "center",
    autoaddnum  = "yes",
    bold_rows   = if (length(bold_idx) > 0) bold_idx else NULL
  )

  # 输出后清空私有累积数据
  .riskdiff_env$table_out <- NULL

  attr(ft, "hbr_varlist")  <- varlist_str
  attr(ft, "hbr_title")    <- title
  attr(ft, "hbr_footnote") <- footnote
  attr(ft, "hbr_styling_params") <- list(
    headerjust = "center", columnjust = "center", col1just = "left",
    bold_rows = if (length(bold_idx) > 0) bold_idx else NULL
  )

  return(invisible(ft))
}
