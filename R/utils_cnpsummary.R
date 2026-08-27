# =============================================================================
# utils_cnpsummary.R
# 事件发生情况频数分析函数（R 包内部版本）
#
# 用途：对"某件事情/某种发生情况"的频数分析，结果为1行；
#       一般可以为不良事件总结；
#       输出各组别及合计的例次（C）、例数N(%)，可选输出组间率差(95%CI)及P值；
#
# 与 Shiny 工作流集成：
#   - 接收模块返回的参数
#   - 接收分子数据框 inds 和分母数据框 denominator_data
#   - data_cond 为纯 R 表达式字符串，用于筛选 inds
#   - 内部构建标准宽表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象（outyn=1）或 NULL（outyn!=1，仅累积）
#
# 累积缓存：使用 .GlobalEnv$.cnpsummary_tbl（outyn!=1 时累积，outyn=1 时出表并清空）
#
# 基本参数：
#   inds              = 分子数据框（如 adae）
#   data_cond         = 对 inds 的筛选条件，纯 R 表达式字符串
#   group             = 分组变量及组别名称，格式："分组变量名|组别1/组别2/……"
#   denominator_data  = 分母数据框（如 adsl），已由调用方筛选好
#   leftlabel         = 表格最左侧的标签
#   subject_id        = 受试者编号变量名，用于计算例数（去重计数）
#   title             = 表格标题，默认 NULL
#   topleftlabel      = 左上角标签，默认 ""
#   footnote          = 底注，默认 NULL
#   rowtotal          = 1 输出合计列；0 不输出
#   outyn             = 1 输出表格并清空累加数据；其他仅累积
#   out_des           = 1 输出各组别例次、例数(%)；默认 1
#   out_rd            = 1 输出两两组间率差(95%CI)及P值；默认不输出
#   out_p             = 1 输出所有组间比较的卡方统计量及P值；默认不输出
#   p                 = 1 计算P值；默认不计算
#   alpha             = 置信水平，默认 0.05
#
# 计算逻辑：
#   例次C：分析数据集中该label下的记录条数（不去重，即事件次数）；
#   例数N：受试者去重后的人数；
#   分母D：来自 denominator_data 数据集，各组受试者总数；
#   发生率P：N/D x 100%；
#   两两组间：Fisher精确检验P值 + 率差(95%CI)（正态近似法）；
#   全组间：卡方检验统计量及P值；
# =============================================================================

#' 事件发生情况频数分析
#'
#' @param inds              分子数据框
#' @param data_cond         数据筛选条件（R 表达式字符串）
#' @param group             分组描述："分组变量名|组名1/组名2/..."
#' @param denominator_data  分母数据框
#' @param leftlabel         左侧标签
#' @param subject_id        受试者编号变量名
#' @param title             表格标题
#' @param topleftlabel      左上角标签
#' @param footnote          底注
#' @param rowtotal          1=输出合计列，0=不输出
#' @param outyn             1=输出表格并清空累积；其他=仅累积
#' @param out_des           1=输出各组例次/例数(%)
#' @param out_rd            1=输出两两率差(95%CI)及P值
#' @param out_p             1=输出全组卡方统计量及P值
#' @param p                 1=计算P值
#' @param alpha             置信水平
#'
#' @return flextable 对象（outyn=1）或 NULL（outyn!=1）
#' @noRd
cnpsummary <- function(inds,
                       data_cond,
                       group,
                       denominator_data,
                       leftlabel,
                       subject_id,
                       title        = NULL,
                       topleftlabel = "",
                       footnote     = NULL,
                       rowtotal     = 1,
                       outyn        = 1,
                       out_des      = 1,
                       out_rd       = NULL,
                       out_p        = NULL,
                       p            = NULL,
                       alpha        = 0.05) {

  # ============================================================
  # 内部工具函数
  # ============================================================

  # SAS语法 -> R语法自动转换
  .sas2r <- function(expr_str) {
    if (is.null(expr_str) || nchar(trimws(expr_str)) == 0) return("")
    s <- expr_str
    # ^missing(x) -> !is.na(x)
    s <- gsub("\\^missing\\(([^)]+)\\)", "!is.na(\\1)", s, perl = TRUE, ignore.case = TRUE)
    # missing(x) -> is.na(x)
    s <- gsub("(?<!\\^)\\bmissing\\(([^)]+)\\)", "is.na(\\1)", s, perl = TRUE, ignore.case = TRUE)
    # and / or / not（词边界，忽略大小写）
    s <- gsub("(?<![!<>=&|])\\band\\b(?!=)", " & ", s, perl = TRUE, ignore.case = TRUE)
    s <- gsub("(?<![!<>=&|])\\bor\\b(?!=)",  " | ", s, perl = TRUE, ignore.case = TRUE)
    s <- gsub("\\bnot\\b",                    "!",   s, perl = TRUE, ignore.case = TRUE)
    # ^= -> !=
    s <- gsub("\\^=", "!=", s)
    # 孤立的 = 转 ==（排除已有 !=、<=、>=、==）
    s <- gsub("([^!<>=])=([^=])", "\\1==\\2", s, perl = TRUE)
    s
  }

  # P值格式化（与SAS pvalue6.4格式一致）
  .fmt_p <- function(pv) {
    if (is.na(pv))        return(NA_character_)
    if (pv < 0.0001)      return("<0.0001")
    formatC(pv, format = "f", digits = 4)
  }

  # N(P%) 格式化（与SAS 6.2格式一致）
  .fmt_np <- function(n, p) {
    paste0(n, "(", formatC(p, format = "f", digits = 2), ")")
  }

  # ============================================================
  # 1. 解析 group 参数
  # ============================================================
  # 参数防御性检查
  p <- if (is.null(p) || length(p) == 0) 0 else p
  out_p <- if (is.null(out_p) || length(out_p) == 0) 0 else out_p
  out_rd <- if (is.null(out_rd) || length(out_rd) == 0) 0 else out_rd

  grp_parts  <- stringr::str_split(group, "\\|", n = 2, simplify = TRUE)
  grp_var    <- trimws(grp_parts[1])
  grp_names  <- trimws(stringr::str_split(grp_parts[2], "/", simplify = TRUE))
  grp_names  <- grp_names[nchar(grp_names) > 0]
  grp_num    <- length(grp_names)

  # ============================================================
  # 2. 筛选分析数据集 -> _s0
  # ============================================================
  d_cond <- .sas2r(data_cond)

  s0 <- if (nchar(d_cond) > 0) {
    inds |> dplyr::filter(!!rlang::parse_expr(d_cond))
  } else inds

  # 添加 _label_ 和 _grpcd_（组别编码）
  s0 <- s0 |>
    dplyr::mutate(
      `_label_` = leftlabel,
      `_grpcd_` = match(as.character(.data[[grp_var]]), grp_names)
    )

  # SAS逻辑：为每个组别各插入1条哨兵记录，防止某组无数据时错乱
  sentinel_rows <- lapply(seq_len(grp_num), function(i) {
    row <- s0[0, , drop = FALSE]   # 取空行保留列结构
    if (nrow(row) == 0) {
      row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(s0)))
      names(row) <- names(s0)
    } else {
      row <- s0[1, , drop = FALSE]
    }
    row[[grp_var]]   <- grp_names[i]
    row[["_label_"]] <- "{需要删除}"
    row[["_grpcd_"]] <- i
    row
  })
  s0 <- dplyr::bind_rows(s0, dplyr::bind_rows(sentinel_rows))

  # ============================================================
  # 3. 筛选分母数据集（直接使用 denominator_data）
  # ============================================================
  denominator <- denominator_data |>
    dplyr::mutate(`_grpcd_` = match(as.character(.data[[grp_var]]), grp_names))

  # 各组分母 D
  denom_n <- denominator |>
    dplyr::filter(!is.na(`_grpcd_`)) |>
    dplyr::count(`_grpcd_`, name = "D")

  # 构建分母向量（对应每个组别）
  d_vec <- stats::setNames(integer(grp_num), seq_len(grp_num))
  for (i in seq_len(grp_num)) {
    v <- denom_n$D[denom_n$`_grpcd_` == i]
    if (length(v) > 0) d_vec[i] <- as.integer(v[1])
  }
  d_999 <- sum(d_vec)   # 合计分母

  # 表头标签：(N=XX)
  grp_ln      <- paste0("(N=", d_vec, ")")
  grp_ln_999  <- paste0("(N=", d_999, ")")

  # ============================================================
  # 4. 计算例次 C（不去重的条数，含哨兵行，后续会被删除）
  # ============================================================
  t0 <- s0 |>
    dplyr::filter(!is.na(`_grpcd_`)) |>
    dplyr::group_by(`_label_`, `_grpcd_`) |>
    dplyr::summarise(n_cnt = dplyr::n(), .groups = "drop")

  # 各组例次（宽表）
  c_wide <- t0 |>
    tidyr::pivot_wider(id_cols = `_label_`, names_from = `_grpcd_`,
                values_from = n_cnt, names_prefix = "_c",
                values_fill = 0L)
  # 合计例次
  c_wide[["_c999"]] <- rowSums(
    c_wide |> dplyr::select(dplyr::starts_with("_c")) |> dplyr::select(-dplyr::any_of("_c999")),
    na.rm = TRUE
  )

  # ============================================================
  # 5. 计算例数 N（subject_id 去重后的人数）
  # ============================================================
  s1 <- s0 |>
    dplyr::filter(!is.na(`_grpcd_`)) |>
    dplyr::distinct(!!rlang::sym(subject_id), `_grpcd_`, `_label_`)

  t1 <- s1 |>
    dplyr::group_by(`_label_`, `_grpcd_`) |>
    dplyr::summarise(n_cnt = dplyr::n(), .groups = "drop")

  n_wide <- t1 |>
    tidyr::pivot_wider(id_cols = `_label_`, names_from = `_grpcd_`,
                values_from = n_cnt, names_prefix = "_n",
                values_fill = 0L)
  n_wide[["_n999"]] <- rowSums(
    n_wide |> dplyr::select(dplyr::starts_with("_n")) |> dplyr::select(-dplyr::any_of("_n999")),
    na.rm = TRUE
  )

  # ============================================================
  # 6. 合并为 _c1，计算发生率P，生成 _c2
  # ============================================================
  c1 <- dplyr::left_join(c_wide, n_wide, by = "_label_")

  # 补齐可能缺失的组别列
  for (i in seq_len(grp_num)) {
    cn <- paste0("_c", i); nn <- paste0("_n", i)
    if (!cn %in% names(c1)) c1[[cn]] <- 0L
    if (!nn %in% names(c1)) c1[[nn]] <- 0L
  }
  if (!"_c999" %in% names(c1)) c1[["_c999"]] <- 0L
  if (!"_n999" %in% names(c1)) c1[["_n999"]] <- 0L

  # 若_c1只有哨兵行（全为"{需要删除}"），构造一行全0的真实行
  real_labels <- c1[["_label_"]][!grepl("需要删除", c1[["_label_"]])]
  if (length(real_labels) == 0) {
    zero_row <- c1[1, , drop = FALSE]
    zero_row[["_label_"]] <- leftlabel
    for (i in seq_len(grp_num)) {
      zero_row[[paste0("_c", i)]] <- 0L
      zero_row[[paste0("_n", i)]] <- 0L
    }
    zero_row[["_c999"]] <- 0L
    zero_row[["_n999"]] <- 0L
    c1 <- zero_row
  }

  # 生成 _c2：删除哨兵行，处理空值，计算NP，计算_dmn
  c2 <- c1 |>
    dplyr::filter(!grepl("需要删除", `_label_`)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("_c") | dplyr::starts_with("_n"), ~ tidyr::replace_na(.x, 0L)))

  # 计算各组 P% 和 NP
  for (i in seq_len(grp_num)) {
    ni  <- as.numeric(c2[[paste0("_n", i)]])
    di  <- d_vec[i]
    pi  <- if (di > 0) ni / di * 100 else rep(0, nrow(c2))
    c2[[paste0("_p",  i)]] <- pi
    c2[[paste0("_np", i)]] <- mapply(.fmt_np, ni, pi)
    c2[[paste0("_d",  i)]] <- di
    c2[[paste0("_dmn",i)]] <- di - ni     # 分母 - 分子（未发生数）
  }
  # 合计列
  n999 <- as.numeric(c2[["_n999"]])
  p999 <- if (d_999 > 0) n999 / d_999 * 100 else rep(0, nrow(c2))
  c2[["_p999"]]  <- p999
  c2[["_np999"]] <- mapply(.fmt_np, n999, p999)
  c2[["_d999"]]  <- d_999

  # ============================================================
  # 7. 计算P值（p=1时执行）
  # ============================================================
  stat_list <- list()   # 两两比较结果列表
  chisq_val <- NA_real_
  chisq_p   <- NA_character_

  if (isTRUE(p == 1)) {

    # 从 _c2 提取各组 _n 和 _dmn（仅取第1行，因为 cnpsummary 是单行输出）
    n_vals   <- stats::setNames(as.integer(c2[1, paste0("_n",   seq_len(grp_num))]),
                         seq_len(grp_num))
    dmn_vals <- stats::setNames(as.integer(c2[1, paste0("_dmn", seq_len(grp_num))]),
                         seq_len(grp_num))

    # 两两组间 Fisher 精确检验 + 率差(95%CI)
    statnum <- 0L
    for (s_i in seq_len(grp_num - 1)) {
      for (s_x in seq(s_i + 1, grp_num)) {
        statnum <- statnum + 1L

        n_a   <- n_vals[s_i];    dmn_a <- dmn_vals[s_i]
        n_b   <- n_vals[s_x];    dmn_b <- dmn_vals[s_x]
        d_a   <- d_vec[s_i];     d_b   <- d_vec[s_x]

        # 如果两组均无发生或均全部发生，输出NA
        if ((n_a == 0 && n_b == 0) || (dmn_a == 0 && dmn_b == 0)) {
          rdCI   <- "NA"
          pvalue <- 1.0
        } else {
          # Fisher精确检验
          mat    <- matrix(c(n_a, n_b, dmn_a, dmn_b), nrow = 2)
          ft_res <- tryCatch(stats::fisher.test(mat), error = function(e) NULL)
          pvalue <- if (!is.null(ft_res)) ft_res$p.value else 1.0

          # 率差(95%CI)：正态近似法
          p_a  <- if (d_a > 0) n_a / d_a else 0
          p_b  <- if (d_b > 0) n_b / d_b else 0
          rd   <- p_a - p_b
          se   <- sqrt(p_a * (1 - p_a) / max(d_a, 1) + p_b * (1 - p_b) / max(d_b, 1))
          z    <- stats::qnorm(1 - alpha / 2)
          lo   <- rd - z * se
          hi   <- rd + z * se
          # 格式：率差(下界,上界)，单位%，保留2位小数
          rdCI <- paste0(
            formatC(rd * 100, format = "f", digits = 2), "(",
            formatC(lo * 100, format = "f", digits = 2), ",",
            formatC(hi * 100, format = "f", digits = 2), ")"
          )
        }

        stat_list[[statnum]] <- list(
          label_a  = grp_names[s_i],
          label_b  = grp_names[s_x],
          rdCI_col = paste0("_rdCI",   statnum),
          pval_col = paste0("_pvalue", statnum),
          rdCI     = rdCI,
          pvalue   = .fmt_p(pvalue)
        )

        c2[[paste0("_rdCI",   statnum)]] <- rdCI
        c2[[paste0("_pvalue", statnum)]] <- .fmt_p(pvalue)
      }
    }

    # 所有组间卡方检验
    sum_n   <- sum(n_vals)
    sum_dmn <- sum(dmn_vals)

    if (sum_n == 0 || sum_dmn == 0) {
      chisq_val <- 0
      chisq_p   <- .fmt_p(1)
    } else {
      chi_mat <- matrix(c(n_vals, dmn_vals), nrow = grp_num)
      chi_res <- tryCatch(stats::chisq.test(chi_mat, correct = FALSE), error = function(e) NULL)
      if (!is.null(chi_res)) {
        chisq_val <- unname(chi_res$statistic)
        chisq_p   <- .fmt_p(chi_res$p.value)
      } else {
        chisq_val <- 0
        chisq_p   <- .fmt_p(1)
      }
    }

    c2[["Value"]] <- formatC(chisq_val, format = "f", digits = 2)
    c2[["prob"]]  <- chisq_p
  }

  # ============================================================
  # 8. 叠加到全局累加对象
  # ============================================================
  env <- globalenv()

  if (!exists(".cnpsummary_tbl", envir = env) ||
      is.null(get(".cnpsummary_tbl", envir = env))) {
    assign(".cnpsummary_tbl", c2, envir = env)
  } else {
    prev <- get(".cnpsummary_tbl", envir = env)
    assign(".cnpsummary_tbl", dplyr::bind_rows(prev, c2), envir = env)
  }

  # ============================================================
  # 9. 若 outyn != 1，不输出表格
  # ============================================================
  if (is.null(outyn) || outyn != 1) {
    message("[cnpsummary] 已追加一行：", leftlabel, "（outyn非1，暂不输出表格）")
    return(invisible(NULL))
  }

  # ============================================================
  # 10. 取出累加数据并重置
  # ============================================================
  tbl_data <- get(".cnpsummary_tbl", envir = env)
  assign(".cnpsummary_tbl", NULL, envir = env)

  # 全部转为字符型
  tbl_data <- tbl_data |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # ============================================================
  # 11. 组装 varlist / doubleheader
  # ============================================================
  col_vars   <- "_label_"
  col_labels <- topleftlabel

  # 双层表头组
  dh_groups <- list()

  # out_des（各组别例次、例数）
  if (isTRUE(out_des == 1)) {
    # 只有当组数 > 1 时，才循环输出各组别列
    if (grp_num > 1) {
      for (i in seq_len(grp_num)) {
        ci_col  <- paste0("_c",  i)
        npi_col <- paste0("_np", i)
        col_vars   <- c(col_vars,   ci_col,  npi_col)
        col_labels <- c(col_labels, "例次",  "例数(%)")
        dh_groups  <- c(dh_groups, list(list(
          label = paste0(grp_names[i], "\n", grp_ln[i]),
          cols   = c(ci_col, npi_col)
        )))
      }
    }

    # 无论 1 组还是多组，只要 rowtotal=1，就输出合计
    if (rowtotal == 1) {
      col_vars   <- c(col_vars,   "_c999",  "_np999")
      col_labels <- c(col_labels, "例次",   "例数(%)")
      dh_groups  <- c(dh_groups, list(list(
        label = if (grp_num == 1) paste0(grp_names[1], "\n", grp_ln_999) else paste0("合计\n", grp_ln_999),
        cols   = c("_c999", "_np999")
      )))
    }
  }

  # out_rd（两两率差CI及P值）
  if (isTRUE(p == 1) && isTRUE(out_rd == 1) && length(stat_list) > 0) {
    for (k in seq_along(stat_list)) {
      st <- stat_list[[k]]
      col_vars   <- c(col_vars,   st$rdCI_col,                    st$pval_col)
      col_labels <- c(col_labels, paste0("率差(", (1 - alpha) * 100, "%CI)"), "P值")
      dh_groups  <- c(dh_groups, list(list(
        label = paste0("组间比较\n", st$label_a, "-", st$label_b),
        cols   = c(st$rdCI_col, st$pval_col)
      )))
    }
  }

  # out_p（全组统计量及P值）
  if (isTRUE(p == 1) && isTRUE(out_p == 1)) {
    col_vars   <- c(col_vars,   "Value",  "prob")
    col_labels <- c(col_labels, "统计量", "P值")
    dh_groups  <- c(dh_groups, list(list(
      label = "组间比较\n所有组",
      cols   = c("Value", "prob")
    )))
  }

  # 仅保留实际存在于数据中的列
  col_vars_exist  <- col_vars[col_vars %in% names(tbl_data)]
  col_labels_exist <- col_labels[col_vars %in% names(tbl_data)]

  tbl_out <- tbl_data |> dplyr::select(dplyr::all_of(col_vars_exist))
  # 补全NA列
  for (col in col_vars_exist) {
    tbl_out[[col]] <- tidyr::replace_na(tbl_out[[col]], "")
  }

  # ============================================================
  # 12. 调用 report_table() 输出 flextable
  # ============================================================
  # 构建 varlist 字符串："var1/label1|var2/label2|..."
  vl_str <- paste(paste(col_vars_exist, col_labels_exist, sep = "/"), collapse = "|")

  report_table(
    data         = tbl_out,
    varlist      = vl_str,
    title        = title,
    footnote     = footnote,
    doubleheader = if (length(dh_groups) > 0) dh_groups else NULL,
    autoaddnum   = "no"
  )

}
