# =============================================================================
# utils_aecnp.R
# AE/合并用药/实验室检查频数分析函数（R 包内部版本）
#
# 用途：用于对不良事件、合并用药、实验室检查新发异常等事件进行分组频数分析，
#       输出各组别及合计的例次（C）和例数/发生率（N(%)），支持多级分类变量。
#
# 与 Shiny 工作流集成：
#   - 接收模块返回的参数
#   - 接收分子数据框 inds 和分母数据框 denominator_data
#   - data_cond 为纯 R 表达式字符串，用于筛选 inds
#   - 内部构建标准宽表，调用 report_table() 统一出表样式
#   - 返回 flextable 对象（outyn=1）或数据框（outyn=0）
#
# 基本参数：
#   inds              = 分子数据框（如 adae、adcm、trans_lb 等）
#   data_cond         = 对 inds 的筛选条件，纯 R 表达式字符串
#   group             = 分组变量及组别名称，格式："分组变量名|组别1/组别2/……"
#   denominator_data  = 分母数据框（如 adsl），已由调用方筛选好
#   varlist           = 要分析的分类变量，"|" 分隔，格式："变量1|变量2|……"
#   subject_id        = 受试者编号变量名，用于计算例数（去重计数）
#   title             = 表格标题，默认 NULL
#   topleftlabel      = 表格左上角标签，默认 NULL（自动使用 varlist 变量名）
#   footnote          = 表格底注，默认 NULL
#   outyn             = 1 输出 flextable；0 返回数据框
#   coltotal          = 1 输出首行合计行；0 不输出
#   rowtotal          = 1 输出合计列；0 不输出
#
# 输出：
#   outyn=1 时返回 flextable 对象，含双行分组表头（组名行+例次/例数行）；
#   outyn=0 时返回整理后的数据框。
# =============================================================================

#' AE/合并用药/实验室检查频数分析
#'
#' @param inds              分子数据框
#' @param data_cond         数据筛选条件（R 表达式字符串）
#' @param group             分组描述："分组变量名|组名1/组名2/..."
#' @param denominator_data  分母数据框
#' @param varlist           分类变量列表："变量1|变量2|..."
#' @param subject_id        受试者编号变量名
#' @param title             表格标题
#' @param topleftlabel      左上角标签
#' @param footnote          底注
#' @param outyn             1=输出 flextable，0=返回数据框
#' @param coltotal          1=输出合计行，0=不输出
#' @param rowtotal          1=输出合计列，0=不输出
#'
#' @return flextable 对象或数据框
#' @noRd
aecnp <- function(inds,
                  data_cond,
                  group,
                  denominator_data,
                  varlist,
                  subject_id,
                  title        = NULL,
                  topleftlabel = NULL,
                  footnote     = NULL,
                  outyn        = 1,
                  coltotal     = 1,
                  rowtotal     = 1) {

  # ============================================================
  # 1. 解析参数
  # ============================================================
  # group: "变量名|组名1/组名2/..."
  group_parts <- stringr::str_split(group, "\\|", simplify = TRUE)
  grp_var     <- trimws(group_parts[1])
  grp_names   <- stringr::str_split(trimws(group_parts[2]), "/", simplify = TRUE)
  grp_names   <- grp_names[grp_names != ""]
  grp_num     <- length(grp_names)

  # varlist: "var1|var2|..."
  var_list   <- stringr::str_split(varlist, "\\|", simplify = TRUE)
  var_list   <- trimws(var_list[var_list != ""])
  anavarnum  <- length(var_list)

  # ============================================================
  # 2. 筛选分子数据集
  # ============================================================
  data_0 <- inds |>
    dplyr::filter(!!rlang::parse_expr(data_cond)) |>
    # 对应 SAS strip()：转字符型后去除前后空格
    dplyr::mutate(dplyr::across(dplyr::all_of(c(grp_var, var_list, subject_id)), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(grp_var, var_list, subject_id)), trimws))

  # ============================================================
  # 3. 筛选分母数据集（直接使用 denominator_data）
  # ============================================================
  den_0 <- denominator_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(grp_var, subject_id)), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(grp_var, subject_id)), trimws))

  # ============================================================
  # 4. 为每行打组别编码 .grpcd
  # ============================================================
  data_0 <- data_0 |>
    dplyr::mutate(.grpcd = match(.data[[grp_var]], grp_names))

  den_0 <- den_0 |>
    dplyr::mutate(.grpcd = match(.data[[grp_var]], grp_names))

  # ============================================================
  # 5. 占位行（防止某组没有记录导致缺失列）
  # ============================================================
  placeholder <- tibble::tibble(
    !!grp_var := grp_names,
    .grpcd     = seq_len(grp_num)
  )
  for (v in var_list)    placeholder[[v]]        <- "{需要删除}"
  placeholder[[subject_id]] <- "{需要删除}"

  data_0 <- dplyr::bind_rows(data_0, placeholder)

  # ============================================================
  # 6. 一级变量缺编码检查
  # ============================================================
  if (any(is.na(data_0[[var_list[1]]]))) {
    warning("数据集中有未编码的内容，请注意")
  }

  # ============================================================
  # 7. 分母 N
  # ============================================================
  den_n_tbl <- den_0 |>
    dplyr::count(.grpcd, name = "N") |>
    tidyr::complete(.grpcd = seq_len(grp_num), fill = list(N = 0L)) |>
    dplyr::arrange(.grpcd)

  den_n_vec   <- den_n_tbl$N                   # 各组 N，长度 = grp_num
  den_n_total <- sum(den_n_vec)                 # 合计 N

  # ============================================================
  # 8-10. 计算例次 C、例数 N，并补全所有 varlist 组合 x 组别
  # ============================================================

  # 剔除无效行，只保留真实数据：
  #   占位行（placeholder 插入的 "{需要删除}" 行）
  #   var_list 任意变量为 NA 或空字符串的行
  #   全行为 NA 的空行
  real_data <- data_0 |>
    dplyr::filter(
      !dplyr::if_any(dplyr::all_of(var_list), ~ . == "{需要删除}"),
      !dplyr::if_any(dplyr::all_of(var_list), ~ is.na(.) | trimws(.) == ""),
      !is.na(.data[[subject_id]])
    )

  # 空数据兜底
  NA_FILL <- "__TOTAL__"
  is_empty_data <- nrow(real_data) == 0

  if (is_empty_data) {
    # 构造空的 all_rows：只有全局合计行（row_level=0），每组 C=0，N=0
    zero_rows <- tibble::tibble(
      .grpcd = c(seq_len(grp_num), 999L), C = 0L, N = 0L, row_level = 0L
    )
    for (v in var_list) zero_rows[[v]] <- NA_FILL
    for (rc in paste0("rank_lv", seq_len(anavarnum))) zero_rows[[rc]] <- 0L
    all_rows <- zero_rows

    denom_map <- tibble::tibble(
      .grpcd = c(seq_len(grp_num), 999L),
      D      = c(den_n_vec, den_n_total)
    )
    all_rows <- all_rows |>
      dplyr::left_join(denom_map, by = ".grpcd") |>
      dplyr::mutate(
        P  = dplyr::if_else(D > 0, N / D * 100, 0),
        NP = paste0(N, "(", formatC(P, format = "f", digits = 2), ")")
      )

  } else {

    # 构造全量骨架：实际出现的 varlist 组合 x 所有组别
    real_combos <- real_data |>
      dplyr::distinct(dplyr::across(dplyr::all_of(var_list)))

    skeleton <- tidyr::crossing(
      real_combos,
      .grpcd = seq_len(grp_num)
    )

    # 例次 C
    c_raw <- real_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(var_list, ".grpcd")))) |>
      dplyr::summarise(C = dplyr::n(), .groups = "drop")

    c_tbl <- skeleton |>
      dplyr::left_join(c_raw, by = c(var_list, ".grpcd")) |>
      dplyr::mutate(C = tidyr::replace_na(C, 0L))

    # 例数 N（受试者去重）
    n_raw <- real_data |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_id, var_list, ".grpcd")))) |>
      dplyr::count(dplyr::across(dplyr::all_of(c(var_list, ".grpcd"))), name = "N")

    n_tbl <- skeleton |>
      dplyr::left_join(n_raw, by = c(var_list, ".grpcd")) |>
      dplyr::mutate(N = tidyr::replace_na(N, 0L))

    # 合并
    cn_tbl <- dplyr::left_join(c_tbl, n_tbl, by = c(var_list, ".grpcd"))

    # ============================================================
    # 11. 按层级构建各汇总行
    #     关键：中间层（SOC）的 N 必须回到原始数据去重计算，
    #     不能对底层 N 求和（否则同一受试者在多个 PT 下会被重复计数）
    # ============================================================

    # 最底层明细行（row_level = anavarnum）
    detail_rows <- cn_tbl
    detail_rows[["row_level"]] <- anavarnum

    # 中间层小计行（lv = 1 ~ anavarnum-1）
    if (anavarnum >= 2) {
      mid_rows <- lapply(seq_len(anavarnum - 1), function(lv) {
        grp_vars <- c(var_list[seq_len(lv)], ".grpcd")

        # 例次 C：底层 C 求和
        c_mid <- cn_tbl |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) |>
          dplyr::summarise(C = sum(C), .groups = "drop")

        # 例数 N：回到 real_data，按前 lv 个变量 + 组别对受试者去重
        n_mid <- real_data |>
          dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_id, var_list[seq_len(lv)], ".grpcd")))) |>
          dplyr::count(dplyr::across(dplyr::all_of(grp_vars)), name = "N")

        tmp <- dplyr::left_join(c_mid, n_mid, by = grp_vars)
        tmp[["N"]] <- tidyr::replace_na(tmp[["N"]], 0L)
        for (v in var_list[seq(lv + 1, anavarnum)]) tmp[[v]] <- NA_FILL
        tmp[["row_level"]] <- lv
        tmp
      })
      mid_combined <- dplyr::bind_rows(mid_rows)
    } else {
      mid_combined <- tibble::tibble()
    }

    # 全局合计行（row_level = 0）
    c_total <- cn_tbl |>
      dplyr::group_by(.grpcd) |>
      dplyr::summarise(C = sum(C), .groups = "drop")

    n_total <- real_data |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_id, ".grpcd")))) |>
      dplyr::count(.grpcd, name = "N")

    total_row <- dplyr::left_join(c_total, n_total, by = ".grpcd")
    total_row[["N"]] <- tidyr::replace_na(total_row[["N"]], 0L)
    for (v in var_list) total_row[[v]] <- NA_FILL
    total_row[["row_level"]] <- 0L

    # 合并各层
    all_rows <- dplyr::bind_rows(total_row, mid_combined, detail_rows)

    # 追加合计列（.grpcd = 999），C 求和，N 回原数据去重
    c_999 <- all_rows |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(var_list, "row_level")))) |>
      dplyr::summarise(C = sum(C), .groups = "drop")

    n_999_list <- lapply(0:anavarnum, function(lv) {
      if (lv == 0) {
        # 全局合计：所有受试者去重
        tmp <- tibble::tibble(
          N = nrow(dplyr::distinct(real_data, !!rlang::sym(subject_id))),
          row_level = 0L
        )
        for (v in var_list) tmp[[v]] <- NA_FILL
        tmp
      } else if (lv == anavarnum) {
        # 底层：按全部 var_list 去重
        real_data |>
          dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_id, var_list)))) |>
          dplyr::count(dplyr::across(dplyr::all_of(var_list)), name = "N") |>
          dplyr::mutate(row_level = lv)
      } else {
        # 中间层：按前 lv 个变量去重
        tmp <- real_data |>
          dplyr::distinct(dplyr::across(dplyr::all_of(c(subject_id, var_list[seq_len(lv)])))) |>
          dplyr::count(dplyr::across(dplyr::all_of(var_list[seq_len(lv)])), name = "N") |>
          dplyr::mutate(row_level = lv)
        for (v in var_list[seq(lv + 1, anavarnum)]) tmp[[v]] <- NA_FILL
        tmp
      }
    })
    n_999 <- dplyr::bind_rows(n_999_list)

    rows_999 <- dplyr::left_join(c_999, n_999, by = c(var_list, "row_level"))
    rows_999[["N"]] <- tidyr::replace_na(rows_999[["N"]], 0L)
    rows_999[[".grpcd"]] <- 999L
    all_rows <- dplyr::bind_rows(all_rows, rows_999)

    # 计算发生率 P，格式化 NP
    denom_map <- tibble::tibble(
      .grpcd = c(seq_len(grp_num), 999L),
      D      = c(den_n_vec, den_n_total)
    )

    all_rows <- all_rows |>
      dplyr::left_join(denom_map, by = ".grpcd") |>
      dplyr::mutate(
        P  = dplyr::if_else(D > 0, N / D * 100, 0),
        NP = paste0(N, "(", formatC(P, format = "f", digits = 2), ")")
      )

  } # end of else (非空数据路径)

  # ============================================================
  # 12. 排序：合计列例数（N）降序，例数相同时按变量名拼音升序
  # ============================================================
  rank_cols <- paste0("rank_lv", seq_len(anavarnum))

  if (!is_empty_data) {
    .pinyin_rank <- function(x) {
      if (requireNamespace("stringi", quietly = TRUE)) {
        stringi::stri_rank(x, locale = "zh")
      } else {
        old_lc <- Sys.getlocale("LC_COLLATE")
        on.exit(Sys.setlocale("LC_COLLATE", old_lc))
        Sys.setlocale("LC_COLLATE", "zh_CN.UTF-8")
        xtfrm(x)
      }
    }

    for (lv in seq_len(anavarnum)) {
      key_vars    <- var_list[seq_len(lv)]
      cur_var     <- var_list[lv]
      parent_vars <- if (lv == 1) character(0) else var_list[seq_len(lv - 1)]
      rank_col    <- paste0("rank_lv", lv)

      sk <- all_rows |>
        dplyr::filter(row_level == lv, .grpcd == 999L) |>
        dplyr::mutate(.pinyin_key = .pinyin_rank(.data[[cur_var]])) |>
        dplyr::arrange(
          dplyr::across(dplyr::all_of(parent_vars)),
          dplyr::desc(N),
          .pinyin_key
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(parent_vars))) |>
        dplyr::mutate(!!rank_col := dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::all_of(c(key_vars, rank_col)))

      all_rows <- all_rows |>
        dplyr::left_join(sk, by = key_vars)
    }

    for (rc in rank_cols) all_rows[[rc]][all_rows[["row_level"]] == 0L] <- 0L

    for (lv in seq_len(anavarnum)) {
      rc <- paste0("rank_lv", lv)
      all_rows[[rc]][all_rows[["row_level"]] < lv] <- 0L
    }
  }

  all_rows <- all_rows |>
    dplyr::arrange(dplyr::across(dplyr::all_of(rank_cols)), row_level, .grpcd)

  # ============================================================
  # 13. 构建宽表（每组 C/NP 列 + 合计 C/NP 列）
  # ============================================================
  grp_c_cols  <- paste0("C_",  seq_len(grp_num))
  grp_np_cols <- paste0("NP_", seq_len(grp_num))
  tot_c_col   <- "C_合计"
  tot_np_col  <- "NP_合计"

  id_cols <- c(var_list, "row_level", rank_cols)

  wide_c <- all_rows |>
    dplyr::filter(.grpcd %in% seq_len(grp_num)) |>
    dplyr::select(dplyr::all_of(c(id_cols, ".grpcd", "C"))) |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_cols),
                names_from = ".grpcd", values_from = "C",
                names_prefix = "C_", values_fill = 0L)

  wide_np <- all_rows |>
    dplyr::filter(.grpcd %in% seq_len(grp_num)) |>
    dplyr::select(dplyr::all_of(c(id_cols, ".grpcd", "NP"))) |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_cols),
                names_from = ".grpcd", values_from = "NP",
                names_prefix = "NP_", values_fill = "0(0.00)")

  out_wide <- dplyr::left_join(wide_c, wide_np, by = id_cols)

  # 兜底补列：空数据时 pivot_wider 不产生 C_N/NP_N 列
  for (g in seq_len(grp_num)) {
    cc <- paste0("C_",  g); nc <- paste0("NP_", g)
    if (!cc %in% names(out_wide)) out_wide[[cc]] <- 0L
    if (!nc %in% names(out_wide)) out_wide[[nc]] <- "0(0.00)"
  }

  if (rowtotal == 1) {
    if (is_empty_data) {
      tot <- all_rows |>
        dplyr::filter(.grpcd == 999L) |>
        dplyr::select(dplyr::all_of(c(var_list, "row_level", rank_cols, "C", "NP"))) |>
        dplyr::rename(!!tot_c_col := "C", !!tot_np_col := "NP")
    } else {
      tot <- all_rows |>
        dplyr::filter(.grpcd == 999L) |>
        dplyr::select(dplyr::all_of(c(id_cols, "C", "NP"))) |>
        dplyr::rename(!!tot_c_col := "C", !!tot_np_col := "NP")
    }
    out_wide <- dplyr::left_join(out_wide, tot, by = id_cols)
    if (!tot_c_col  %in% names(out_wide)) out_wide[[tot_c_col]]  <- 0L
    if (!tot_np_col %in% names(out_wide)) out_wide[[tot_np_col]] <- "0(0.00)"
  }

  # ============================================================
  # 14. 生成 .label 列（层级缩进）
  # ============================================================
  indent_spaces <- "  "

  out_wide[[".label"]] <- vapply(seq_len(nrow(out_wide)), function(i) {
    lv <- out_wide[["row_level"]][i]
    if (lv == 0L) {
      "合计"
    } else if (lv == anavarnum) {
      paste0(strrep(indent_spaces, anavarnum - 1),
             out_wide[[var_list[anavarnum]]][i])
    } else {
      paste0(strrep(indent_spaces, lv - 1),
             out_wide[[var_list[lv]]][i])
    }
  }, character(1))

  # 删除占位符行（{需要删除}）及层级key列为 NA_FILL 的冗余行
  out_wide <- out_wide |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(var_list), ~ stringr::str_detect(., fixed("{需要删除}"))))
  # 对 row_level>0 的行，其层级关键列不应为 NA_FILL
  keep_mask <- vapply(seq_len(nrow(out_wide)), function(i) {
    lv <- out_wide[["row_level"]][i]
    if (lv == 0L) return(TRUE)
    key_col <- var_list[min(lv, anavarnum)]
    val <- out_wide[[key_col]][i]
    !is.na(val) && val != NA_FILL
  }, logical(1))
  out_wide <- out_wide[keep_mask, ]

  # 不输出首行合计
  if (coltotal == 0) {
    out_wide <- out_wide |> dplyr::filter(row_level != 0L)
  }

  # ============================================================
  # 15. 组织最终输出列顺序
  # ============================================================
  data_cols <- as.vector(rbind(grp_c_cols, grp_np_cols))
  if (rowtotal == 1) data_cols <- c(data_cols, tot_c_col, tot_np_col)

  out_final <- out_wide |>
    dplyr::select(dplyr::all_of(c(".label", data_cols)))

  # outyn=0：直接返回数据框
  if (outyn != 1) return(out_final)

  # ============================================================
  # 16. 调用 report_table() 生成 flextable
  # ============================================================

  # 支持 $ 换行符
  topleft   <- stringr::str_replace_all(
    topleftlabel %||% paste(var_list, collapse = " / "),
    fixed("$"), "\n"
  )
  grp_names_disp <- stringr::str_replace_all(grp_names, fixed("$"), "\n")

  # varlist 参数：".label/topleft | C_1/例次 | NP_1/例数(%) | ..."
  vl_parts <- paste0(".label/", topleft)
  if (grp_num > 1) {
    for (g in seq_len(grp_num)) {
      vl_parts <- c(vl_parts,
                    paste0("C_",  g, "/例次"),
                    paste0("NP_", g, "/例数(%)"))
    }
  }
  if (rowtotal == 1) {
    vl_parts <- c(vl_parts, "C_合计/例次", "NP_合计/例数(%)")
  }
  varlist_arg <- paste(vl_parts, collapse = "|")

  # doubleheader 参数（双行表头结构）
  dh_list <- list()
  dh_list[[1]] <- list(label = topleft, cols = ".label")

  if (grp_num > 1) {
    for (g in seq_len(grp_num)) {
      grp_label <- paste0(grp_names_disp[g], "\n(N=", den_n_vec[g], ")")
      dh_list[[length(dh_list) + 1]] <- list(
        label = grp_label,
        cols = c(paste0("C_", g), paste0("NP_", g))
      )
    }
  }

  if (grp_num == 1 || rowtotal == 1) {
    dh_list[[length(dh_list) + 1]] <- list(
      label = paste0("合计\n(N=", den_n_total, ")"),
      cols = c(tot_c_col, tot_np_col)
    )
  }

  # 粗体行（合计行 & SOC 小计行）
  bold_rows <- which(out_wide[["row_level"]] < anavarnum)

  ft <- report_table(
    data         = out_final,
    varlist      = varlist_arg,
    title        = title,
    footnote     = footnote,
    doubleheader = dh_list,
    bold_rows    = bold_rows
  )

  return(ft)
}
