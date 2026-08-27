# =============================================================================
# utils_reporttable.R
# 清单列表型表格输出，对应 SAS %report_table 宏。
#
# 对应原始文件: Function/report_table.R
# 主要变更:
#   .GlobalEnv$pre_title / inner_num → .doc_ctx_get_pre_title/inner_num()
#   .GlobalEnv$inner_num 自增        → .doc_ctx_set_inner_num()
#   .ods_initialized()               → .doc_ctx_initialized()
#   .ods_get_style()                 → .doc_ctx_get_style()
#   .ods_get_doc()                   → .doc_ctx_get_doc()
#   .ods_set_doc()                   → .doc_ctx_set_doc()
#   .ods_flush_pending_section()     → .doc_ctx_flush_pending()
#   .ods_set_pending_section()       → .doc_ctx_set_pending()
#   .ods_add_table()                 → .doc_ctx_add_table()
# =============================================================================

# ---- 内部辅助函数 ----

#' XML 特殊字符转义
#' @noRd
.xml_escape <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x <- gsub("'",  "&apos;", x, fixed = TRUE)
  x
}

#' 脚注预处理（兼容 SAS \line 标记）
#' @noRd
.process_footnote <- function(fn) {
  if (is.null(fn) || length(fn) == 0 || nchar(trimws(as.character(fn))) == 0) {
    return(NULL)
  }
  fn <- as.character(fn)
  fn <- gsub("\\\\line", "\n", fn, ignore.case = TRUE)
  lines <- strsplit(fn, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  fn <- paste(lines, collapse = "\n")
  return(fn)
}

#' SAS 筛选语法 → R 语法转换
#' @noRd
.sas2r_filter <- function(expr_str) {
  if (is.null(expr_str) || nchar(trimws(expr_str)) == 0) return("")
  expr_str <- gsub("(?<![!<>=])\\band\\b(?![=])", " & ",
                   expr_str, perl = TRUE, ignore.case = TRUE)
  expr_str <- gsub("(?<![!<>=])\\bor\\b(?![=])",  " | ",
                   expr_str, perl = TRUE, ignore.case = TRUE)
  expr_str <- gsub("\\bnot\\b", " !", expr_str, perl = TRUE, ignore.case = TRUE)
  expr_str <- gsub("([^!<>=])=([^=])", "\\1==\\2", expr_str, perl = TRUE)
  expr_str
}

#' 向 Word 写入表格标题段落（带大纲级别）
#' @noRd
.write_table_title <- function(doc, title_str, level_int = 7L, fs = 10.5) {
  sp        <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
  font_cn   <- if (!is.null(sp)) sp$font_cn   else "等线"
  font_en   <- if (!is.null(sp)) sp$font_en   else "Times New Roman"
  font_size <- if (!is.null(sp)) sp$font_size else fs

  sz_val    <- as.integer(round(font_size * 2))
  title_esc <- .xml_escape(as.character(title_str))

  if (as.integer(level_int) > 0L) {
    outline_xml <- sprintf('<w:outlineLvl w:val="%d"/>', as.integer(level_int) - 1L)
  } else {
    outline_xml <- ""
  }

  xml_str <- sprintf(
    '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
      <w:pPr>
        %s
        <w:jc w:val="left"/>
      </w:pPr>
      <w:r>
        <w:rPr>
          <w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/>
          <w:sz w:val="%d"/>
          <w:szCs w:val="%d"/>
        </w:rPr>
        <w:t xml:space="preserve">%s</w:t>
      </w:r>
    </w:p>',
    outline_xml,
    font_en, font_cn, font_en, font_en,
    sz_val, sz_val,
    title_esc
  )

  officer::body_add_xml(doc, str = xml_str, pos = "after")
}


# ---- 主函数 ----

#' 输出清单表格并进行序号自动排列
#'
#' @param data         需要输出的数据框对象
#' @param varlist      变量名及标签，格式："var1/标签1|var2/标签2|……"
#' @param title        表格标题内容（不含编号，编号由函数自动拼接）
#' @param cond         数据筛选条件（R 表达式字符串，默认 NULL）
#' @param footnote     脚注内容（字符，默认 NULL；换行用 "\n"）
#' @param fs           表格标题字体大小（磅），默认 10.5
#' @param level        表格标题的大纲级别，默认 7
#' @param headerjust   表头对齐："left"（默认）/ "center" / "right"
#' @param columnjust   数据列对齐（第一列除外）："left"（默认）
#' @param col1just     第一列对齐："left"（默认）
#' @param oneline      非 NULL 时所有列等宽
#' @param blank        "on"（默认）第一列保留空格；"off" 折叠
#' @param doubleheader 多重表头，list，每元素 list(label=…, cols=c(…))
#' @param autoaddnum   "yes"（默认）自动编号；"no" 不编号
#' @param autofit      是否自动列宽，默认 TRUE
#' @param fontsize     表格正文字体大小（磅），默认从 hbr3 style 读取
#' @param outyn        1（默认）返回 flextable；0 返回数据框
#' @param write_to_doc 是否写入 Word（默认 NULL，自动判断）
#' @param bold_rows    需要加粗的行索引向量
#' @return flextable 对象（outyn=1）或数据框（outyn=0）
#' @noRd
report_table <- function(data,
                         varlist,
                         title        = NULL,
                         cond         = NULL,
                         footnote     = NULL,
                         fs           = 10.5,
                         level        = 7,
                         headerjust   = "left",
                         columnjust   = "left",
                         col1just     = "left",
                         oneline      = NULL,
                         blank        = "on",
                         doubleheader = NULL,
                         autoaddnum   = "yes",
                         fontsize     = NULL,
                         outyn        = 1,
                         write_to_doc = NULL,
                         bold_rows    = NULL) {

  library(dplyr)
  library(stringr)
  library(rlang)
  library(flextable)
  library(officer)

  # 从上下文读取正文字号
  sp <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
  if (is.null(fontsize)) {
    fontsize <- if (!is.null(sp)) sp$font_size else 10
  }

  # ================================================================
  # Step 1：解析 varlist
  # ================================================================
  var_parts  <- str_split(varlist, "\\|", simplify = TRUE)
  var_parts  <- var_parts[nchar(trimws(var_parts)) > 0]
  num_vars   <- length(var_parts)

  var_names  <- character(num_vars)
  var_labels <- character(num_vars)
  for (i in seq_len(num_vars)) {
    vp            <- str_split(var_parts[i], "/", n = 2, simplify = TRUE)
    var_names[i]  <- trimws(vp[1])
    raw_label     <- if (ncol(vp) >= 2) trimws(vp[2]) else ""
    var_labels[i] <- raw_label
  }

  # ================================================================
  # Step 2：oneline 列宽比例
  # ================================================================
  col_width_pct <- floor(98 / num_vars)

  # ================================================================
  # Step 3：自动编号
  # ================================================================
  if (autoaddnum == "yes") {
    pre_title_val <- .doc_ctx_get_pre_title()
    inner_num_val <- .doc_ctx_get_inner_num()

    inner_num_val <- inner_num_val + 1L
    .doc_ctx_set_inner_num(inner_num_val)

    title_str <- trimws(as.character(title %||% ""))
    if (nchar(title_str) > 0) {
      full_title      <- paste0("表 ", pre_title_val, ".", inner_num_val, " ", title_str)
      full_title_none <- paste0("表 ", pre_title_val, ".", inner_num_val, " ", title_str, "（无）")
    } else {
      full_title      <- paste0("表 ", pre_title_val, ".", inner_num_val)
      full_title_none <- paste0("表 ", pre_title_val, ".", inner_num_val, "（无）")
    }
    message(sprintf("***** 正在输出表格：%s", full_title))
  } else {
    title_str       <- trimws(as.character(title %||% ""))
    full_title      <- title_str
    full_title_none <- paste0(title_str, "（无）")
    message(sprintf("***** 正在输出表格：%s", full_title))
  }

  # ================================================================
  # Step 4：大纲级别
  # ================================================================
  level_int <- as.integer(level)

  # ================================================================
  # Step 5：筛选数据
  # ================================================================
  cond_r <- .sas2r_filter(cond)
  if (nchar(cond_r) > 0) {
    data <- tryCatch(
      data |> filter(!!parse_expr(cond_r)),
      error = function(e) stop(paste0("筛选条件解析失败：", conditionMessage(e)))
    )
  }
  data_out <- data |> select(all_of(var_names))

  # ================================================================
  # Step 6：outyn=0 → 返回数据框
  # ================================================================
  if (outyn == 0) return(invisible(data_out))

  # ================================================================
  # Step 7：确定是否写入 Word
  # ================================================================
  if (is.null(write_to_doc)) {
    write_to_doc <- tryCatch(.doc_ctx_initialized(), error = function(e) FALSE)
  }

  # ================================================================
  # Step 8：数据为空 → 仅输出"（无）"标题
  # ================================================================
  if (nrow(data_out) == 0) {
    empty_df        <- as.data.frame(matrix(character(0), ncol = num_vars))
    names(empty_df) <- var_names
    ft              <- flextable(empty_df)
    ft              <- set_header_labels(ft,
                         .list = setNames(as.list(var_labels), var_names))
    ft              <- .apply_style_to_ft(ft, sp,
                                          headerjust = headerjust,
                                          col1just   = col1just,
                                          bodyjust   = columnjust)

    if (write_to_doc && tryCatch(.doc_ctx_initialized(), error = function(e) FALSE)) {
      .doc_ctx_flush_pending()
      doc <- .doc_ctx_get_doc()
      doc <- .write_table_title(doc, full_title_none, level_int, fs)
      .doc_ctx_set_doc(doc)
      .doc_ctx_set_pending()
      .doc_ctx_add_table()
    }
    return(invisible(ft))
  }

  # ================================================================
  # Step 9：构建 flextable
  # ================================================================
  ft <- flextable(data_out, col_keys = var_names)

  # 9a. 表头标签
  if (is.null(doubleheader)) {
    var_labels_disp <- gsub("$", "\n", var_labels, fixed = TRUE)

    for (i in seq_len(num_vars)) {
      lbl <- var_labels_disp[i]
      if (nchar(lbl) == 0) {
        ft <- compose(ft, j = var_names[i], part = "header",
                      value = as_paragraph(""))
      } else if (grepl("\n", lbl, fixed = TRUE)) {
        parts <- strsplit(lbl, "\n", fixed = TRUE)[[1]]
        cl    <- lapply(seq_along(parts), function(k) {
          if (k == 1) as_chunk(parts[k]) else as_chunk(paste0("\n", parts[k]))
        })
        ft <- compose(ft, j = var_names[i], part = "header",
                      value = as_paragraph(list_values = cl))
      } else {
        ft <- compose(ft, j = var_names[i], part = "header",
                      value = as_paragraph(as_chunk(lbl)))
      }
    }
  } else {
    row1 <- rep("", num_vars)
    row2 <- var_labels
    for (dh in doubleheader) {
      for (cn in dh$cols) {
        idx <- which(var_names == cn)
        if (length(idx) > 0) row1[idx] <- dh$label
      }
    }
    hdr_df <- data.frame(col_keys = var_names,
                         row1 = row1, row2 = row2,
                         stringsAsFactors = FALSE)
    ft <- set_header_df(ft, mapping = hdr_df, key = "col_keys")
    ft <- merge_h(ft, part = "header")
    ft <- merge_v(ft, part = "header")

    for (i in seq_len(num_vars)) {
      if (nchar(trimws(row2[i])) == 0)
        ft <- compose(ft, i = 2L, j = var_names[i], part = "header",
                      value = as_paragraph(""))
      if (nchar(trimws(row1[i])) == 0)
        ft <- compose(ft, i = 1L, j = var_names[i], part = "header",
                      value = as_paragraph(""))
    }
  }

  # 9b. 统一应用 hbr3 样式
  ft <- .apply_style_to_ft(ft, sp,
                            headerjust = headerjust,
                            col1just   = col1just,
                            bodyjust   = columnjust)

  # 9b-补1：doubleheader 细线
  if (!is.null(doubleheader)) {
    labeled_cols <- character(0)
    for (dh in doubleheader) {
      if (nchar(trimws(dh$label)) > 0) {
        labeled_cols <- c(labeled_cols, dh$cols)
      }
    }
    if (length(labeled_cols) > 0) {
      labeled_idx <- which(var_names %in% labeled_cols)
      ft <- flextable::hline(ft, i = 1L, j = labeled_idx, part = "header",
                             border = officer::fp_border(color = "black", width = 0.75))
    }
  }

  # 9b-补2：doubleheader 多列 group 的 row1 居中对齐
  if (!is.null(doubleheader)) {
    for (dh in doubleheader) {
      if (length(dh$cols) >= 2) {
        ft <- flextable::align(ft, i = 1L, j = dh$cols, part = "header", align = "center")
      }
    }
  }

  footnote <- .process_footnote(footnote)

  # 9c. 脚注
  if (!is.null(footnote) && nchar(trimws(as.character(footnote))) > 0) {
    fn_lines <- strsplit(as.character(footnote), "\n", fixed = TRUE)[[1]]
    fn_lines <- fn_lines[nchar(trimws(fn_lines)) > 0]
    if (length(fn_lines) == 0) fn_lines <- as.character(footnote)

    ft <- add_footer_lines(ft, values = fn_lines)

    footer_fp <- officer::fp_text(
      font.size   = max(fontsize, 8),
      font.family = if (!is.null(sp) && !is.null(sp$font_en)) sp$font_en else "Times New Roman",
      bold        = FALSE
    )
    ft <- flextable::style(ft, part = "footer", pr_t = footer_fp)
  }

  # 9d. 表格宽度处理
  if (!is.null(oneline)) {
    content_w_in <- .ft_content_width()
    col_w_in <- content_w_in * 0.98 / num_vars
    ft <- set_table_properties(ft, layout = "fixed", align = "left")
    for (i in seq_len(num_vars)) {
      ft <- width(ft, j = var_names[i], width = col_w_in)
    }
  } else {
    ft <- set_table_properties(ft, layout = "autofit", width = 1, align = "left")
  }

  # 9e. 加粗指定行
  if (!is.null(bold_rows) && length(bold_rows) > 0) {
    ft <- flextable::bold(ft, i = bold_rows, part = "body")
  }

  # ================================================================
  # Step 10：写入 Word 文档
  # ================================================================
  if (write_to_doc && tryCatch(.doc_ctx_initialized(), error = function(e) FALSE)) {
    doc <- .doc_ctx_get_doc()

    # 10a. 落地上一次挂起的分节符
    .doc_ctx_flush_pending()

    # 10b. 写入表格标题
    if (nchar(full_title) > 0) {
      doc <- .doc_ctx_get_doc()
      doc <- .write_table_title(doc, full_title, level_int, fs)
    } else {
      doc <- .doc_ctx_get_doc()
    }

    # 10c. 写入 flextable（移除 footer 避免重复）
    ft_word <- ft
    if (!is.null(footnote) && nchar(trimws(as.character(footnote))) > 0) {
      ft_word <- flextable::delete_part(ft_word, part = "footer")
    }
    doc <- flextable::body_add_flextable(doc, value = ft_word)
    .doc_ctx_set_doc(doc)

    # 10c-补. 脚注写入 Word 独立段落
    if (!is.null(footnote) && nchar(trimws(as.character(footnote))) > 0) {
      fn_processed <- .process_footnote(footnote)
      fn_lines_doc <- strsplit(as.character(fn_processed), "\n", fixed = TRUE)[[1]]
      fn_lines_doc <- fn_lines_doc[nchar(trimws(fn_lines_doc)) > 0]

      if (length(fn_lines_doc) > 0) {
        sp_fn      <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
        fn_font_cn <- if (!is.null(sp_fn)) sp_fn$font_cn else "宋体"
        fn_font_en <- if (!is.null(sp_fn)) sp_fn$font_en else "Times New Roman"
        fn_sz_val  <- as.integer(round(fontsize * 2))

        runs_xml <- ""
        for (k in seq_along(fn_lines_doc)) {
          line_esc <- .xml_escape(fn_lines_doc[k])
          br_xml   <- if (k > 1) '<w:br w:type="textWrapping"/>' else ""
          runs_xml <- paste0(
            runs_xml, br_xml,
            sprintf(
              '<w:r>
                <w:rPr>
                  <w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/>
                  <w:sz w:val="%d"/>
                  <w:szCs w:val="%d"/>
                </w:rPr>
                <w:t xml:space="preserve">%s</w:t>
              </w:r>',
              fn_font_en, fn_font_cn, fn_font_en, fn_font_en,
              fn_sz_val, fn_sz_val, line_esc
            )
          )
        }

        fn_xml <- sprintf(
          '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
            <w:pPr>
              <w:jc w:val="left"/>
              <w:spacing w:before="20"/>
            </w:pPr>
            %s
          </w:p>',
          runs_xml
        )

        doc <- .doc_ctx_get_doc()
        doc <- officer::body_add_xml(doc, str = fn_xml, pos = "after")
        .doc_ctx_set_doc(doc)
      }
    }

    # 10d. 挂起分节符
    .doc_ctx_set_pending()
    .doc_ctx_add_table()
  }

  return(invisible(ft))
}
