# =============================================================================
# utils_reporttitle.R
# 向 Word 文档追加章节标题，并重置全局序号变量。
#
# 对应原始文件: Function/reporttitle.R
# 主要变更:
#   .GlobalEnv$pre_title     → .doc_ctx_set_pre_title()
#   .GlobalEnv$inner_num     → .doc_ctx_set_inner_num()
#   .GlobalEnv$pic_inner_num → .doc_ctx_set_pic_inner_num()
#   .ods_initialized()       → .doc_ctx_initialized()
#   .ods_get_style()         → .doc_ctx_get_style()
#   .ods_get_doc()           → .doc_ctx_get_doc()
#   .ods_set_doc()           → .doc_ctx_set_doc()
#   .ods_flush_pending_section() → .doc_ctx_flush_pending()
#   .ods_set_pending_section()   → .doc_ctx_set_pending()
# =============================================================================

#' 向 Word 文档追加章节标题，并重置序号变量
#'
#' @param title  标题文字（含数字前缀，前缀与文字间空格分隔）
#'               例："1.2.2 SF-36 量表评分"
#' @param fs     字体大小（磅），默认 10.5（五号）
#' @param level  大纲级别（0~9），默认 1；0=普通段落（不带大纲）
#' @param bold   是否加粗：0=否（默认），1=是
#' @noRd
reporttitle <- function(title,
                        fs    = 10.5,
                        level = 1,
                        bold  = 0) {

  if (!requireNamespace("officer", quietly = TRUE))
    stop("请安装 officer 包：install.packages('officer')")

  if (!.doc_ctx_initialized()) {
    message("reporttitle: 请先调用 odsrtf() 初始化文档。")
    return(invisible(NULL))
  }

  # 1. 维护序号变量
  parts         <- strsplit(trimws(as.character(title)), "\\s+")[[1]]
  pre_title_val <- if (length(parts) >= 1) parts[1] else ""

  .doc_ctx_set_pre_title(pre_title_val)
  .doc_ctx_set_inner_num(0L)
  .doc_ctx_set_pic_inner_num(0L)

  # 2. 字体
  sp      <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
  font_cn <- if (!is.null(sp)) sp$font_cn else "等线"
  font_en <- if (!is.null(sp)) sp$font_en else "Times New Roman"

  # 3. 构建 Word 段落 XML
  level_int  <- as.integer(level)
  sz_val     <- as.integer(round(fs * 2))
  title_esc  <- .xml_escape(as.character(title))
  bold_xml   <- if (as.integer(bold) == 1L) "<w:b/>" else ""

  if (level_int > 0L) {
    outline_xml <- sprintf('<w:outlineLvl w:val="%d"/>', level_int - 1L)
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
          %s
        </w:rPr>
        <w:t xml:space="preserve">%s</w:t>
      </w:r>
    </w:p>',
    outline_xml,
    font_en, font_cn, font_en, font_en,
    sz_val, sz_val,
    bold_xml,
    title_esc
  )

  # 4. 写入文档（延迟分节符机制）
  .doc_ctx_flush_pending()

  doc <- .doc_ctx_get_doc()
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_xml(doc, str = xml_str, pos = "on")
  .doc_ctx_set_doc(doc)

  .doc_ctx_set_pending(n_blank = 1L)

  message(sprintf("******* 正在输出标题：%s ********", title))
  return(invisible(NULL))
}
