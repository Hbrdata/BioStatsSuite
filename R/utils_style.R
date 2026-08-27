# =============================================================================
# utils_style.R
# 报告文档统一样式定义，供表格和文档输出使用。
#
# 对应原始文件: Function/style.R
# 主要变更: .ods_get_style() → .doc_ctx_get_style()（包级别环境）
#
# 函数清单:
#   hbr3.style()          - 生成样式属性列表（字体/边框/页边距等）
#   .apply_style_to_ft()  - 将 hbr3 样式应用到 flextable
#   .ft_content_width()   - 计算表格内容区可用宽度（英寸）
# =============================================================================

#' 生成 hbr3 样式属性列表
#'
#' 对应 SAS proc template define style Styles.hbr3
#'
#' @param font_size 字号（磅），默认 10.5
#' @param font_cn   中文字体，默认 "等线"
#' @param font_en   西文字体，默认 "Times New Roman"
#' @return 命名 list，包含字体/边框/页边距/页面尺寸等样式参数
#' @noRd
hbr3_style <- function(font_size = 10.5,
                        font_cn   = "等线",
                        font_en   = "Times New Roman") {

  header_fp <- officer::fp_text(
    bold            = TRUE,
    font.size       = font_size,
    font.family     = font_en,
    eastasia.family = font_cn,
    cs.family       = font_en
  )

  body_fp <- officer::fp_text(
    bold            = FALSE,
    font.size       = font_size,
    font.family     = font_en,
    eastasia.family = font_cn,
    cs.family       = font_en
  )

  footer_fp <- officer::fp_text(
    bold            = FALSE,
    font.size       = max(font_size - 1, 8),
    font.family     = font_en,
    eastasia.family = font_cn,
    cs.family       = font_en
  )

  border_thick <- officer::fp_border(color = "black", width = 1.5)
  border_thin  <- officer::fp_border(color = "black", width = 0.75)
  border_none  <- officer::fp_border(color = "transparent", width = 0)

  page_mar <- officer::page_mar(
    top    = 2.0 / 2.54,
    bottom = 1.5 / 2.54,
    left   = 2.5 / 2.54,
    right  = 2.0 / 2.54,
    header = 2.0 / 2.54,
    footer = 1.5 / 2.54
  )

  page_size_land <- officer::page_size(orient = "landscape",
                                        width  = 297 / 25.4,
                                        height = 210 / 25.4)
  page_size_port <- officer::page_size(orient = "portrait",
                                        width  = 210 / 25.4,
                                        height = 297 / 25.4)

  list(
    font_size      = font_size,
    font_cn        = font_cn,
    font_en        = font_en,
    border_thick   = border_thick,
    border_thin    = border_thin,
    border_none    = border_none,
    cell_padding   = 0,
    header_fp      = header_fp,
    body_fp        = body_fp,
    footer_fp      = footer_fp,
    page_mar       = page_mar,
    page_size_land = page_size_land,
    page_size_port = page_size_port
  )
}


#' 计算表格内容区可用宽度（英寸）
#'
#' 根据当前文档上下文中的 orientation 自动选择横/纵向页面，
#' 内容区宽度 = 页面宽 - 左边距 - 右边距。
#'
#' @param orientation 页面方向字符串，NULL 时从上下文自动读取
#' @return 数值，内容区可用宽度（英寸）
#' @noRd
.ft_content_width <- function(orientation = NULL) {
  sp <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)

  if (is.null(orientation)) {
    orientation <- .doc_ctx_get_orientation()
  }
  is_landscape <- toupper(trimws(orientation)) == "LANDSCAPE"

  if (!is.null(sp)) {
    page_size <- if (is_landscape) sp$page_size_land else sp$page_size_port
    page_w    <- page_size$width
    mar_left  <- sp$page_mar$left
    mar_right <- sp$page_mar$right
  } else {
    page_w    <- if (is_landscape) 297 / 25.4 else 210 / 25.4
    mar_left  <- 25 / 25.4
    mar_right <- 20 / 25.4
  }

  page_w - mar_left - mar_right
}


#' 将 hbr3 样式应用到 flextable（不含宽度）
#'
#' 对应 SAS proc template 中的三线表、表头加粗、字体分槽等设置。
#' 宽度由 report_table() 单独处理。
#'
#' @param ft         flextable 对象
#' @param sp         hbr3_style() 返回的列表；NULL 时自动调用 hbr3_style()
#' @param headerjust 表头对齐，默认 "left"
#' @param col1just   第 1 列对齐，默认 "left"
#' @param bodyjust   第 2 列及以后对齐，默认 "left"
#' @return 已应用样式的 flextable（列宽未处理）
#' @noRd
.apply_style_to_ft <- function(ft,
                                sp         = NULL,
                                headerjust = "left",
                                col1just   = "left",
                                bodyjust   = "left") {

  if (is.null(sp)) sp <- hbr3_style()
  ncols <- length(ft$col_keys)

  # 三线表边框
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,    part = "header", border = sp$border_thick)
  ft <- flextable::hline_bottom(ft, part = "header", border = sp$border_thin)
  ft <- flextable::hline_bottom(ft, part = "body",   border = sp$border_thick)

  # 表头字体
  ft <- flextable::style(ft, part = "header",
    pr_t = officer::fp_text(
      bold            = TRUE,
      font.size       = sp$font_size,
      font.family     = sp$font_en,
      eastasia.family = sp$font_cn,
      cs.family       = sp$font_en
    ))
  ft <- flextable::align(ft,  part = "header", align = headerjust)
  ft <- flextable::valign(ft, part = "header", valign = "bottom")

  # 正文字体
  ft <- flextable::style(ft, part = "body",
    pr_t = officer::fp_text(
      bold            = FALSE,
      font.size       = sp$font_size,
      font.family     = sp$font_en,
      eastasia.family = sp$font_cn,
      cs.family       = sp$font_en
    ))
  ft <- flextable::align(ft, j = 1,       part = "body", align = col1just)
  if (ncols >= 2)
    ft <- flextable::align(ft, j = 2:ncols, part = "body", align = bodyjust)

  # 脚注字体
  ft <- flextable::style(ft, part = "footer",
    pr_t = officer::fp_text(
      bold            = FALSE,
      font.size       = max(sp$font_size - 1, 8),
      font.family     = sp$font_en,
      eastasia.family = sp$font_cn,
      cs.family       = sp$font_en
    ))
  ft <- flextable::align(ft, part = "footer", align = "left")

  # 单元格内边距
  ft <- flextable::padding(ft, padding = 1, part = "all")

  ft
}
