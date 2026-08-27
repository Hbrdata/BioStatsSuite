# =============================================================================
# utils_doc_context.R
# 包级别文档状态管理器，替代 Function/ 中直接操作 .GlobalEnv 的方式。
#
# 提供统一的上下文读写接口，供 utils_odsrtf / utils_reporttable /
# utils_reporttitle / utils_figtitle / utils_style 共同调用。
#
# Shiny 多会话安全：所有状态存储在包内部环境 .doc_ctx 中，
# 不污染用户的全局环境。
#
# 对应原始文件: Function/odsrtf.R 中的 .ods_ctx 相关逻辑
# =============================================================================

# 包级别环境（不可被用户意外覆盖）
.doc_ctx <- new.env(parent = emptyenv())

# ---- 初始化 / 清理 ----

#' 初始化文档上下文（调用 odsrtf 时执行）
#' @noRd
.doc_ctx_init <- function() {
  rm(list = ls(.doc_ctx, all.names = TRUE), envir = .doc_ctx)
  .doc_ctx$.doc                <- NULL
  .doc_ctx$num_table           <- 0L
  .doc_ctx$num_chart           <- 0L
  .doc_ctx$pending_section     <- FALSE
  .doc_ctx$pending_blank_lines <- 2L
  .doc_ctx$orientation         <- "PORTRAIT"
  .doc_ctx$prop_sec            <- NULL
  .doc_ctx$style               <- NULL
  .doc_ctx$pre_title           <- ""
  .doc_ctx$inner_num           <- 0L
  .doc_ctx$pic_inner_num       <- 0L
  invisible(NULL)
}

#' 清理文档上下文（文档保存后执行）
#' @noRd
.doc_ctx_clear <- function() {
  rm(list = ls(.doc_ctx, all.names = TRUE), envir = .doc_ctx)
  invisible(NULL)
}

#' 检查上下文是否已初始化
#' @noRd
.doc_ctx_initialized <- function() {
  exists(".doc", envir = .doc_ctx) && !is.null(.doc_ctx$.doc)
}

# ---- 文档对象读写 ----

.doc_ctx_get_doc <- function() {
  if (exists(".doc", envir = .doc_ctx)) .doc_ctx$.doc else NULL
}

.doc_ctx_set_doc <- function(doc) {
  .doc_ctx$.doc <- doc
  invisible(NULL)
}

# ---- 输出计数 ----

.doc_ctx_add_table <- function() {
  .doc_ctx$num_table <- .doc_ctx$num_table + 1L
  invisible(NULL)
}

.doc_ctx_add_chart <- function() {
  .doc_ctx$num_chart <- .doc_ctx$num_chart + 1L
  invisible(NULL)
}

.doc_ctx_get_num_table <- function() {
  if (exists("num_table", envir = .doc_ctx)) .doc_ctx$num_table else 0L
}

.doc_ctx_get_num_chart <- function() {
  if (exists("num_chart", envir = .doc_ctx)) .doc_ctx$num_chart else 0L
}

# ---- 样式读写 ----

.doc_ctx_get_style <- function() {
  if (exists("style", envir = .doc_ctx)) .doc_ctx$style else NULL
}

.doc_ctx_set_style <- function(style) {
  .doc_ctx$style <- style
  invisible(NULL)
}

# ---- 分节属性读写 ----

.doc_ctx_get_prop_sec <- function() {
  if (exists("prop_sec", envir = .doc_ctx)) .doc_ctx$prop_sec else NULL
}

.doc_ctx_set_prop_sec <- function(prop_sec) {
  .doc_ctx$prop_sec <- prop_sec
  invisible(NULL)
}

# ---- 页面方向读写 ----

.doc_ctx_get_orientation <- function() {
  if (exists("orientation", envir = .doc_ctx)) .doc_ctx$orientation else "PORTRAIT"
}

.doc_ctx_set_orientation <- function(orientation) {
  .doc_ctx$orientation <- orientation
  invisible(NULL)
}

# ---- 页眉页脚参数读写 ----

.doc_ctx_get_params <- function() {
  fields <- c("project", "sponsor", "title", "version", "font", "company", "page_mar_val")
  params <- list()
  for (f in fields) {
    params[[f]] <- if (exists(f, envir = .doc_ctx)) .doc_ctx[[f]] else NULL
  }
  params
}

.doc_ctx_set_params <- function(project, sponsor, title, version,
                                 font, company, page_mar_val) {
  .doc_ctx$project      <- project
  .doc_ctx$sponsor      <- sponsor
  .doc_ctx$title        <- title
  .doc_ctx$version      <- version
  .doc_ctx$font         <- font
  .doc_ctx$company      <- company
  .doc_ctx$page_mar_val <- page_mar_val
  invisible(NULL)
}

# ---- 自动编号变量读写 ----

.doc_ctx_get_pre_title <- function() {
  if (exists("pre_title", envir = .doc_ctx)) as.character(.doc_ctx$pre_title) else ""
}

.doc_ctx_set_pre_title <- function(val) {
  .doc_ctx$pre_title <- as.character(val)
  invisible(NULL)
}

.doc_ctx_get_inner_num <- function() {
  if (exists("inner_num", envir = .doc_ctx)) as.integer(.doc_ctx$inner_num) else 0L
}

.doc_ctx_set_inner_num <- function(val) {
  .doc_ctx$inner_num <- as.integer(val)
  invisible(NULL)
}

.doc_ctx_get_pic_inner_num <- function() {
  if (exists("pic_inner_num", envir = .doc_ctx)) as.integer(.doc_ctx$pic_inner_num) else 0L
}

.doc_ctx_set_pic_inner_num <- function(val) {
  .doc_ctx$pic_inner_num <- as.integer(val)
  invisible(NULL)
}

# ---- 延迟分节符机制 ----

#' 挂起分节符意图（写完表格/标题后调用）
#' @param n_blank 分节符前空行数，表格/图形传 2，标题传 1
#' @noRd
.doc_ctx_set_pending <- function(n_blank = 2L) {
  .doc_ctx$pending_section     <- TRUE
  .doc_ctx$pending_blank_lines <- as.integer(n_blank)
  invisible(NULL)
}

#' 落地或丢弃挂起的分节符
#' @param flush TRUE=落地写入分节符；FALSE=仅清除标志（收尾丢弃）
#' @noRd
.doc_ctx_flush_pending <- function(flush = TRUE) {
  if (!.doc_ctx_initialized()) return(invisible(NULL))

  pending <- isTRUE(.doc_ctx$pending_section)
  if (!pending) return(invisible(NULL))

  n_blank <- if (exists("pending_blank_lines", envir = .doc_ctx))
    as.integer(.doc_ctx$pending_blank_lines) else 2L

  # 无论落地还是丢弃，先清除标志
  .doc_ctx$pending_section     <- FALSE
  .doc_ctx$pending_blank_lines <- 2L

  if (flush) {
    doc      <- .doc_ctx$.doc
    prop_sec <- .doc_ctx_get_prop_sec()
    if (!is.null(prop_sec)) {
      for (i in seq_len(n_blank)) {
        doc <- officer::body_add_par(doc, value = "", style = "Normal")
      }
      doc <- officer::body_end_block_section(doc, officer::block_section(prop_sec))
    } else {
      doc <- officer::body_end_section_continuous(doc)
    }
    .doc_ctx$.doc <- doc
  }

  invisible(NULL)
}
