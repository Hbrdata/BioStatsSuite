# =============================================================================
# utils_figtitle.R
# 图形标题自动生成，配合绘图函数使用。
#
# 对应原始文件: Function/figtitle.R
# 主要变更:
#   .GlobalEnv$pre_title     → .doc_ctx_get/set_pre_title()
#   .GlobalEnv$pic_inner_num → .doc_ctx_get/set_pic_inner_num()
#
# 函数清单:
#   figtitle_reset()       - 重置图编号计数器
#   figtitle_set_prefix()  - 设置章节前缀编号
#   figtitle()             - 核心函数：自动编号 + 拼接图标题
#   print.figtitle()       - S3 打印方法
# =============================================================================

#' 重置图编号计数器至指定值
#' @param n 整数，默认 0
#' @noRd
figtitle_reset <- function(n = 0L) {
  .doc_ctx_set_pic_inner_num(as.integer(n))
  invisible(NULL)
}

#' 设置当前报告章节的前缀编号
#' @param prefix 字符串，如 "1.1"
#' @noRd
figtitle_set_prefix <- function(prefix = "") {
  .doc_ctx_set_pre_title(as.character(prefix))
  invisible(NULL)
}

#' 自动生成图标题（自动编号 + 拼接）
#'
#' @param title       图标题文字
#' @param fs          字体大小（磅），默认 10.5
#' @param level       大纲级别，默认 7
#' @param bold        是否加粗，默认 FALSE
#' @param autonum     是否自动编号，默认 TRUE
#' @param print_title 是否打印到控制台，默认 TRUE
#' @return class="figtitle" 的 list，包含 $label/$fs/$bold/$level/$num
#' @noRd
figtitle <- function(title,
                     fs          = 10.5,
                     level       = 7,
                     bold        = FALSE,
                     autonum     = TRUE,
                     print_title = TRUE) {

  # 读取前缀和当前编号
  prefix      <- .doc_ctx_get_pre_title()
  current_num <- .doc_ctx_get_pic_inner_num()

  # 自动递增
  if (isTRUE(autonum)) {
    current_num <- current_num + 1L
    .doc_ctx_set_pic_inner_num(current_num)
  }

  # 拼接标题
  if (isTRUE(autonum)) {
    if (nchar(prefix) > 0) {
      label <- paste0("图", prefix, ".", current_num, " ", title)
    } else {
      label <- paste0("图", current_num, " ", title)
    }
  } else {
    label <- as.character(title)
  }

  if (isTRUE(print_title)) {
    message("******* 正在输出图：", label, " *******")
  }

  structure(
    list(
      label = label,
      fs    = fs,
      bold  = bold,
      level = level,
      num   = if (isTRUE(autonum)) current_num else NA_integer_
    ),
    class = "figtitle"
  )
}

#' S3 print 方法：控制台友好显示 figtitle 对象
#' @export
#' @noRd
print.figtitle <- function(x, ...) {
  cat("图标题  :", x$label,  "\n")
  cat("字号    :", x$fs, "pt\n")
  cat("加粗    :", if (isTRUE(x$bold)) "是" else "否", "\n")
  cat("大纲级别:", x$level, "\n")
  if (!is.na(x$num)) cat("当前编号:", x$num, "\n")
  invisible(x)
}
