# =============================================================================
# utils_odsrtf.R
# Word 文档初始化、执行报告脚本、保存文件。
#
# 对应原始文件: Function/odsrtf.R
# 主要变更:
#   .GlobalEnv$.ods_ctx (environment) → .doc_ctx (包级别环境)
#   所有 .ods_* 辅助函数 → .doc_ctx_* 函数
#
# 函数清单:
#   odsrtf()           - 主函数：初始化文档 → 执行脚本 → 保存
#   ods_orientation()  - 运行时切换页面方向
# =============================================================================

# ---- 内部辅助：依赖检查 ----

.check_officer <- function() {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("请安装 officer 包: install.packages('officer')")
}

# ---- 内部辅助：构建页眉页脚用的字体属性 ----

.make_hf_fp <- function(font_size = 10.5,
                         font_cn   = "等线",
                         font_en   = "Times New Roman") {
  sp <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
  if (!is.null(sp)) {
    font_size <- sp$font_size
    font_cn   <- sp$font_cn
    font_en   <- sp$font_en
  }
  officer::fp_text(
    font.size       = font_size,
    font.family     = font_en,
    eastasia.family = font_cn,
    cs.family       = font_en
  )
}

# ---- 内部辅助：构建页眉 ----

.make_header <- function(project, sponsor, title, version, font = "等线") {
  today_str <- format(Sys.Date(), "%Y%m%d")
  fp        <- .make_hf_fp(font_size = 10.5)

  line1 <- officer::fpar(
    officer::ftext(project, fp),
    officer::ftext("\t", fp),
    officer::ftext(title, fp),
    fp_p = officer::fp_par(
      text.align = "left",
      tabs = officer::fp_tabs(officer::fp_tab(pos = 22860, style = "right"))
    )
  )

  line2 <- officer::fpar(
    officer::ftext(sponsor, fp),
    officer::ftext("\t", fp),
    officer::ftext(paste0(version, "/", today_str), fp),
    fp_p = officer::fp_par(
      text.align = "left",
      tabs = officer::fp_tabs(officer::fp_tab(pos = 22860, style = "right"))
    )
  )

  line_space <- officer::fpar(fp_p = officer::fp_par(padding.bottom = 0))
  officer::block_list(line1, line2, line_space)
}

# ---- 内部辅助：构建页脚 ----

.make_footer <- function(company = "海博瑞（北京）数据科技有限公司",
                         font    = "等线") {
  fp <- .make_hf_fp(font_size = 10.5)

  ftr_line <- officer::fpar(
    officer::ftext(company, fp),
    officer::ftext("\t", fp),
    officer::ftext("第 ", fp),
    officer::run_word_field("PAGE", prop = fp),
    officer::ftext(" 页 共 ", fp),
    officer::run_word_field("NUMPAGES", prop = fp),
    officer::ftext(" 页", fp),
    fp_p = officer::fp_par(
      text.align = "left",
      tabs = officer::fp_tabs(officer::fp_tab(pos = 22860, style = "right"))
    )
  )

  officer::block_list(ftr_line)
}

# ---- 内部辅助：构建 prop_section ----

.make_prop_sec <- function(orientation, page_mar_val,
                            project, sponsor, title, version,
                            font, company, type = "continuous") {
  sp <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)

  is_portrait <- toupper(trimws(orientation)) != "LANDSCAPE"

  page_size_val <- if (!is.null(sp)) {
    if (is_portrait) sp$page_size_port else sp$page_size_land
  } else {
    officer::page_size(
      orient = if (is_portrait) "portrait" else "landscape",
      width  = if (is_portrait) 210 / 25.4 else 297 / 25.4,
      height = if (is_portrait) 297 / 25.4 else 210 / 25.4
    )
  }

  officer::prop_section(
    page_size      = page_size_val,
    page_margins   = page_mar_val,
    type           = type,
    header_default = .make_header(project, sponsor, title, version, font),
    footer_default = .make_footer(company, font)
  )
}


# ---- 公开函数：ods_orientation ----

#' 运行时切换页面方向
#'
#' 在报告脚本中调用，切换后续内容的页面方向。
#' 对应 SAS: OPTIONS ORIENTATION = LANDSCAPE/PORTRAIT;
#'
#' @param orientation "PORTRAIT"（默认）或 "LANDSCAPE"
#' @noRd
ods_orientation <- function(orientation = "PORTRAIT") {
  if (!.doc_ctx_initialized()) {
    warning("ods_orientation: 请先调用 odsrtf() 初始化文档。")
    return(invisible(NULL))
  }

  orientation_norm <- if (toupper(trimws(orientation)) == "PORTRAIT") "PORTRAIT" else "LANDSCAPE"

  # 落地上一次挂起的分节符（使用旧方向）
  .doc_ctx_flush_pending()

  # 读取页眉页脚参数
  params <- .doc_ctx_get_params()

  new_prop_sec <- .make_prop_sec(
    orientation  = orientation_norm,
    page_mar_val = params$page_mar_val,
    project      = params$project,
    sponsor      = params$sponsor,
    title        = params$title,
    version      = params$version,
    font         = params$font,
    company      = params$company,
    type         = "continuous"
  )

  .doc_ctx_set_orientation(orientation_norm)
  .doc_ctx_set_prop_sec(new_prop_sec)

  message(sprintf("页面方向已切换为: %s", orientation_norm))
  invisible(NULL)
}


# ---- 主函数：odsrtf ----

#' 初始化报告文档，执行报告脚本，保存 Word 文件
#'
#' @param output      输出文件名（不含 .docx 后缀）
#' @param program     报告 R 脚本名（不含 .R 后缀）
#' @param project     项目名称（页眉第1行左侧）
#' @param sponsor     申办方（页眉第2行左侧）
#' @param title       报告标题（页眉第1行右侧）
#' @param version     版本号（页眉第2行右侧）
#' @param outdir      输出根目录
#' @param program_dir 报告脚本所在目录
#' @param company     页脚左侧公司名
#' @param orientation 默认页面方向 "PORTRAIT"
#' @param font        中文字体，默认 "宋体"
#' @param style       hbr3_style() 返回值，NULL 时自动使用默认样式
#' @return 输出文件完整路径（不可见）
#' @noRd
odsrtf <- function(output,
                   program      = "",
                   project      = "",
                   sponsor      = "",
                   title        = "",
                   version      = "",
                   outdir       = NULL,
                   program_dir  = NULL,
                   company      = "海博瑞（北京）数据科技有限公司",
                   orientation  = "PORTRAIT",
                   font         = "宋体",
                   style        = NULL) {

  .check_officer()

  # 输出名兜底：避免空名生成 ".docx"
  output <- .sanitize_filename(output)

  # 记录开始时间
  time_start     <- proc.time()["elapsed"]
  time_start_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # 初始化包级别上下文
  .doc_ctx_init()
  .doc_ctx$.doc         <- officer::read_docx()
  .doc_ctx$orientation  <- orientation

  # 存储页眉页脚参数
  page_mar_val <- if (!is.null(style)) {
    style$page_mar
  } else {
    officer::page_mar(top=2.0/2.54, bottom=1.5/2.54, left=2.5/2.54, right=2.0/2.54,
                      header=2.0/2.54, footer=1.5/2.54)
  }

  .doc_ctx_set_params(project, sponsor, title, version, font, company, page_mar_val)

  # 存储样式
  .doc_ctx_set_style(if (!is.null(style)) {
    style
  } else if (exists("hbr3_style", mode = "function")) {
    hbr3_style()
  } else {
    NULL
  })

  # 构建 prop_section
  prop_sec <- .make_prop_sec(
    orientation  = orientation,
    page_mar_val = page_mar_val,
    project      = project,
    sponsor      = sponsor,
    title        = title,
    version      = version,
    font         = font,
    company      = company,
    type         = "continuous"
  )
  .doc_ctx_set_prop_sec(prop_sec)

  # 设置文档默认节
  .doc_ctx$.doc <- officer::body_set_default_section(.doc_ctx$.doc, prop_sec)

  # ---- 创建输出目录 ----
  if (is.null(outdir)) {
    outdir <- tryCatch(path_outlib, error = function(e) NULL)
  }
  if (is.null(outdir)) {
    outdir <- file.path(dirname(getwd()), "TFLs")
    message("path_outlib 未定义，使用默认路径: ", outdir)
  }

  parent_dir <- dirname(outdir)
  tfl_dir    <- file.path(parent_dir, "TFLs")
  if (!dir.exists(tfl_dir)) dir.create(tfl_dir, recursive = TRUE)

  if (normalizePath(outdir, winslash = "/", mustWork = FALSE) !=
      normalizePath(tfl_dir, winslash = "/", mustWork = FALSE)) {
    outdir <- file.path(tfl_dir, basename(outdir))
  } else {
    outdir <- tfl_dir
  }
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  today_str <- format(Sys.Date(), "%Y%m%d")
  dated_dir <- file.path(outdir, today_str)
  if (!dir.exists(dated_dir)) dir.create(dated_dir, recursive = TRUE)

  # ---- 执行报告脚本 ----
  if (!is.null(program) && nchar(trimws(program)) > 0) {
    pdir <- if (!is.null(program_dir)) program_dir else outdir

    candidates  <- unique(c(
      file.path(pdir,   paste0(program, ".R")),
      file.path(outdir, paste0(program, ".R")),
      paste0(program, ".R")
    ))
    script_path <- Filter(file.exists, candidates)[1]

    if (length(script_path) == 0 || is.na(script_path)) {
      warning(sprintf(
        "报告脚本未找到: %s.R\n  已搜索路径: %s\n  请通过 program_dir 参数指定脚本目录。",
        program, paste(candidates, collapse = "\n  ")
      ))
    } else {
      message(sprintf("正在执行报告脚本: %s", script_path))
      exec_result <- tryCatch({
        source(script_path, local = FALSE, encoding = "UTF-8")
        TRUE
      }, error = function(e) {
        message(sprintf("\n[ERROR] 报告脚本执行失败: %s", e$message))
        FALSE
      })
      if (!exec_result) {
        warning("由于脚本运行错误，导出的文档可能不完整。")
      }
    }
  }

  # ---- 收尾：丢弃最后一个挂起的分节符 ----
  .doc_ctx_flush_pending(flush = FALSE)

  # ---- 取出最终文档及计数 ----
  final_doc <- .doc_ctx$.doc
  n_table   <- .doc_ctx_get_num_table()
  n_chart   <- .doc_ctx_get_num_chart()

  # ---- 保存文件 ----
  out_file <- file.path(dated_dir, paste0(output, ".docx"))
  print(final_doc, target = out_file)

  # ---- 清理上下文 ----
  .doc_ctx_clear()

  # ---- 日志 ----
  time_end     <- proc.time()["elapsed"]
  time_end_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  elapsed_sec  <- round(time_end - time_start, 1)

  msg <- paste0(
    "\n-------------------------------------------------------------------------\n",
    "\n*********** 开始于 ", time_start_str, " ***********\n",
    "\n*********** 完成于 ", time_end_str,   " ***********\n",
    "\n*********** 总计耗时约 ", elapsed_sec, " 秒 ***********\n",
    "\n*********** 生成表格：", n_table, " 个  生成统计图：", n_chart, " 个 ***********\n",
    "\n*********** 生成 docx 文档：", output, " ***********\n",
    "\n-------------------------------------------------------------------------"
  )
  message(msg)

  invisible(out_file)
}


# ---- 拆分版 API：供 Shiny 模块调用 ----

#' Initialize document context without executing a script
#'
#' Same as odsrtf() but stops after initialization. Caller can then
#' programmatically call reporttitle() / report_table() before saving
#' with odsrtf_save().
#'
#' @inheritParams odsrtf
#' @return Output directory path (invisibly)
#' @noRd
odsrtf_init <- function(output,
                        project     = "",
                        sponsor     = "",
                        title       = "",
                        version     = "",
                        outdir      = NULL,
                        company     = "海博瑞（北京）数据科技有限公司",
                        orientation = "PORTRAIT",
                        font        = "宋体",
                        style       = NULL) {

  .check_officer()

  # 初始化包级别上下文
  .doc_ctx_init()
  .doc_ctx$.doc        <- officer::read_docx()
  .doc_ctx$orientation <- orientation

  # 存储页眉页脚参数
  page_mar_val <- if (!is.null(style)) {
    style$page_mar
  } else {
    officer::page_mar(top=2.0/2.54, bottom=1.5/2.54, left=2.5/2.54, right=2.0/2.54,
                      header=2.0/2.54, footer=1.5/2.54)
  }

  .doc_ctx_set_params(project, sponsor, title, version, font, company, page_mar_val)

  # 存储样式
  .doc_ctx_set_style(if (!is.null(style)) {
    style
  } else if (exists("hbr3_style", mode = "function")) {
    hbr3_style()
  } else {
    NULL
  })

  # 构建 prop_section
  prop_sec <- .make_prop_sec(
    orientation  = orientation,
    page_mar_val = page_mar_val,
    project      = project,
    sponsor      = sponsor,
    title        = title,
    version      = version,
    font         = font,
    company      = company,
    type         = "continuous"
  )
  .doc_ctx_set_prop_sec(prop_sec)

  # 设置文档默认节
  .doc_ctx$.doc <- officer::body_set_default_section(.doc_ctx$.doc, prop_sec)

  # 创建输出目录
  if (is.null(outdir)) {
    outdir <- tryCatch(path_outlib, error = function(e) NULL)
  }
  if (is.null(outdir)) {
    outdir <- file.path(dirname(getwd()), "TFLs")
  }

  # Store outdir and output name for odsrtf_save()
  .doc_ctx$outdir  <- outdir
  .doc_ctx$output  <- .sanitize_filename(output)

  message(sprintf("文档已初始化: %s (方向: %s)", output, orientation))
  invisible(outdir)
}


#' Save the document and clean up context
#'
#' Must be called after odsrtf_init() + reporttitle()/report_table() calls.
#'
#' @return Output .docx file path (invisibly), or NULL if not initialized
#' @noRd
odsrtf_save <- function() {
  if (!.doc_ctx_initialized()) {
    warning("odsrtf_save: 文档未初始化，请先调用 odsrtf_init()。")
    return(invisible(NULL))
  }

  # 丢弃最后一个挂起的分节符
  .doc_ctx_flush_pending(flush = FALSE)

  # 取出文档和计数
  final_doc <- .doc_ctx$.doc
  n_table   <- .doc_ctx_get_num_table()
  n_chart   <- .doc_ctx_get_num_chart()
  output    <- .sanitize_filename(.doc_ctx$output)
  outdir    <- .doc_ctx$outdir %||% tempdir()

  # 直接使用 outdir（由 odsrtf_init 传入），不做 TFLs 重组
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # 保存文件
  out_file <- file.path(outdir, paste0(output, ".docx"))
  print(final_doc, target = out_file)

  # 清理上下文
  .doc_ctx_clear()

  message(sprintf("报告已保存: %s (表格: %d, 图形: %d)", out_file, n_table, n_chart))
  invisible(out_file)
}
