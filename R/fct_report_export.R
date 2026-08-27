# Report Export Helper Functions
# Functions for exporting analysis results to Word (.docx) using
# the odsrtf / reporttitle / report_table pipeline.

# ---- Flextable → data.frame 转换 ----

#' Extract body data and column labels from a flextable
#'
#' This is a transitional utility: existing analysis functions return flextable,
#' but report_table() needs data.frame + varlist. This function bridges the gap.
#'
#' @param ft A flextable object
#' @return A list with:
#'   $data   - data.frame of body content
#'   $labels - named character vector of column labels
#'   $varlist - formatted varlist string for report_table()
#' @noRd
extract_ft_data <- function(ft) {
  if (is.null(ft) || !inherits(ft, "flextable")) {
    message("extract_ft_data: 输入不是 flextable 对象")
    return(NULL)
  }

  # 提取 body 数据
  body_data <- tryCatch(ft$body$dataset, error = function(e) NULL)
  if (is.null(body_data)) {
    # 尝试从 body$content 提取
    body_data <- tryCatch({
      content <- ft$body$content
      if (!is.null(content) && length(content) > 0) {
        # content 是 list of rows, 每行是 list of cells
        # 尝试构建 data.frame
        n_cols <- length(ft$col_keys)
        n_rows <- length(content)
        mat <- matrix("", nrow = n_rows, ncol = n_cols)
        for (i in seq_len(n_rows)) {
          for (j in seq_len(min(n_cols, length(content[[i]])))) {
            cell <- content[[i]][[j]]
            mat[i, j] <- if (is.character(cell)) cell else as.character(cell)
          }
        }
        df <- as.data.frame(mat, stringsAsFactors = FALSE)
        names(df) <- ft$col_keys
        df
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }

  if (is.null(body_data) || nrow(body_data) == 0) {
    message("extract_ft_data: 无法提取 flextable body 数据")
    return(NULL)
  }

  col_names <- names(body_data)
  message("extract_ft_data: 列名 = ", paste(col_names, collapse = ", "))

  # 提取表头标签
  labels <- col_names  # 默认用列名
  names(labels) <- col_names

  tryCatch({
    hdr <- ft$header$dataset
    if (!is.null(hdr) && nrow(hdr) > 0) {
      last_row <- as.character(unlist(hdr[nrow(hdr), ]))
      if (length(last_row) == length(col_names)) {
        labels[] <- last_row
      }
    }
  }, error = function(e) {
    # 忽略，使用默认列名
  })

  # 填充空标签
  empty_idx <- is.na(labels) | nchar(trimws(labels)) == 0
  labels[empty_idx] <- col_names[empty_idx]

  # 构建 varlist
  varlist_parts <- paste0(col_names, "/", labels)
  varlist <- paste(varlist_parts, collapse = "|")
  message("extract_ft_data: varlist = ", varlist)

  list(
    data    = as.data.frame(body_data, stringsAsFactors = FALSE),
    labels  = labels,
    varlist = varlist
  )
}


#' Convert a single flextable to report_table output
#'
#' Extracts data from a flextable and re-renders it through report_table()
#' for consistent Word output styling.
#'
#' @param ft          flextable object
#' @param title       Table title (without number prefix)
#' @param footnote    Footnote text
#' @param write_to_doc Whether to write to Word document
#' @return flextable from report_table()
#' @noRd
ft_to_report_table <- function(ft, title = NULL, footnote = NULL, write_to_doc = TRUE) {
  # 优先使用分析函数附着的原始参数（避免 extract_ft_data 丢失表头标签）
  hbr_varlist  <- attr(ft, "hbr_varlist")
  hbr_title    <- attr(ft, "hbr_title")
  hbr_footnote <- attr(ft, "hbr_footnote")

  varlist  <- hbr_varlist  %||% NULL
  title    <- title        %||% hbr_title
  footnote <- footnote     %||% hbr_footnote

  if (is.null(varlist)) {
    # 降级：从 flextable 提取
    extracted <- extract_ft_data(ft)
    if (is.null(extracted)) {
      message("ft_to_report_table: 无法提取 flextable 数据，跳过")
      return(NULL)
    }
    var_names <- names(extracted$data)
    if (is.null(var_names) || length(var_names) == 0) {
      message("ft_to_report_table: data 无列名，跳过")
      return(NULL)
    }
    varlist <- extracted$varlist
    data    <- extracted$data
  } else {
    # 直接从 flextable body 提取 data
    data <- tryCatch(as.data.frame(ft$body$dataset, stringsAsFactors = FALSE),
                     error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0) {
      message("ft_to_report_table: 无法提取 flextable body，跳过")
      return(NULL)
    }
  }

  # 提取原始 report_table 样式参数（对齐、加粗行、多重表头等）
  styling_params <- attr(ft, "hbr_styling_params")

  tryCatch({
    do.call(report_table, c(list(
      data        = data,
      varlist     = varlist,
      title       = title,
      footnote    = footnote,
      write_to_doc = write_to_doc
    ), styling_params))
  }, error = function(e) {
    message("ft_to_report_table: report_table 失败: ", e$message)
    message("  列名: ", paste(names(data), collapse = ", "))

    # 降级：直接将原始 flextable 写入文档
    if (write_to_doc && .doc_ctx_initialized()) {
      message("  降级: 直接写入原始 flextable")
      .doc_ctx_flush_pending()
      doc <- .doc_ctx_get_doc()
      doc <- flextable::body_add_flextable(doc, value = ft)
      .doc_ctx_set_doc(doc)
      .doc_ctx_set_pending()
      .doc_ctx_add_table()
    }
    return(NULL)
  })
}


# ---- 图形导出到 Word ----

#' Write a ggplot figure to the Word document
#'
#' Uses figtitle() for auto-numbered title, saves plot as temp PNG,
#' then inserts title + image into the document via officer.
#'
#' @param plot_obj  ggplot object
#' @param title     Figure title text (without number prefix)
#' @param width     Image width in inches (default 6)
#' @param height    Image height in inches (default 4)
#' @param dpi       Image resolution (default 300)
#' @return NULL invisibly
#' @noRd
plot_to_docx <- function(plot_obj, title = NULL, width = 6, height = 4, dpi = 300) {
  if (is.null(plot_obj)) return(invisible(NULL))

  if (!.doc_ctx_initialized()) {
    message("plot_to_docx: 请先调用 odsrtf() 初始化文档。")
    return(invisible(NULL))
  }

  # 1. Generate auto-numbered title via figtitle()
  ft_obj   <- figtitle(title %||% "")
  ft_label <- ft_obj$label

  # 2. Save plot to temp PNG
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)

  tryCatch(
    ggplot2::ggsave(tmp_png, plot = plot_obj, width = width, height = height,
                    dpi = dpi, bg = "white"),
    error = function(e) {
      message("plot_to_docx: ggsave 失败: ", e$message)
      tmp_png <<- NULL
    }
  )
  if (is.null(tmp_png) || !file.exists(tmp_png)) return(invisible(NULL))

  # 3. Write title paragraph to document (same style as reporttitle)
  sp      <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)
  font_cn <- if (!is.null(sp)) sp$font_cn else "等线"
  font_en <- if (!is.null(sp)) sp$font_en else "Times New Roman"
  sz_val  <- as.integer(round(10.5 * 2))
  title_esc <- .xml_escape(as.character(ft_label))

  xml_str <- sprintf(
    '<w:p xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
      <w:pPr>
        <w:outlineLvl w:val="6"/>
        <w:jc w:val="left"/>
      </w:pPr>
      <w:r>
        <w:rPr>
          <w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/>
          <w:sz w:val="%d"/>
          <w:szCs w:val="%d"/>
          <w:b/>
        </w:rPr>
        <w:t xml:space="preserve">%s</w:t>
      </w:r>
    </w:p>',
    font_en, font_cn, font_en, font_en,
    sz_val, sz_val,
    title_esc
  )

  .doc_ctx_flush_pending()
  doc <- .doc_ctx_get_doc()
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_xml(doc, str = xml_str, pos = "on")
  doc <- officer::body_add_img(doc, src = tmp_png, width = width, height = height)
  .doc_ctx_set_doc(doc)

  .doc_ctx_set_pending(n_blank = 2L)
  .doc_ctx_add_chart()

  message(sprintf("******* 正在输出图：%s *******", ft_label))
  invisible(NULL)
}


# ---- 报告导出主函数 ----

#' Export selected analysis results to Word document
#'
#' Orchestrates the full pipeline:
#'   1. odsrtf() init
#'   2. For each result: reporttitle() → report_table()
#'   3. Save .docx
#'
#' @param results_list     Named list of analysis results (from mod_analyze)
#' @param selected_ids     Integer vector of selected result IDs
#' @param report_config    List of odsrtf parameters:
#'   $output, $project, $sponsor, $title, $version, $company, $orientation, $font
#' @param chapter_titles   Named list mapping result_id → chapter title string
#' @param temp_dir         Directory for temp file output
#' @return Path to generated .docx file (invisibly), or NULL on error
#' @noRd
export_results_to_docx <- function(results_list,
                                    selected_ids,
                                    report_config,
                                    chapter_titles = list(),
                                    temp_dir = tempdir()) {

  if (length(selected_ids) == 0) {
    warning("没有选中任何结果")
    return(NULL)
  }

  # Sort results by ID
  selected_ids <- sort(selected_ids)
  selected_results <- results_list[as.character(selected_ids)]
  selected_results <- Filter(Negate(is.null), selected_results)

  if (length(selected_results) == 0) {
    warning("选中的结果为空")
    return(NULL)
  }

  # Build odsrtf parameters
  output_name <- .sanitize_filename(report_config$output)

  # Use temp directory for file generation
  out_dir <- file.path(temp_dir, "report_export")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  tryCatch({
    # Step 1: Initialize document (without executing a script)
    odsrtf_init(
      output     = output_name,
      project    = report_config$project %||% "",
      sponsor    = report_config$sponsor %||% "",
      title      = report_config$title %||% "",
      version    = report_config$version %||% "",
      outdir     = out_dir,
      company    = .blank_default(report_config$company, "海博瑞（北京）数据科技有限公司"),
      orientation = .blank_default(report_config$orientation, "PORTRAIT"),
      font       = .blank_default(report_config$font, "宋体")
    )

    # Step 2: Group results by chapter and write
    # Build chapter mapping: chapter_id → list of results
    chapters <- list()
    for (res in selected_results) {
      rid <- as.character(res$id)
      ch_title <- chapter_titles[[rid]] %||% res$name
      ch_id <- paste0("ch_", rid)

      if (is.null(chapters[[ch_id]])) {
        chapters[[ch_id]] <- list(title = ch_title, results = list())
      }
      chapters[[ch_id]]$results <- c(chapters[[ch_id]]$results, list(res))
    }

    # Step 3: Write each chapter
    for (ch in chapters) {
      # reporttitle sets the chapter prefix and resets numbering
      reporttitle(ch$title)

      # Write each table in the chapter
      for (res in ch$results) {
        if (isTRUE(res$is_plot)) {
          plot_to_docx(res$plot, title = res$name)
          next
        }

        ft <- res$table
        if (is.null(ft)) next

        # Handle list of flextables (some analyses return 2)
        if (is.list(ft) && !inherits(ft, "flextable")) {
          for (sub_ft in ft) {
            if (inherits(sub_ft, "flextable")) {
              ft_to_report_table(sub_ft, write_to_doc = TRUE)
            }
          }
        } else if (inherits(ft, "flextable")) {
          ft_to_report_table(ft, write_to_doc = TRUE)
        }
      }
    }

    # Step 4: Save document
    docx_file <- odsrtf_save()

    if (!is.null(docx_file) && file.exists(docx_file)) {
      message("报告已生成: ", docx_file)
      return(docx_file)
    } else {
      warning("报告文件未找到")
      return(NULL)
    }

  }, error = function(e) {
    message("导出报告错误: ", e$message)
    message("错误调用栈:")
    print(traceback())
    warning(paste("导出报告失败:", e$message))
    return(NULL)
  })
}


# ---- Block-based export (new API) ----

#' Export report using ordered blocks
#'
#' Each block is a list with:
#'   $type   - "title" (standalone title) or "result" (analysis result)
#'   $title  - chapter title string
#'   $level  - outline level (1-6)
#'   $orient - orientation ("PORTRAIT"/"LANDSCAPE") or NULL (no change)
#'   $result - analysis result object (only for type="result")
#'
#' Processing order:
#'   1. odsrtf_init (with default orientation)
#'   2. For each block:
#'      - If orient changed: ods_orientation(orient)
#'      - reporttitle(title, level)
#'      - If type="result": write table(s)
#'   3. odsrtf_save
#'
#' @param export_blocks  Ordered list of block definitions
#' @param report_config  Document-level config (project, sponsor, etc.)
#' @param temp_dir       Output directory
#' @return Path to .docx file
#' @noRd
export_report_blocks <- function(export_blocks,
                                  report_config,
                                  temp_dir = tempdir()) {

  if (length(export_blocks) == 0) {
    warning("没有可导出的内容块")
    return(NULL)
  }

  output_name <- .sanitize_filename(report_config$output)
  out_dir <- file.path(temp_dir, "report_export")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  tryCatch({
    # Step 1: Initialize document
    odsrtf_init(
      output      = output_name,
      project     = report_config$project %||% "",
      sponsor     = report_config$sponsor %||% "",
      title       = report_config$title %||% "",
      version     = report_config$version %||% "",
      outdir      = out_dir,
      company     = .blank_default(report_config$company, "海博瑞（北京）数据科技有限公司"),
      orientation = .blank_default(report_config$orientation, "PORTRAIT"),
      font        = .blank_default(report_config$font, "宋体")
    )

    # Track current orientation to avoid redundant switches
    current_orient <- report_config$orientation %||% "PORTRAIT"

    # Step 2: Process each block
    for (block in export_blocks) {
      # 2a. Orient block: switch orientation only, no title/table output
      if (block$type == "orient") {
        block_orient <- block$orient
        if (!is.null(block_orient) && nchar(block_orient) > 0) {
          ods_orientation(block_orient)
          current_orient <- toupper(trimws(block_orient))
        }
        next
      }

      # 2b. Switch orientation if needed
      block_orient <- block$orient
      if (!is.null(block_orient) && nchar(block_orient) > 0 &&
          toupper(trimws(block_orient)) != toupper(trimws(current_orient))) {
        ods_orientation(block_orient)
        current_orient <- toupper(trimws(block_orient))
      }

      # 2c. Title block: write chapter title and reset numbering
      if (block$type == "title") {
        reporttitle(
          title = block$title,
          level = block$level %||% 2
        )
        next
      }

      # 2d. Result block: write table(s) or plot(s)
      if (block$type == "result" && !is.null(block$result)) {
        res <- block$result

        if (isTRUE(res$is_plot)) {
          plot_to_docx(res$plot, title = res$name)
          next
        }

        ft <- res$table
        if (is.null(ft)) next

        # Handle list of flextables
        if (is.list(ft) && !inherits(ft, "flextable")) {
          for (sub_ft in ft) {
            if (inherits(sub_ft, "flextable")) {
              ft_to_report_table(sub_ft, write_to_doc = TRUE)
            }
          }
        } else if (inherits(ft, "flextable")) {
          ft_to_report_table(ft, write_to_doc = TRUE)
        }
      }
    }

    # Step 3: Save
    docx_file <- odsrtf_save()

    if (!is.null(docx_file) && file.exists(docx_file)) {
      message("报告已生成: ", docx_file)
      return(docx_file)
    } else {
      warning("报告文件未找到")
      return(NULL)
    }

  }, error = function(e) {
    message("导出报告错误: ", e$message)
    message("错误调用栈:")
    print(traceback())
    warning(paste("导出报告失败:", e$message))
    return(NULL)
  })
}
