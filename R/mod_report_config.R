#' Report Configuration Module
#'
#' @noRd
#' @importFrom shiny NS tagList

# ---- Caption extraction helper ----

.rcfg_get_caption <- function(ft) {
  if (is.null(ft) || !inherits(ft, "flextable")) return(NULL)
  tryCatch({
    cap <- ft$caption
    if (is.null(cap)) return(NULL)
    txt <- paste(vapply(cap, function(p) {
      paste(vapply(p, function(chunk) {
        if (is.list(chunk) && !is.null(chunk$txt)) chunk$txt else ""
      }, character(1)), collapse = "")
    }, character(1)), collapse = "")
    if (nchar(trimws(txt)) > 0) trimws(txt) else NULL
  }, error = function(e) NULL)
}

# ---- Sidebar UI ----

mod_report_config_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = paste(
        "border: 2px solid #e9ecef; padding: 20px; margin-bottom: 25px;",
        "border-radius: 10px; background: linear-gradient(to bottom, #ffffff, #f8f9fa);",
        "box-shadow: 0 2px 4px rgba(0,0,0,0.05);"
      ),
      tags$div(
        style = paste(
          "display: flex; align-items: center; margin-bottom: 15px;",
          "padding-bottom: 10px; border-bottom: 2px solid #e67e22;"
        ),
        icon("file-word", style = "color: #e67e22; margin-right: 10px; font-size: 18px;"),
        h5("Report Export Settings", style = "margin: 0; color: #2c3e50; font-weight: 600;")
      ),
      tabsetPanel(
        id = ns("config_tabs"),
        tabPanel(
          "Header",
          tags$div(
            style = "padding-top: 12px;",
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px;",
              textInput(ns("project"), "Project Name (L1-Left)", value = "", placeholder = "e.g. ABC-123"),
              textInput(ns("sponsor"), "Sponsor (L1-Right)", value = "", placeholder = "e.g. XX Pharma"),
              textInput(ns("title"), "Report Title (L2-Left)", value = "", placeholder = "e.g. Statistical Report"),
              textInput(ns("version"), "Version (L2-Right)", value = "", placeholder = "e.g. V1.0")
            )
          )
        ),
        tabPanel(
          "Footer",
          tags$div(
            style = "padding-top: 12px;",
            textInput(ns("company"), "Footer Company",
                      value = "HaiBoRui (Beijing) Data Technology Co., Ltd.")
          )
        ),
        tabPanel(
          "Document",
          tags$div(
            style = "padding-top: 12px;",
            textInput(ns("output"), "Output Filename", value = "", placeholder = "without .docx"),
            selectInput(ns("default_orientation"), "Default Orientation",
                        choices = list("Portrait" = "PORTRAIT", "Landscape" = "LANDSCAPE"),
                        selected = "PORTRAIT"),
            textInput(ns("font"), "Chinese Font", value = "SimSun")
          )
        )
      )
    )
  )
}

# ---- Tab Panel UI ----

mod_report_config_tabPanel_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Report Export",
    tags$div(
      style = paste(
        "border: 2px solid #e9ecef; padding: 20px; margin: 10px;",
        "border-radius: 10px; background: linear-gradient(to bottom, #ffffff, #f8f9fa);",
        "box-shadow: 0 2px 4px rgba(0,0,0,0.05); min-height: 400px;"
      ),
      tags$div(
        style = paste(
          "display: flex; align-items: center; margin-bottom: 15px;",
          "padding-bottom: 10px; border-bottom: 2px solid #e67e22;"
        ),
        icon("file-word", style = "color: #e67e22; margin-right: 10px; font-size: 18px;"),
        h4("Report Export", style = "margin: 0; color: #2c3e50; font-weight: 600;")
      ),
      tags$div(
        style = paste(
          "background-color: #fef9e7; padding: 12px; border-radius: 6px;",
          "margin-bottom: 15px; border-left: 4px solid #f39c12;"
        ),
        tags$small(
          "Insert title blocks to define chapters. Insert orient to switch page direction. Run analysis to add result tables. Drag rows or use arrows to reorder.",
          style = "color: #2c3e50; line-height: 1.4;"
        )
      ),
      tags$div(
        style = "margin-bottom: 20px;",
        tags$h5("Export Content", style = "color: #2c3e50; margin-bottom: 10px;"),
        uiOutput(ns("content_table_ui"))
      ),
      tags$div(
        style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
        actionButton(ns("add_title_block"), "Insert Title Block",
                     icon = icon("heading"),
                     style = "background-color: #8e44ad; color: white; border: none; font-weight: bold; padding: 10px 16px; border-radius: 4px;"),
        actionButton(ns("add_orient"), "Insert Orient",
                     icon = icon("compass"),
                     style = "background-color: #2980b9; color: white; border: none; font-weight: bold; padding: 10px 16px; border-radius: 4px;"),
        actionButton(ns("export_docx"), "Export Word",
                     icon = icon("file-export"),
                     style = "background-color: #e67e22; color: white; border: none; font-weight: bold; padding: 10px 20px; border-radius: 4px;"),
        actionButton(ns("refresh_results"), "Refresh",
                     icon = icon("sync"),
                     style = "background-color: #95a5a6; color: white; border: none; padding: 10px 16px; border-radius: 4px;")
      ),
      uiOutput(ns("download_area_ui"))
    )
  )
}

# ---- Shared helpers ----
# Grid: Drag | # | Type(L+Level) | Chapter Title | Actions
.rcfg_grid_cols <- "28px 40px 90px 60px 1fr 80px"

.rcfg_content_row <- function(item, idx, ns) {
  bg <- if (idx %% 2 == 0) "#f8f9fa" else "white"
  cid <- as.character(item$content_id)
  tags$div(
    id = ns(paste0("content_row_", cid)),
    class = "rcfg-sortable-row",
    `data-cid` = item$content_id,
    draggable = "true",
    style = paste0(
      "display: grid; grid-template-columns: ", .rcfg_grid_cols, "; ",
      "gap: 6px; padding: 6px 10px; background-color: ", bg, "; ",
      "border: 1px solid #dee2e6; border-top: none; align-items: center;"
    ),
    tags$span(icon("grip-vertical"), class = "rcfg-drag-handle", style = "color: #adb5bd; cursor: grab; font-size: 12px; text-align: center;"),
    tags$span(sprintf("%d", idx), style = "font-weight: bold; color: #3498db;"),
    tags$span(item$analysis_type, style = "color: #7f8c8d; font-size: 11px;"),
    tags$span(),
    tags$span(item$title, style = "color: #2c3e50; font-size: 13px;"),
    tags$div(
      style = "display: flex; gap: 2px; align-items: center; justify-content: center;",
      tags$button(
        icon("arrow-up"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'up'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      ),
      tags$button(
        icon("arrow-down"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'down'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      )
    )
  )
}

.rcfg_block_row <- function(item, idx, ns) {
  bg <- if (idx %% 2 == 0) "#f3e5f5" else "white"
  cid <- as.character(item$content_id)
  tags$div(
    id = ns(paste0("content_row_", cid)),
    class = "rcfg-sortable-row",
    `data-cid` = item$content_id,
    draggable = "true",
    style = paste0(
      "display: grid; grid-template-columns: ", .rcfg_grid_cols, "; ",
      "gap: 6px; padding: 6px 10px; background-color: ", bg, "; ",
      "border: 1px solid #ce93d8; border-top: none; align-items: center;"
    ),
    tags$span(icon("grip-vertical"), class = "rcfg-drag-handle", style = "color: #adb5bd; cursor: grab; font-size: 12px; text-align: center;"),
    tags$span(icon("heading"), style = "color: #8e44ad;"),
    tags$span("Title", style = "color: #8e44ad; font-size: 11px; font-weight: bold;"),
    selectInput(ns(paste0("block_level_", cid)), label = NULL,
                choices = 1:9, selected = item$level, width = "50px"),
    textInput(ns(paste0("block_title_", cid)), label = NULL, value = item$title, width = "100%"),
    tags$div(
      style = "display: flex; gap: 2px; align-items: center; justify-content: center;",
      tags$button(
        icon("trash-alt"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #e74c3c; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %d, {priority: 'event'})",
          ns("del_content_click"), item$content_id
        )
      ),
      tags$button(
        icon("arrow-up"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'up'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      ),
      tags$button(
        icon("arrow-down"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'down'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      )
    )
  )
}

.rcfg_orient_row <- function(item, idx, ns) {
  bg <- if (idx %% 2 == 0) "#e8f6f3" else "white"
  cid <- as.character(item$content_id)
  tags$div(
    id = ns(paste0("content_row_", cid)),
    class = "rcfg-sortable-row",
    `data-cid` = item$content_id,
    draggable = "true",
    style = paste0(
      "display: grid; grid-template-columns: ", .rcfg_grid_cols, "; ",
      "gap: 6px; padding: 6px 10px; background-color: ", bg, "; ",
      "border: 1px solid #48c9b0; border-top: none; align-items: center;"
    ),
    tags$span(icon("grip-vertical"), class = "rcfg-drag-handle", style = "color: #adb5bd; cursor: grab; font-size: 12px; text-align: center;"),
    tags$span(icon("compass"), style = "color: #2980b9;"),
    tags$span("Orient", style = "color: #2980b9; font-size: 11px; font-weight: bold;"),
    tags$span(),
    selectInput(ns(paste0("orient_orient_", cid)), label = NULL,
                choices = list("Portrait" = "PORTRAIT", "Landscape" = "LANDSCAPE"),
                selected = item$orient, width = "180px"),
    tags$div(
      style = "display: flex; gap: 2px; align-items: center; justify-content: center;",
      tags$button(
        icon("trash-alt"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #e74c3c; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %d, {priority: 'event'})",
          ns("del_content_click"), item$content_id
        )
      ),
      tags$button(
        icon("arrow-up"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'up'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      ),
      tags$button(
        icon("arrow-down"),
        class = "btn btn-link action-button",
        style = "padding: 1px; color: #3498db; border: none; background: none; cursor: pointer; font-size: 11px;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', {id: %d, dir: 'down'}, {priority: 'event'})",
          ns("move_click"), item$content_id
        )
      )
    )
  )
}

# ---- Server ----

mod_report_config_server <- function(id, data_upload_module, analyze_result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exported_file <- reactiveVal(NULL)

    # Unified ordered content list
    content_items <- reactiveVal(list())
    next_content_id <- reactiveVal(1L)
    prev_result_ids <- reactiveVal(character(0))

    # ---- Auto-append new analysis results ----
    observe({
      results <- analyze_result()$results_list
      selected <- analyze_result()$selected_results
      prev_ids <- prev_result_ids()

      if (is.null(results) || length(results) == 0) {
        if (length(prev_ids) > 0) prev_result_ids(character(0))
        return()
      }

      sel_ids <- as.character(selected)
      valid_ids <- sel_ids[sel_ids %in% names(results)]
      if (length(valid_ids) == 0) valid_ids <- names(results)

      existing_ids <- vapply(content_items(), function(x) {
        if (identical(x$type, "result")) as.character(x$result_id) else ""
      }, character(1))

      new_ids <- setdiff(valid_ids, existing_ids)

      if (length(new_ids) > 0) {
        items <- content_items()
        cid <- next_content_id()
        for (rid in new_ids) {
          res <- results[[rid]]
          # Extract table caption as chapter title
          caption <- NULL
          if (!is.null(res$table)) {
            if (is.list(res$table) && !inherits(res$table, "flextable")) {
              for (sub_ft in res$table) {
                caption <- .rcfg_get_caption(sub_ft)
                if (!is.null(caption)) break
              }
            } else {
              caption <- .rcfg_get_caption(res$table)
            }
          }
          ch_title <- caption %||% res$name
          items[[length(items) + 1]] <- list(
            type = "result", content_id = cid, result_id = as.integer(rid),
            title = ch_title, level = 7L, orient = "PORTRAIT",
            analysis_type = res$analysis_type
          )
          cid <- cid + 1L
        }
        content_items(items)
        next_content_id(cid)
      }

      prev_result_ids(valid_ids)
    })

    # ---- Sync input values back to stored content ----
    observe({
      items <- content_items()
      if (length(items) == 0) return()
      changed <- FALSE
      for (i in seq_along(items)) {
        cid <- items[[i]]$content_id
        if (items[[i]]$type == "title") {
          tv <- input[[paste0("block_title_", cid)]]
          if (!is.null(tv)) {
            lv <- input[[paste0("block_level_", cid)]]
            if (!identical(items[[i]]$title, tv) ||
                !identical(as.character(items[[i]]$level), as.character(lv))) {
              items[[i]]$title <- tv
              items[[i]]$level <- lv
              changed <- TRUE
            }
          }
        } else if (items[[i]]$type == "orient") {
          ov <- input[[paste0("orient_orient_", cid)]]
          if (!is.null(ov) && !identical(items[[i]]$orient, ov)) {
            items[[i]]$orient <- ov
            changed <- TRUE
          }
        }
      }
      if (changed) content_items(items)
    })

    # ---- Add title block ----
    observeEvent(input$add_title_block, {
      cid <- next_content_id()
      items <- content_items()
      items[[length(items) + 1]] <- list(
        type = "title", content_id = cid,
        title = "", level = 2L, orient = "", analysis_type = "Title"
      )
      content_items(items)
      next_content_id(cid + 1L)
    })

    # ---- Add orient ----
    observeEvent(input$add_orient, {
      cid <- next_content_id()
      items <- content_items()
      items[[length(items) + 1]] <- list(
        type = "orient", content_id = cid,
        title = "", level = 0L, orient = "PORTRAIT", analysis_type = "Orient"
      )
      content_items(items)
      next_content_id(cid + 1L)
    })

    # ---- Delete content item ----
    observeEvent(input$del_content_click, {
      bid <- input$del_content_click
      items <- content_items()
      content_items(Filter(function(x) x$content_id != bid, items))
    })

    # ---- Move content item up/down ----
    observeEvent(input$move_click, {
      move_id <- input$move_click$id
      direction <- input$move_click$dir
      items <- content_items()
      n <- length(items)
      if (n < 2) return()

      idx <- which(vapply(items, function(x) x$content_id == move_id, logical(1)))
      if (length(idx) != 1) return()

      new_idx <- if (direction == "up") idx - 1L else idx + 1L
      if (new_idx < 1 || new_idx > n) return()

      items[c(idx, new_idx)] <- items[c(new_idx, idx)]
      content_items(items)
    })

    # ---- Drag reorder content items ----
    observeEvent(input$drag_reorder, {
      new_order <- input$drag_reorder
      if (is.null(new_order) || length(new_order) < 2) return()
      items <- content_items()
      ids <- vapply(items, function(x) x$content_id, integer(1))
      if (!setequal(ids, new_order)) return()
      reordered <- lapply(new_order, function(cid) {
        items[[which(ids == cid)[1]]]
      })
      content_items(reordered)
    })

    # ---- Render unified content table ----
    output$content_table_ui <- renderUI({
      items <- content_items()
      results <- analyze_result()$results_list
      selected <- analyze_result()$selected_results
      sel_ids <- if (!is.null(selected)) as.character(selected) else character(0)

      display_items <- Filter(function(x) {
        if (x$type != "result") return(TRUE)
        rid <- as.character(x$result_id)
        if (is.null(results) || !rid %in% names(results)) return(FALSE)
        if (length(sel_ids) > 0 && !rid %in% sel_ids) return(FALSE)
        TRUE
      }, items)

      if (length(display_items) == 0) {
        return(tags$div(
          style = "text-align: center; padding: 30px; color: #6c757d;",
          icon("inbox", style = "font-size: 36px; margin-bottom: 10px;"),
          tags$p("No content yet. Click 'Insert Title Block' or run analysis first.")
        ))
      }

      header_row <- tags$div(
        style = paste0(
          "display: grid; grid-template-columns: ", .rcfg_grid_cols, "; ",
          "gap: 6px; padding: 6px 10px; background-color: #34495e; color: white; ",
          "border-radius: 5px 5px 0 0; font-weight: bold; font-size: 12px;"
        ),
        tags$span(""), tags$span("#"), tags$span("Type"), tags$span("Level"), tags$span("Chapter Title"), tags$span("")
      )

      all_rows <- list()
      for (i in seq_along(display_items)) {
        item <- display_items[[i]]
        if (item$type == "title") {
          all_rows[[length(all_rows) + 1]] <- .rcfg_block_row(item, i, ns)
        } else if (item$type == "orient") {
          all_rows[[length(all_rows) + 1]] <- .rcfg_orient_row(item, i, ns)
        } else {
          all_rows[[length(all_rows) + 1]] <- .rcfg_content_row(item, i, ns)
        }
      }

      tags$div(
        class = "rcfg-sortable",
        `data-ns` = ns(""),
        do.call(tagList, c(list(header_row), all_rows))
      )
    })

    # ---- Export ----
    observeEvent(input$export_docx, {
      items <- content_items()
      results <- analyze_result()$results_list
      selected <- analyze_result()$selected_results
      sel_ids <- if (!is.null(selected)) as.character(selected) else character(0)

      default_orient <- input$default_orientation %||% "PORTRAIT"
      current_orient <- default_orient

      export_blocks <- list()
      for (item in items) {
        if (item$type == "orient") {
          orient_val <- input[[paste0("orient_orient_", item$content_id)]] %||% item$orient
          current_orient <- toupper(trimws(orient_val))
          export_blocks[[length(export_blocks) + 1]] <- list(
            type = "orient", orient = current_orient
          )
        } else if (item$type == "title") {
          title_val <- input[[paste0("block_title_", item$content_id)]] %||% item$title
          level_val <- input[[paste0("block_level_", item$content_id)]] %||% item$level
          if (nchar(trimws(title_val)) > 0) {
            export_blocks[[length(export_blocks) + 1]] <- list(
              type = "title", title = trimws(title_val),
              level = as.integer(level_val %||% 2), orient = current_orient
            )
          }
        } else {
          rid <- as.character(item$result_id)
          if (is.null(results) || !rid %in% names(results)) next
          if (length(sel_ids) > 0 && !rid %in% sel_ids) next
          export_blocks[[length(export_blocks) + 1]] <- list(
            type = "result", title = trimws(item$title),
            level = 7L, orient = current_orient,
            result = results[[rid]]
          )
        }
      }

      if (length(export_blocks) == 0) {
        showNotification("No content to export.", type = "warning")
        return()
      }

      report_config <- list(
        output = .sanitize_filename(input$output),
        project = input$project %||% "",
        sponsor = input$sponsor %||% "",
        title = input$title %||% "",
        version = input$version %||% "",
        company = .blank_default(input$company, "HaiBoRui (Beijing) Data Technology Co., Ltd."),
        orientation = .blank_default(default_orient, "PORTRAIT"),
        font = .blank_default(input$font, "SimSun")
      )

      withProgress(message = "Generating report...", value = 0, {
        incProgress(0.3, detail = "Initializing document...")
        docx_path <- tryCatch({
          export_report_blocks(export_blocks, report_config, tempdir())
        }, error = function(e) {
          message("Export error: ", e$message)
          showNotification(paste("Export failed:", e$message), type = "error")
          return(NULL)
        })
        incProgress(0.9, detail = "Saving file...")
      })

      if (!is.null(docx_path) && file.exists(docx_path)) {
        exported_file(docx_path)
        showNotification("Report generated successfully!", type = "message")
      }
    })

    # ---- Download ----
    output$download_docx <- downloadHandler(
      filename = function() {
        fp <- exported_file()
        base <- if (!is.null(fp)) basename(fp) else ""
        paste0(.sanitize_filename(base), ".docx")
      },
      content = function(file) {
        fp <- exported_file()
        if (!is.null(fp) && file.exists(fp)) file.copy(fp, file)
      }
    )

    output$download_area_ui <- renderUI({
      fp <- exported_file()
      if (!is.null(fp) && file.exists(fp)) {
        tagList(
          tags$div(
            style = paste(
              "background-color: #d4edda; padding: 10px; border-radius: 5px;",
              "border: 1px solid #c3e6cb; color: #155724; margin-top: 15px;"
            ),
            icon("check-circle"),
            paste("Report ready:", basename(fp)),
            tags$br(),
            tags$small(paste("Path:", normalizePath(fp, winslash = "/")),
                       style = "color: #6c757d;")
          ),
          tags$div(
            style = "margin-top: 10px;",
            downloadButton(ns("download_docx"), "Download Word",
                           class = "btn-success",
                           style = "background-color: #27ae60; border-color: #27ae60; color: white;")
          )
        )
      }
    })

    observeEvent(input$refresh_results, {
      showNotification("Results list auto-refreshes with analysis results.", type = "message")
    })

    return(reactive(list(exported_file = exported_file())))
  })
}
