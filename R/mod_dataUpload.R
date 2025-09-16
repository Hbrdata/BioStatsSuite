#' dataUpload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataUpload_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 数据上传模块 - 美化版
    tags$div(
      style = "border: 2px solid #e9ecef;
               padding: 20px;
               margin-bottom: 25px;
               border-radius: 10px;
               background: linear-gradient(to bottom, #ffffff, #f8f9fa);
               box-shadow: 0 2px 4px rgba(0,0,0,0.05);
               transition: all 0.3s ease;",

      # 模块标题
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #3498db;",
        icon("database", style = "color: #3498db; margin-right: 10px; font-size: 18px;"),
        h5("数据上传管理", style = "margin: 0; color: #2c3e50; font-weight: 600;")
      ),

      # 文件上传区域
      tags$div(
        style = "margin-bottom: 15px;",
        fileInput(ns("file"), "上传数据文件",
                  accept = c(".xlsx", "xls", "sas7bdat"),
                  buttonLabel = "选择文件...",
                  placeholder = "Excel或SAS文件")
      ),

      # 文件信息
      tags$div(
        style = "background-color: #f8f9fa; padding: 8px 12px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #6c757d;",
        tags$small(icon("info-circle"), "支持格式: .xlsx, .xls, .sas7bdat", style = "color: #6c757d;"),
        tags$br(),
        tags$small(icon("hard-drive"), "最大文件大小: 100MB", style = "color: #6c757d;")
      ),

      # 数据集名称
      textInput(ns("data_name"), "数据集名称",
                value = "",
                placeholder = "文件上传后自动填充",
                width = "100%"),

      # 清空按钮
      actionButton(ns("clear_data"), "清空上传数据",
                   icon = icon("trash-alt"),
                   style = "background-color: #e74c3c; color: white; margin-top: 10px; width: 100%;
                            border: none; border-radius: 5px; padding: 8px 12px;"),

      # 数据筛选模块
      conditionalPanel(
        condition = paste0("output['", ns("has_data"), "']"),
        tags$div(style = "margin-top: 20px; padding-top: 15px; border-top: 1px dashed #dee2e6;",
                 mod_data_filter_ui(ns("data_filter_1")))
      )
    )
  )
}

mod_dataUpload_tabPanel_ui <- function(id) {
  ns <- NS(id)

  tabPanel("数据预览",
           h4("数据预览"),
           # 状态指示器
           tags$div(
             style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 5px;",
             uiOutput(ns("data_status"))
           ),
           # 数据预览表格
           DT::DTOutput(ns("data_preview")),
           # 数据信息统计
           tags$div(
             style = "margin-top: 10px; padding: 8px; background-color: #e8f4f8; border-radius: 5px;",
             uiOutput(ns("data_info"))
           )
  )
}

#' dataUpload Server Functions
#'
#' @noRd
mod_dataUpload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 创建响应式值存储数据
    rv <- reactiveValues(
      raw_data = NULL,      # 原始上传数据
      current_data = NULL,  # 当前显示数据（原始或筛选后）
      data_name = NULL,
      is_filtered = FALSE,  # 标记数据是否经过筛选
      filter_text = "",      # 当前筛选条件文本
      is_resetting = FALSE, # 标记是否正在重置
      reset_trigger = 0     # 新增：重置触发器，用于通知筛选模块
    )

    # 重置文件输入框UI的函数
    reset_file_input_ui <- function() {
      # 使用JavaScript来重置文件输入框的UI显示
      session$sendCustomMessage(type = "resetFileInputUI", message = ns("file"))
    }


    # 响应上传文件
    observeEvent(input$file, {
      req(input$file)

      tryCatch({
        file_ext <- tools::file_ext(input$file$name)

        if (file_ext %in% c("xlsx", "xls")) {
          df <- readxl::read_excel(input$file$datapath)
        } else if (file_ext %in% c("sas7bdat")) {
          if (!requireNamespace("haven", quietly = TRUE)) {
            stop("请先安装haven包: install.packages('haven')")
          }
          df <- haven::read_sas(input$file$datapath)
        } else {
          stop("请上传Excel文件(.xlsx, .xls)或SAS文件(.sas7bdat)")
        }

        rv$raw_data <- df
        rv$current_data <- df
        rv$is_filtered <- FALSE
        rv$filter_text <- ""

        data_name <- tools::file_path_sans_ext(input$file$name)
        rv$data_name <- data_name

        updateTextInput(session, "data_name", value = data_name)
        showNotification("数据上传成功！", type = "message")

      }, error = function(e) {
        showNotification(paste("上传错误:", e$message), type = "error")
      })
    })

    # 初始化数据筛选模块
    filter_module <- mod_data_filter_server("data_filter_1", reactive({
      list(
        raw_data = rv$raw_data,
        updateFilteredData = function(filtered_df, filter_text) {
          rv$filtered_data <- filtered_df
          rv$is_filtered <- TRUE
          rv$filter_text <- filter_text
        },
        reset_trigger = rv$reset_trigger  # 传递重置信号
      )
    }))


    # 获取当前显示的数据（可能是原始数据或筛选后数据）
    current_data <- reactive({
      if (rv$is_filtered && !is.null(rv$filtered_data)) {
        return(rv$filtered_data)
      } else {
        return(rv$raw_data)
      }
    })

    observe({
      req(filter_module()$current_filter_text)
      req(rv$raw_data)

      filter_text <- filter_module()$current_filter_text

      # 只在有新筛选条件时执行
      if (filter_text != "" && filter_text != rv$filter_text && !rv$is_resetting) {
        tryCatch({
          filter_expr <- parse(text = filter_text)
          filtered_df <- subset(rv$raw_data, eval(parse(text = filter_expr)))
          rv$current_data <- filtered_df
          rv$is_filtered <- TRUE
          rv$filter_text <- filter_text

          showNotification(sprintf("筛选完成！从 %d 行筛选到 %d 行",
                                   nrow(rv$raw_data),
                                   nrow(filtered_df)),
                           type = "message")
        }, error = function(e) {
          showNotification(paste("筛选条件错误:", e$message), type = "error")
        })
      }
    })

    # 清空上传数据
    observeEvent(input$clear_data, {

      # # 清空筛选条件
      # clear_filter_conditions()


      rv$raw_data <- NULL
      rv$current_data <- NULL
      rv$data_name <- NULL
      rv$is_filtered <- FALSE
      rv$filter_text <- ""
      rv$is_resetting <- FALSE

      # 重置文件输入框的显示
      reset_file_input_ui()

      updateTextInput(session, "data_name", value = "")
      showNotification("数据已清空", type = "message")
    })

    # 数据状态显示
    output$data_status <- renderUI({
      if (is.null(rv$current_data)) {
        tags$div(
          style = "color: #dc3545;",
          icon("exclamation-triangle"), "请先上传数据文件"
        )
      } else if (rv$is_filtered) {
        tags$div(
          style = "color: #28a745;",
          icon("filter"), "已应用筛选条件（显示筛选后数据）",
          actionButton(ns("reset_data"), "重置为原始数据",
                       style = "margin-left: 15px; padding: 2px 8px; font-size: 12px;")
        )
      } else {
        tags$div(
          style = "color: #17a2b8;",
          icon("database"), "显示原始数据（未筛选）"
        )
      }
    })

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(rv$raw_data) && nrow(rv$raw_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 重置数据按钮
    observeEvent(input$reset_data, {

      rv$is_resetting <- TRUE
      rv$current_data <- rv$raw_data
      rv$is_filtered <- FALSE
      rv$filter_text <- ""

      # 触发重置信号，通知筛选模块
      rv$reset_trigger <- rv$reset_trigger + 1

      invalidateLater(100, session)
      observe({
        rv$is_resetting <- FALSE
      })

      showNotification("已重置为原始数据", type = "message")
    })

    # 数据信息统计
    output$data_info <- renderUI({
      req(rv$current_data)

      tags$div(
        tags$span(icon("table"), sprintf("行数: %d", nrow(rv$current_data))),
        tags$span(style = "margin-left: 20px;"),
        tags$span(icon("columns"), sprintf("列数: %d", ncol(rv$current_data))),
        tags$span(style = "margin-left: 20px;"),
        tags$span(icon("filter"), ifelse(rv$is_filtered, "已筛选", "未筛选")),
        if (rv$is_filtered) {
          tagList(
            tags$br(),
            tags$span(icon("code"), sprintf("条件: %s", rv$filter_text),
                      style = "font-size: 12px; color: #6c757d;")
          )
        }
      )
    })

    # 数据预览
    output$data_preview <- DT::renderDT({
      req(rv$current_data)

      DT::datatable(
        rv$current_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          dom = 'ltip'
        ),
        rownames = FALSE,
        selection = 'none'
      )
    })

    # 返回响应式值
    return(reactive({
      list(
        raw_data = rv$raw_data,
        current_data = rv$current_data,
        data_name = rv$data_name,
        is_filtered = rv$is_filtered,
        filter_text = rv$filter_text
      )
    }))
  })
}

## To be copied in the UI
# mod_dataUpload_sidebar_ui("dataUpload_1")
# mod_dataUpload_tabPanel_ui("dataUpload_1")

## To be copied in the server
# mod_dataUpload_server("dataUpload_1")
