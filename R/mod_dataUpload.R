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

      # 文件上传区域 - 整体美化版
      tags$div(
        style = "border: 1px solid #e1e5f1;
           padding: 20px;
           margin-bottom: 20px;
           border-radius: 8px;
           background: linear-gradient(135deg, #f8f9ff 0%, #f0f4ff 100%);
           box-shadow: 0 2px 8px rgba(0,0,0,0.05);",

        # 区域标题
        # tags$div(
        #   style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #d1d9ff;",
        #   icon("upload", style = "color: #3498db; margin-right: 10px; font-size: 16px;"),
        #   tags$strong("文件上传设置", style = "margin: 0; color: #2c3e50; font-size: 14px;")
        # ),

        # 文件上传控件
        tags$div(
          style = "margin-bottom: 15px;",
          fileInput(ns("file"), "上传数据文件",
                    accept = c(".xlsx", "xls", "sas7bdat", ".rda", ".RData", ".csv", ".txt"),
                    buttonLabel = "选择文件...",
                    placeholder = "Excel、SAS、CSV或R数据文件")
        ),

        # 文件信息
        tags$div(
          style = "background-color: #e8f4f8;
             padding: 12px;
             border-radius: 6px;
             margin-top: 8px;
             margin-bottom: 10px;
             border-left: 4px solid #3498db;
             border: 1px solid #b8e0f0;",
          tags$div(
            style = "display: flex; align-items: flex-start; margin-bottom: 8px;",
            icon("info-circle", style = "color: #3498db; margin-right: 8px; margin-top: 2px; flex-shrink: 0;"),
            tags$div(
              style = "flex: 1;",
              tags$small("支持格式: .xlsx, .xls, .sas7bdat, .rda, .RData, .csv, .txt",
                         style = "color: #2c3e50; line-height: 1.4; display: block;"),
              tags$small("最大文件大小: 5MB",
                         style = "color: #2c3e50; line-height: 1.4; display: block; margin-top: 4px;")
            )
          )
        ),

        # 表头选项（无边框）
        tags$div(
          style = "margin-bottom: 15px;",
          checkboxInput(ns("file_header"), "启用表头", value = TRUE)
        ),

        # 表头说明信息框
        tags$div(
          style = "background-color: #e8f4f8;
           padding: 12px;
           border-radius: 6px;
           margin-bottom: 15px;
           border-left: 4px solid #3498db;
           border: 1px solid #b8e0f0;",
          tags$div(
            style = "display: flex; align-items: flex-start;",
            icon("info-circle", style = "color: #3498db; margin-right: 8px; margin-top: 2px; flex-shrink: 0;"),
            tags$div(
              style = "flex: 1;",
              tags$small("如果数据第一行是变量名，请勾选此项",
                         style = "color: #2c3e50; line-height: 1.4;")
            )
          )
        ),
      ),

      # 🟢 修改：示例数据选择区域 - 添加美化样式和提示信息
      tags$div(
        style = "margin-bottom: 15px; padding: 15px; background: linear-gradient(135deg, #f8f9ff 0%, #f0f4ff 100%); border-radius: 8px; border: 1px solid #e1e5f1;",

        # 示例数据选择标题
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          # icon("star", style = "color: #ff6b35; margin-right: 8px; font-size: 16px;"),
          # tags$strong("示例数据集", style = "color: #2c3e50; font-size: 14px;")
        ),

        # 🟢 新增：提示信息
        tags$div(
          style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
          tags$div(
            style = "display: flex; align-items: flex-start;",
            icon("info-circle", style = "color: #856404; margin-right: 8px; margin-top: 2px; flex-shrink: 0;"),
            tags$div(
              style = "flex: 1;",
              tags$small(
                "如对每种分析方法使用的数据格式有疑问，可以参考示例数据集。",
                style = "color: #856404; line-height: 1.4;"
              )
            )
          )
        ),

        tags$br(),

        # 示例数据选择器
        uiOutput(ns("example_data_selector"))


      ),

      # 清空按钮
      actionButton(ns("clear_data"), "清空数据",
                   icon = icon("trash-alt"),
                   style = "background-color: #e74c3c; color: white; margin-top: 10px; width: 100%;
                            border: none; border-radius: 5px; padding: 8px 12px;"),

      # 数据筛选模块
      conditionalPanel(
        condition = paste0("output['", ns("has_data"), "']"),
        tags$div(style = "margin-top: 20px; padding-top: 15px; border-top: 1px dashed #dee2e6;",
                 mod_data_filter_ui(ns("data_filter_1"),type="数据筛选", show_apply_button = TRUE)
        )
      )
      # 分母筛选模块
      # ,conditionalPanel(
      #   condition = paste0("output['", ns("has_data"), "'] && output['", ns("show_denominator_filter"), "']"),
      #   tags$div(style = "margin-top: 20px; padding-top: 15px; border-top: 1px dashed #dee2e6;",
      #            mod_data_filter_ui(ns("denominator_filter_1"),type="分析人数", show_apply_button = FALSE)
      #            )
      # )
    )
  )
}

mod_dataUpload_tabPanel_ui <- function(id) {
  ns <- NS(id)

  tabPanel("数据预览",
           # 数据预览容器 - 美化版
           tags$div(
             style = "border: 2px solid #e9ecef;
                      padding: 20px;
                      margin: 10px;
                      border-radius: 10px;
                      background: linear-gradient(to bottom, #ffffff, #f8f9fa);
                      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
                      height: calc(100vh - 200px);
                      display: flex;
                      flex-direction: column;",

             # 模块标题
             tags$div(
               style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #3498db; flex-shrink: 0;",
               icon("table", style = "color: #3498db; margin-right: 10px; font-size: 18px;"),
               h4("数据预览", style = "margin: 0; color: #2c3e50; font-weight: 600;")
             ),

             # 状态指示器
             tags$div(
               style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #3498db; flex-shrink: 0;",
               uiOutput(ns("data_status"))
             ),

             # 数据预览表格 - 可伸缩部分
             tags$div(
               style = "flex: 1; overflow: auto; margin-bottom: 15px;
                        border: 1px solid #e9ecef;
                        border-radius: 5px;
                        background-color: white;
                        padding: 10px;",
               DT::DTOutput(ns("data_preview"))
             ),

             # 数据信息统计 - 固定在底部
             tags$div(
               style = "padding: 12px;
                        background-color: #e8f4f8;
                        border-radius: 5px;
                        border: 1px solid #b8e0f0;
                        flex-shrink: 0;",
               uiOutput(ns("data_info"))
             )
           )
  )
}

#' dataUpload Server Functions
#'
#' @noRd
mod_dataUpload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 获取分析类型的函数
    getAnalysisType <- reactive({
      if (!is.null(session$userData$getAnalysisType)) {
        session$userData$getAnalysisType()
      } else {
        NULL
      }
    })

    # 创建响应式值存储数据
    rv <- reactiveValues(
      raw_data = NULL,
      current_data = NULL,
      filtered_data = NULL,
      data_name = NULL,
      is_filtered = FALSE,
      filter_text = "",
      is_resetting = FALSE,
      reset_trigger = 0,
      show_denominator_filter = FALSE,
      denominator_filter_text = "",
      file_type = NULL,
      example_data_loaded = FALSE,
      current_analysis_type = NULL,
      previous_analysis_type = NULL,
      # 🟢 新增：存储当前选择的示例数据名称
      current_example_data_name = NULL,
      # 🟢 新增：数据更新触发器
      data_update_trigger = 0
    )

    # 监听分析类型变化
    observe({
      analysis_type <- getAnalysisType()
      if (!is.null(analysis_type)) {
        rv$previous_analysis_type <- rv$current_analysis_type
        rv$current_analysis_type <- analysis_type
      }
    })

    # 🟢 新增：示例数据选择器UI
    output$example_data_selector <- renderUI({
      ns <- session$ns

      example_data_choices <- c(
        "描述性统计；分类变量描述" = "adsl",
        "秩和检验" = "tyypspa",
        "组间/组内比较" = "cov_adur",
        "协方差分析" = "adts",
        "2*2列联表" = "adcrslb",
        "生存分析" = "adhj"
      )

      selectInput(
        ns("example_data_choice"),
        "示例数据集(可选)",
        choices = c("选取数据集..." = "", example_data_choices),
        selected = rv$current_example_data_name
      )
    })

    # 🟢 修改：加载示例数据的函数
    load_example_data_wrapper <- function(data_name = NULL) {
      # 如果指定了数据名称，使用指定的；否则使用UI中选择的
      if (is.null(data_name)) {
        data_name <- input$example_data_choice
      }

      if (is.null(data_name) || data_name == "") {
        showNotification("请先选择示例数据集", type = "warning")
        return()
      }

      tryCatch({
        # 🟢 使用您提供的代码逻辑加载数据
        df <- switch(data_name,
                     adsl = BioStatsSuite::adsl,
                     tyypspa = BioStatsSuite::tyypspa,
                     cov_adur = BioStatsSuite::cov_adur,
                     adts = BioStatsSuite::adts,
                     adcrslb = BioStatsSuite::adcrslb,
                     adhj = BioStatsSuite::adhj
        )

        if (!is.data.frame(df)) {
          stop("加载的数据不是数据框格式")
        }

        # 🟢 更新数据状态
        rv$raw_data <- df
        rv$current_data <- df
        rv$filtered_data <- NULL
        rv$is_filtered <- FALSE
        rv$filter_text <- ""
        rv$denominator_filter_text <- ""
        rv$data_name <- toupper(data_name)  # 使用大写名称
        rv$file_type <- "example"
        rv$example_data_loaded <- TRUE
        rv$current_example_data_name <- data_name  # 记录当前选择的示例数据

        # 🟢 新增：触发数据更新信号
        rv$data_update_trigger <- rv$data_update_trigger + 1

        showNotification(paste("示例数据加载成功！(", toupper(data_name), ")", sep = ""), type = "message")

        message("Example data loaded successfully: ", data_name)
        message("Dimensions: ", nrow(df), " x ", ncol(df))
        message("Columns: ", paste(names(df), collapse = ", "))

      }, error = function(e) {
        showNotification(paste("加载示例数据错误:", e$message), type = "error")
        message("Error loading example data: ", e$message)
      })
    }

    # 🟢 修复：监听示例数据选择变化 - 选择后立即加载
    observeEvent(input$example_data_choice, {
      message("示例数据选择变化: ", input$example_data_choice)

      # 当选择了有效的数据集时立即加载
      if (!is.null(input$example_data_choice) && input$example_data_choice != "") {

        message("加载示例数据: ", input$example_data_choice)

        # 如果当前已有数据，提示用户
        if (!is.null(rv$raw_data)) {
          showNotification(paste("正在加载示例数据:", toupper(input$example_data_choice)),
                           type = "message")
        }

        load_example_data_wrapper(input$example_data_choice)
      }
    })

    # 重置文件输入框UI的函数
    reset_file_input_ui <- function() {
      # 使用JavaScript来重置文件输入框的UI显示
      session$sendCustomMessage(type = "resetFileInputUI", message = ns("file"))
    }

    # 响应上传文件
    observeEvent(input$file, {
      req(input$file)

      # ----------------test----------------
      message("=== 文件上传调试信息 ===")
      message("文件名: ", input$file$name)
      message("文件路径: ", input$file$datapath)
      message("文件大小: ", input$file$size)
      # ----------------test----------------

      tryCatch({
        # 使用工具函数读取数据
        df <- read_data_file(
          file_path = input$file$datapath,
          file_name = input$file$name,
          file_header = input$file_header  # 传递表头选项
        )

        rv$raw_data <- df
        rv$current_data <- df
        rv$filtered_data <- NULL
        rv$is_filtered = FALSE
        rv$filter_text <- ""
        rv$denominator_filter_text <- ""

        # 🟢 清空示例数据选择状态
        rv$example_data_loaded <- FALSE
        rv$current_example_data_name <- NULL

        # 🟢 新增：触发数据更新信号
        rv$data_update_trigger <- rv$data_update_trigger + 1

        # 使用工具函数获取数据名称
        data_name <- get_data_name(input$file$name)
        rv$data_name <- data_name

        # 使用工具函数获取文件类型
        rv$file_type <- get_file_type(input$file$name)

        showNotification("数据上传成功！", type = "message")

      }, error = function(e) {
        showNotification(paste("上传错误:", e$message), type = "error")
      })
    })

    # 检查是否为CSV文件
    output$is_csv_file <- reactive({
      req(input$file)
      file_ext <- tolower(tools::file_ext(input$file$name))
      file_ext %in% c("csv", "txt")
    })
    outputOptions(output, "is_csv_file", suspendWhenHidden = FALSE)

    # 初始化数据筛选模块
    data_filter_module <- mod_data_filter_server("data_filter_1", reactive({
      list(
        raw_data = rv$raw_data,
        updateFilteredData = function(filtered_df, filter_text) {
          rv$filtered_data <- filtered_df
          rv$is_filtered <- TRUE
          rv$filter_text <- filter_text
        },
        reset_trigger = rv$reset_trigger,  # 传递重置信号
        # 🟢 新增：传递数据更新触发器
        data_update_trigger = rv$data_update_trigger
      )
    }))

    # 初始化分母筛选模块
    denominator_filter_module <- mod_data_filter_server("denominator_filter_1", reactive({
      list(
        raw_data = rv$raw_data,
        updateFilteredData = function(filtered_df, filter_text) {
          rv$denominator_filter_text <- filter_text
        },
        reset_trigger = rv$reset_trigger,
        # 🟢 新增：传递数据更新触发器
        data_update_trigger = rv$data_update_trigger
      )
    }))

    # 监听分析类型变化，控制分母筛选模块显示
    observe({
      # 这里需要从外部获取当前分析类型
      req(getAnalysisType())

      rv$show_denominator_filter <- (getAnalysisType() %in% c("c_describe","q_param"))
    })

    # 输出控制分母筛选模块显示的状态
    output$show_denominator_filter <- reactive({
      rv$show_denominator_filter
    })
    outputOptions(output, "show_denominator_filter", suspendWhenHidden = FALSE)

    # 获取当前显示的数据（可能是原始数据或筛选后数据）
    current_data <- reactive({
      if (rv$is_filtered && !is.null(rv$filtered_data)) {
        return(rv$filtered_data)
      } else {
        return(rv$raw_data)
      }
    })

    # 监听数据筛选模块的变化
    observe({
      req(data_filter_module()$current_filter_text)
      req(rv$raw_data)

      # 确保筛选模块返回有效数据
      if (is.null(data_filter_module()$current_filter_text)) {
        return()
      }

      filter_text <- data_filter_module()$current_filter_text

      # 只在有新筛选条件时执行
      if (filter_text != "" && filter_text != rv$filter_text && !rv$is_resetting) {
        tryCatch({
          filtered_df <- subset(rv$raw_data, eval(parse(text = filter_text)))
          rv$filtered_data <- filtered_df
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
      rv$raw_data <- NULL
      rv$current_data <- NULL
      rv$filtered_data <- NULL
      rv$data_name <- NULL
      rv$is_filtered <- FALSE
      rv$filter_text <- ""
      rv$denominator_filter_text <- ""
      rv$is_resetting <- FALSE
      rv$file_type <- NULL
      rv$example_data_loaded <- FALSE
      rv$current_data_category <- NULL
      rv$previous_analysis_type <- NULL
      # 🟢 新增：清空示例数据选择状态
      rv$current_example_data_name <- NULL
      # 🟢 新增：触发数据更新信号
      rv$data_update_trigger <- rv$data_update_trigger + 1

      # 重置文件输入框的显示
      reset_file_input_ui()

      showNotification("数据已清空", type = "message")
    })

    # 数据状态显示
    output$data_status <- renderUI({
      if (is.null(rv$current_data)) {
        tags$div(
          style = "color: #dc3545;",
          icon("exclamation-triangle"), "请先上传数据文件或示例数据"
        )
      } else if (rv$is_filtered) {
        tags$div(
          style = "color: #28a745;",
          icon("filter"), "已应用筛选条件（显示筛选后数据）",
          actionButton(ns("reset_data"), "重置为原始数据",
                       style = "margin-left: 15px; padding: 2px 8px; font-size: 12px;")
        )
      } else if (rv$example_data_loaded) {
        tags$div(
          style = "color: #17a2b8;",
          icon("database"), paste("显示示例数据（", rv$data_name, "，未筛选）", sep = "")
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
      rv$filtered_data <- NULL
      rv$is_filtered <- FALSE
      rv$filter_text <- ""

      # 触发重置信号，通知筛选模块
      rv$reset_trigger <- rv$reset_trigger + 1
      # 🟢 新增：触发数据更新信号
      rv$data_update_trigger <- rv$data_update_trigger + 1

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
        style="default",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE,
        selection = 'none'
        # ,editable = TRUE
      )
    })

    # 返回响应式值
    return(reactive({
      # -----------在返回的reactive中添加调试信息-----------
      message("=== 数据模块返回信息 ===")
      message("当前时间: ", Sys.time())
      message("raw_data是否为NULL: ", is.null(rv$raw_data))
      if (!is.null(rv$raw_data)) {
        message("raw_data维度: ", nrow(rv$raw_data), " x ", ncol(rv$raw_data))
        message("raw_data列名: ", paste(names(rv$raw_data), collapse = ", "))
      }
      message("current_data是否为NULL: ", is.null(rv$current_data))
      message("data_name: ", rv$data_name)
      message("is_filtered: ", rv$is_filtered)
      # -----------在返回的reactive中添加调试信息-----------

      # 确保数据存在才返回
      if (is.null(rv$raw_data)) {
        message("返回NULL因为raw_data为NULL")
        return(NULL)
      }
      list(
        raw_data = rv$raw_data,
        current_data = rv$current_data,
        data_name = rv$data_name,
        is_filtered = rv$is_filtered,
        filter_text = rv$filter_text,
        denominator_filter_text = rv$denominator_filter_text,
        show_denominator_filter = rv$show_denominator_filter,
        file_type = rv$file_type,

        current_analysis_type = rv$current_analysis_type,
        example_data_loaded = rv$example_data_loaded,

        # 🟢 新增：更新分析类型的方法
        updateAnalysisType = function(new_type) {
          rv$current_analysis_type <- new_type
        },

        # 🟢 新增：数据更新触发器
        data_update_trigger = rv$data_update_trigger
      )
    }))
  })
}
## To be copied in the UI
# mod_dataUpload_sidebar_ui("dataUpload_1")
# mod_dataUpload_tabPanel_ui("dataUpload_1")

## To be copied in the server
# mod_dataUpload_server("dataUpload_1")
