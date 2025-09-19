#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @noRd

app_server <- function(input, output, session) {
  shinyjs::useShinyjs()

  data_upload <- mod_dataUpload_server("dataUpload_1")
  analyze_result <- mod_analyze_server("analyze_1", data_upload)

  # 创建响应式值存储当前分析类型
  rv <- reactiveValues(
    current_analysis_type = NULL,
    trigger_example_data = FALSE,
    data_module_ready = FALSE
  )

  # 监听数据上传模块是否就绪
  observe({
    req(data_upload())
    rv$data_module_ready <- TRUE
    message("Data upload module is now ready")
  })

  # 监听分析类型变化
  observe({
    req(analyze_result()$current_analysis_type)
    rv$current_analysis_type <- analyze_result()$current_analysis_type

    message("Analysis type changed to: ", rv$current_analysis_type)
    message("Data module ready: ", rv$data_module_ready)

    # 当选择q_describe且数据模块就绪时，触发示例数据加载
    if (rv$current_analysis_type == "q_describe" && rv$data_module_ready) {
      # 检查是否已经有数据
      current_data <- data_upload()$raw_data
      message("Current data exists: ", !is.null(current_data))

      if (is.null(current_data)) {
        message("Triggering example data loading...")
        rv$trigger_example_data <- TRUE

        # 3秒后重置触发信号
        shinyjs::delay(3000, {
          rv$trigger_example_data <- FALSE
          message("Reset trigger_example_data")
        })
      }
    }
  })

  # 提供获取分析类型的函数
  getAnalysisType <- reactive({
    rv$current_analysis_type
  })

  # 提供触发示例数据的函数
  triggerExampleData <- reactive({
    rv$trigger_example_data
  })

  # 将函数传递给模块 - 修复这里！添加缺失的 trigger_example_data
  session$userData$getAnalysisType <- getAnalysisType
  session$userData$trigger_example_data <- triggerExampleData  # 添加这一行

}
