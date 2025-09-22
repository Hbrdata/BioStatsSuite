#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  data_upload <- mod_dataUpload_server("dataUpload_1")
  analyze_result <- mod_analyze_server("analyze_1", data_upload)

  # 创建响应式值存储当前分析类型
  rv <- reactiveValues(
    current_analysis_type = NULL
  )

  # 监听分析类型变化
  observe({
    req(analyze_result()$current_analysis_type)
    rv$current_analysis_type <- analyze_result()$current_analysis_type
  })

  # 提供获取分析类型的函数
  getAnalysisType <- reactive({
    rv$current_analysis_type
  })

  # 提供触发示例数据的函数
  triggerExampleData <- reactive({
    rv$trigger_example_data
  })

  # 将函数传递给模块
  session$userData$getAnalysisType <- getAnalysisType
  session$userData$trigger_example_data <- triggerExampleData

}
