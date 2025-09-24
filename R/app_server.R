#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
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

  # -------------监听数据上传模块的变化 test-------------
  observe({
    data_info <- data_upload()
    message("=== 主服务器监听数据变化 ===")
    message("数据模块返回值是否为NULL: ", is.null(data_info))
    if (!is.null(data_info)) {
      message("可用数据元素: ", paste(names(data_info), collapse = ", "))
    }
  })
  # ----------------------------------  test-------------

}
