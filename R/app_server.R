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
  rv <- reactiveValues(current_analysis_type = NULL)

  # 监听分析类型变化
  observe({
    # 这里需要从分析模块获取当前选择的类型
    # 您需要在 mod_analyze.R 中暴露当前分析类型
    req(analyze_result()$current_analysis_type)
    rv$current_analysis_type <- analyze_result()$current_analysis_type
  })

  # 提供获取分析类型的函数
  getAnalysisType <- reactive({
    rv$current_analysis_type
  })

  # 将分析类型信息传递给数据上传模块
  session$userData$getAnalysisType <- getAnalysisType

}
