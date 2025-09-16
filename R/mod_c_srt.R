#' c_srt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_c_srt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("srt_data_cond"), "数据筛选条件", value = "tyypspa|FAS=='是' & visit=='用药后 18 周±3 天' & sptest=='腺体萎缩病理组织学分级'", placeholder = "数据集|条件"),
    textInput(ns("srt_varlist"), "变量列表", value = "DIFF_TB|用药后18周±3天-基线|-3=改善3个等级/-2=改善2个等级/-1=改善1个等级/0=无变化/1=加重1个等级/2=加重2个等级/3=加重3个等级", placeholder = "变量|标签|分类=标签/分类=标签"),
    textInput(ns("srt_group_c"), "分组变量", value = "arm3|试验组/阳性药组/安慰剂组", placeholder = "变量|组别1/组别2"),
    checkboxInput(ns("srt_coltotal"), "显示列合计", value = FALSE),
    checkboxInput(ns("srt_rowtotal"), "显示行合计", value = TRUE),
    # checkboxInput(ns("srt_outyn"), "输出表格", value = TRUE),
    checkboxInput(ns("srt_test_between"), "组间比较", value = 2),
    checkboxInput(ns("srt_test_in"), "组内比较", value = FALSE),
    textInput(ns("srt_table_title"), "表格标题", value = "秩和检验结果"),
    textInput(ns("srt_ftnote"), "底注", value = "")
  )
}

#' c_srt Server Functions
#'
#' @noRd
mod_c_srt_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(reactive({
      req(rv()$data)
      list(
        data_cond = input$srt_data_cond,
        varlist = input$srt_varlist,
        group_c = input$srt_group_c,
        coltotal = as.integer(input$srt_coltotal),
        rowtotal = as.integer(input$srt_rowtotal),
        outyn = 1,
        test_between = as.integer(input$srt_test_between),
        test_in = as.integer(input$srt_test_in),
        table_title = input$srt_table_title,
        ftnote = input$srt_ftnote
      )
    }))
  })
}

## To be copied in the UI
# mod_c_srt_ui("c_srt_1")

## To be copied in the server
# mod_c_srt_server("c_srt_1")
