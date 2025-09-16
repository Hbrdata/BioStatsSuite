#' covancova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_covancova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("ancova_data_cond"), "数据筛选条件", value = "adts|RANDYN=='是' & FAS=='是'  & tstest=='胃痛' & visitnum=='2'", placeholder = "数据集|条件"),
    textInput(ns("ancova_group_c"), "分组变量", value = "arm3|试验组/阳性药组/安慰剂组", placeholder = "变量|组别1/组别2/组别3"),
    textInput(ns("ancova_varlist"), "变量列表", value = "difftbsum/用药后6周±3天|SITEID/中心|TSORRES0sum/基线", placeholder = "因变量/标签|协变量1/标签|协变量2/标签"),
    textInput(ns("ancova_title1"), "表格1标题", value = "协方差分析结果－因素分析"),
    textInput(ns("ancova_title2"), "表格2标题", value = "协方差分析结果－组间比较"),
    textInput(ns("ancova_footnote1"), "底注1", value = ""),
    textInput(ns("ancova_footnote2"), "底注2", value = "")
  )
}

#' covancova Server Functions
#'
#' @noRd
mod_covancova_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(reactive({
      req(rv()$data)
      list(
        data_cond = input$ancova_data_cond,
        group_c = input$ancova_group_c,
        varlist = input$ancova_varlist,
        title1 = input$ancova_title1,
        title2 = input$ancova_title2,
        footnote1 = input$ancova_footnote1,
        footnote2 = input$ancova_footnote2
      )
    }))
  })
}

## To be copied in the UI
# mod_covancova_ui("covancova_1")

## To be copied in the server
# mod_covancova_server("covancova_1")
