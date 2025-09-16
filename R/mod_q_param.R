#' q_param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_q_param_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("param_data_cond"), "数据筛选条件", value = "cov_adur|DSYN=='是' & visit_no==0 &  fas=='是'  &  IPSSYZ1=='≥12分'", placeholder = "数据集|条件"),
    textInput(ns("param_denominator_cond"), "分母条件", value = "cov_adur|DSYN=='是' & visit_no==0 &  fas=='是'  &  IPSSYZ1=='≥12分'", placeholder = "数据集|条件"),
    textInput(ns("param_group_c"), "分组变量", value = "arm3|大剂量组/小剂量组/零剂量组", placeholder = "变量|组别1/组别2/组别3"),
    textInput(ns("param_varlist"), "变量列表", value = "URPVVtb|基线", placeholder = "变量|标签"),
    checkboxInput(ns("param_rowtotal"), "显示行合计", value = TRUE),
    checkboxInput(ns("param_pairt"), "配对t检验", value = FALSE),
    # checkboxInput(ns("param_outyn"), "输出表格", value = TRUE),
    checkboxInput(ns("param_test_between"), "组间检验", value = FALSE),
    textInput(ns("param_title"), "表格标题", value = "定量参数分析"),
    textInput(ns("param_footnote"), "底注", value = "")
  )
}

#' q_param Server Functions
#'
#' @noRd
mod_q_param_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(reactive({
      req(rv()$data)
      list(
        data_cond = input$param_data_cond,
        denominator_cond = input$param_denominator_cond,
        group_c = input$param_group_c,
        varlist = input$param_varlist,
        rowtotal = as.integer(input$param_rowtotal),
        pairt = as.integer(input$param_pairt),
        outyn = 1,
        test_between = as.integer(input$param_test_between),
        title = input$param_title,
        footnote = input$param_footnote
      )
    }))
  })
}

## To be copied in the UI
# mod_q_param_ui("q_param_1")

## To be copied in the server
# mod_q_param_server("q_param_1")
