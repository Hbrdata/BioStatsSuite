#' crosstable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crosstable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("cross_data_cond"), "数据筛选条件", value = "adcrslb|RANDYN=='是' & SS=='是' & visitnum=='2' & lbtest=='白细胞数'", placeholder = "数据集|条件"),
    textInput(ns("cross_group_c"), "分组变量", value = "arm3|试验组/阳性药组/安慰剂组", placeholder = "变量|组别1/组别2/组别3"),
    numericInput(ns("cross_missing"), "缺失值填补", value = 4),
    textInput(ns("cross_row_colvar"), "行列变量", value = "LBCLSIG_1/治疗前|LBCLSIG/治疗后", placeholder = "行变量/标签|列变量/标签"),
    textInput(ns("cross_format"), "格式定义", value = "1=正常|2=异常无临床意义|3=异常有临床意义|4=未查", placeholder = "值=标签|值=标签"),
    textInput(ns("cross_table_title"), "表格标题", value = "交叉表分析"),
    textInput(ns("cross_footnote"), "底注", value = "")
  )
}

#' crosstable Server Functions
#'
#' @noRd
mod_crosstable_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(reactive({
      req(rv()$data)
      list(
        data_cond = input$cross_data_cond,
        group_c = input$cross_group_c,
        missing = input$cross_missing,
        row_colvar = input$cross_row_colvar,
        format = input$cross_format,
        table_title = input$cross_table_title,
        footnote = input$cross_footnote
      )
    }))
  })
}

## To be copied in the UI
# mod_crosstable_ui("crosstable_1")

## To be copied in the server
# mod_crosstable_server("crosstable_1")
