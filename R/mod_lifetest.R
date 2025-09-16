#' lifetest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lifetest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("life_data_cond"), "数据筛选条件", value = "adhj|RANDYN=='是' & FAS=='是'  & fxyn==1 & anafl=='是'", placeholder = "数据集|条件"),
    textInput(ns("life_group_c"), "分组变量", value = "arm3|试验组/对照组", placeholder = "变量|组别1/组别2"),
    textInput(ns("life_censor"), "删失变量", value = "censor"),
    textInput(ns("life_time_label"), "时间变量", value = "lgzzhj|流感症状缓解时间（h）", placeholder = "变量|标签"),
    textInput(ns("life_timelist"), "时间点列表", value = "0,2,4,6,10,14,18,24,48,72", placeholder = "逗号分隔的时间点"),
    numericInput(ns("life_type"), "输出类型", value = 1, min = 0, max = 1),
    textInput(ns("life_topleftlabel"), "左列标签", value = "指标"),
    textInput(ns("life_title"), "表格标题", value = "生存分析结果"),
    textInput(ns("life_footnote"), "底注", value = "")
  )
}

#' lifetest Server Functions
#'
#' @noRd
mod_lifetest_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(reactive({
      req(rv()$data)
      list(
        data_cond = input$life_data_cond,
        group_c = input$life_group_c,
        censor = input$life_censor,
        time_label = input$life_time_label,
        timelist = as.numeric(unlist(strsplit(input$life_timelist, ",\\s*"))),
        type = input$life_type,
        topleftlabel = input$life_topleftlabel,
        title = input$life_title,
        footnote = input$life_footnote
      )
    }))
  })
}

## To be copied in the UI
# mod_lifetest_ui("lifetest_1")

## To be copied in the server
# mod_lifetest_server("lifetest_1")
