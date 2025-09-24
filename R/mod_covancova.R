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
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 因变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("dep_var"), "因变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择因变量',
                                      maxItems = 1))
      ),

      # 因变量标签
      textInput(ns("dep_label"), "因变量标签", value = "",
                placeholder = "例如: 用药后6周±3天",
                width = "100%"),

      # 中心变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("site_var"), "中心变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择中心变量'))
      ),

      # 中心变量标签
      textInput(ns("site_label"), "中心变量标签", value = "",
                placeholder = "例如: 中心",
                width = "100%"),

      # 基线变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("base_var"), "基线变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择基线变量'))
      ),

      # 基线变量标签
      textInput(ns("base_label"), "基线变量标签", value = "",
                placeholder = "例如: 基线",
                width = "100%"),

      # 分组变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("group_var"), "分组变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择分组变量'))
      ),

      # 分组条件选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("group_cond"), "分组条件",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(placeholder = '选择分组条件（可多选）',
                                      maxItems = 10,
                                      plugins = list('remove_button'),
                                      create = FALSE,
                                      persist = FALSE))
      ),

      # 表格标题
      textInput(ns("title1"), "表格1标题",
                value = "协方差分析结果－因素分析",
                placeholder = "输入表格1标题",
                width = "100%"),

      textInput(ns("title2"), "表格2标题",
                value = "协方差分析结果－组间比较",
                placeholder = "输入表格2标题",
                width = "100%"),

      # 底注
      textInput(ns("footnote1"), "表格1底注",
                value = "",
                placeholder = "输入表格1底注",
                width = "100%"),

      textInput(ns("footnote2"), "表格2底注",
                value = "",
                placeholder = "输入表格2底注",
                width = "100%")
    )
  )
}

#' covancova Server Functions
#'
#' @noRd
mod_covancova_server <- function(id, data_upload_module){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 更新变量选择列表
    observe({
      req(data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data

      if (!is.null(current_data) && nrow(current_data) > 0 && ncol(current_data) > 0) {
        vars <- names(current_data)

        # 更新所有变量选择
        updateSelectizeInput(session, "dep_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择因变量'))

        updateSelectizeInput(session, "site_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择中心变量'))

        updateSelectizeInput(session, "base_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择基线变量'))

        updateSelectizeInput(session, "group_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分组变量'))
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_var, data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data
      group_var <- input$group_var

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(session, "group_cond",
                             choices = as.character(unique_values),
                             selected = input$group_cond,
                             options = list(placeholder = '选择分组条件',
                                            maxItems = 10))
      }
    })

    # 自动填充变量标签
    observe({
      # 因变量标签
      req(input$dep_var)
      if (input$dep_label == "") {
        updateTextInput(session, "dep_label", value = input$dep_var)
      }

      # 中心变量标签
      req(input$site_var)
      if (input$site_label == "") {
        updateTextInput(session, "site_label", value = input$site_var)
      }

      # 基线变量标签
      req(input$base_var)
      if (input$base_label == "") {
        updateTextInput(session, "base_label", value = input$base_var)
      }
    })

    return(reactive({
      req(input$dep_var, input$site_var, input$base_var, input$group_var)

      # 构建数据条件
      data_cond <- if (!is.null(data_upload_module()$filter_text) &&
                       data_upload_module()$filter_text != "") {
        data_upload_module()$filter_text
      } else {
        "TRUE"  # 默认选择所有行
      }

      # 构建分组条件
      group_c <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste0(input$group_var, "|", paste(input$group_cond, collapse = "/"))
      } else {
        input$group_var
      }

      # 构建变量列表
      varlist <- paste0(
        input$dep_var, "/", input$dep_label, "|",
        input$site_var, "/", input$site_label, "|",
        input$base_var, "/", input$base_label
      )

      list(
        data_cond = data_cond,
        group_c = group_c,
        varlist = varlist,
        title1 = input$title1,
        title2 = input$title2,
        footnote1 = input$footnote1,
        footnote2 = input$footnote2
      )
    }))
  })
}

## To be copied in the UI
# mod_covancova_ui("covancova_1")

## To be copied in the server
# mod_covancova_server("covancova_1")
