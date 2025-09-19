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
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 分析变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("var_name"), "分析变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择要分析的变量'))
      ),

      # 变量标签
      textInput(ns("var_label"), "变量标签", value = "",
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
                                      persist = FALSE)
        ),
        tags$small(icon("info-circle"), "选择分组变量后，此处会自动显示可选项",
                   style = "color: #6c757d; font-size: 12px;")
      ),

      # 分组标签
      textInput(ns("group_labels"), "分组标签", value = "",
                placeholder = "例如: 大剂量组/小剂量组/零剂量组",
                width = "100%"),

      # 统计选项
      checkboxInput(ns("rowtotal"), "显示行合计", value = TRUE),
      checkboxInput(ns("pairt"), "配对t检验", value = FALSE),
      checkboxInput(ns("test_between"), "组间检验", value = TRUE),

      # 表格设置
      textInput(ns("title"), "表格标题", value = "定量参数分析"),
      textInput(ns("footnote"), "底注", value = "")
    )
  )
}

#' q_param Server Functions
#'
#' @noRd
mod_q_param_server <- function(id, data_upload_module){
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

        # 更新分析变量选择
        updateSelectizeInput(session, "var_name",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分析变量'))

        # 更新分组变量选择
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
      req(input$var_name)
      var_name <- input$var_name

      # 常见的变量标签映射
      label_mapping <- list(
        "URPVVtb" = "基线",
        "SCORE" = "评分",
        "VALUE" = "数值",
        "CHANGE" = "变化值"
      )

      if (var_name %in% names(label_mapping)) {
        updateTextInput(session, "var_label", value = label_mapping[[var_name]])
      } else if (input$var_label == "") {
        updateTextInput(session, "var_label", value = var_name)
      }
    })

    # 自动填充分组标签
    observe({
      req(input$group_var, data_upload_module()$current_data)
      group_var <- input$group_var
      current_data <- data_upload_module()$current_data

      if (group_var %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[group_var]]))
        if (length(unique_values) <= 5) {  # 只对少量分组的变量自动生成标签
          labels <- paste(unique_values, collapse = "/")
          updateTextInput(session, "group_labels", value = labels)
        }
      }
    })

    return(reactive({
      req(data_upload_module()$current_data)

      # 构建分组条件字符串
      group_cond_text <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste0(input$group_var, "|", paste(input$group_cond, collapse = "/"))
      } else if (!is.null(input$group_var)) {
        # 如果没有选择具体条件，使用所有唯一值
        current_data <- data_upload_module()$current_data
        if (!is.null(current_data) && input$group_var %in% names(current_data)) {
          unique_values <- unique(na.omit(current_data[[input$group_var]]))
          paste0(input$group_var, "|", paste(sort(unique_values), collapse = "/"))
        } else {
          input$group_var
        }
      } else {
        ""  # 默认值
      }

      # 构建变量列表字符串
      varlist_text <- if (!is.null(input$var_label) && input$var_label != "") {
        paste0(input$var_name, "|", input$var_label)
      } else {
        input$var_name
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          paste0(data_upload_module()$data_name, "|", data_upload_module()$filter_text)
        } else {
          data_upload_module()$data_name
        },
        denominator_cond = if (!is.null(data_upload_module()$filter_text) &&
                               data_upload_module()$filter_text != "") {
          paste0(data_upload_module()$data_name, "|", data_upload_module()$filter_text)
        } else {
          data_upload_module()$data_name
        },
        group_c = group_cond_text,
        varlist = varlist_text,
        rowtotal = as.integer(input$rowtotal),
        pairt = as.integer(input$pairt),
        outyn = 1,
        test_between = as.integer(input$test_between),
        title = input$title,
        footnote = input$footnote
      )
    }))
  })
}

## To be copied in the UI
# mod_q_param_ui("q_param_1")

## To be copied in the server
# mod_q_param_server("q_param_1")
