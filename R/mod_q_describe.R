#' @description A shiny Module for quantitative variable descriptive statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# mod_q_describe_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # 原有的其他参数
#     textInput(ns("var_name"), "变量名", value = "HEIGHT", placeholder = "例如: AGE, HEIGHT, WEIGHT"),
#     textInput(ns("var_label"), "变量标签", value = "身高", placeholder = "例如: 年龄, 身高, 体重"),
#     textInput(ns("group_name"), "分组变量名", value = "arm3", placeholder = "例如: TRT01A, ARM"),
#     textInput(ns("group_cond"), "分组条件（用逗号分隔）", value = "对照组,试验组,安慰剂组", placeholder = "例如: 对照组,试验组"),
#     textInput(ns("table_title"), "表格标题", value = "描述性统计表", placeholder = "输入表格标题"),
#     textInput(ns("ftnote"), "底注", value = "我是底注", placeholder = "输入表格底注"),
#     checkboxInput(ns("totalyn"), "是否显示合计列", value = TRUE)
#   )
# }

mod_q_describe_ui <- function(id) {
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
                       options = list(placeholder = '选择要分析的变量',
                                      maxItems = 1))
      ),

      # 变量标签
      textInput(ns("var_label"), "变量标签", value = "",
                placeholder = "例如: 年龄, 身高, 体重",
                width = "100%"),

      # 分组变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("group_name"), "分组变量",
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
                                      plugins = list('remove_button'),  # 添加移除按钮
                                      create = FALSE,  # 禁止用户输入新选项
                                      persist = FALSE  # 移除后不保持选项
                                      )
                       ),
        tags$small(icon("info-circle"), "选择分组变量后，此处会自动显示可选项",
                   style = "color: #6c757d; font-size: 12px;")
      ),

      # 表格标题
      textInput(ns("table_title"), "表格标题",
                value = "描述性统计表",
                placeholder = "输入表格标题",
                width = "100%"),

      # 底注
      textInput(ns("ftnote"), "底注",
                value = "我是底注",
                placeholder = "输入表格底注",
                width = "100%"),

      # 合计列选项
      checkboxInput(ns("totalyn"), "显示合计列", value = TRUE)
    )
  )
}

#' q_describe Server Functions
#'
#' @noRd
mod_q_describe_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
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

        # 更新分析变量选择，并设置第一个变量为默认值
        updateSelectizeInput(session, "var_name",
                             choices = vars,
                             server = TRUE,
                             selected = ifelse(length(vars) > 0, vars[1], ""),
                             options = list(placeholder = '选择分析变量'))

        # 更新分组变量选择
        updateSelectizeInput(session, "group_name",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分组变量'))
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_name, data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data
      group_var <- input$group_name

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(session, "group_cond",
                             choices = as.character(unique_values),
                             selected = input$group_cond,  # 保持之前的选择
                             options = list(placeholder = '选择分组条件',
                                            maxItems = 10))
      }
    })

    # 返回参数列表
    reactive({

      req(input$var_name, input$group_name)

      group_cond_text <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste(input$group_cond, collapse = ",")
      } else {
        ""  # 或者设置默认值
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          data_upload_module()$filter_text
        } else {
          "FAS!=''"  # 默认条件
        },
        var_name = input$var_name,
        var_label = input$var_label,
        group_name = input$group_name,
        group_cond = group_cond_text,
        table_title = input$table_title,
        ftnote = input$ftnote,
        totalyn = as.numeric(input$totalyn)
      )
    })
  })
}

## To be copied in the UI
# mod_q_describe_ui("q_describe_1")

## To be copied in the server
# mod_q_describe_server("q_describe_1")
