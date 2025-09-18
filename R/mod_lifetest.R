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
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("生存分析参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 时间变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("time_var"), "时间变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择时间变量'))
      ),

      # 时间变量标签
      textInput(ns("time_label"), "时间变量标签", value = "",
                placeholder = "例如: 流感症状缓解时间（h）",
                width = "100%"),

      # 删失变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("censor_var"), "删失变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择删失变量'))
      ),

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
                placeholder = "例如: 试验组/对照组",
                width = "100%"),

      # 时间点列表
      textInput(ns("timelist"), "时间点列表", value = "0,2,4,6,10,14,18,24,48,72",
                placeholder = "逗号分隔的时间点"),

      # 输出类型
      numericInput(ns("output_type"), "输出类型", value = 1, min = 0, max = 1),

      # 表格设置
      textInput(ns("topleftlabel"), "左列标签", value = "指标"),
      textInput(ns("title"), "表格标题", value = "生存分析结果"),
      textInput(ns("footnote"), "底注", value = "")
    )
  )
}

#' lifetest Server Functions
#'
#' @noRd
mod_lifetest_server <- function(id, data_upload_module){
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

        # 更新时间变量选择
        updateSelectizeInput(session, "time_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择时间变量'))

        # 更新删失变量选择
        updateSelectizeInput(session, "censor_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择删失变量'))

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

    # 自动填充时间变量标签
    observe({
      req(input$time_var)
      time_var <- input$time_var

      # 常见的时间变量标签映射
      label_mapping <- list(
        "lgzzhj" = "流感症状缓解时间（h）",
        "survival_time" = "生存时间",
        "time_to_event" = "事件发生时间"
      )

      if (time_var %in% names(label_mapping)) {
        updateTextInput(session, "time_label", value = label_mapping[[time_var]])
      } else if (input$time_label == "") {
        updateTextInput(session, "time_label", value = time_var)
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

      # 构建时间变量字符串
      time_label_text <- if (!is.null(input$time_label) && input$time_label != "") {
        paste0(input$time_var, "|", input$time_label)
      } else {
        input$time_var
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          paste0(data_upload_module()$data_name, "|", data_upload_module()$filter_text)
        } else {
          paste0(data_upload_module()$data_name, "|FAS=='是'")
        },
        group_c = group_cond_text,
        censor = input$censor_var,
        time_label = time_label_text,
        timelist = as.numeric(unlist(strsplit(input$timelist, ",\\s*"))),
        type = input$output_type,
        topleftlabel = input$topleftlabel,
        title = input$title,
        footnote = input$footnote
      )
    }))
  })
}

## To be copied in the UI
# mod_lifetest_ui("lifetest_1")

## To be copied in the server
# mod_lifetest_server("lifetest_1")
