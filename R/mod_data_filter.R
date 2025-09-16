#' Current Filter Display UI Function
#'
#' @description A shiny Module for displaying current filter conditions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_current_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 当前筛选条件显示
    tags$div(
      style = "background-color: #e8f4f8; padding: 15px; border-radius: 6px; border-left: 4px solid #3498db;",
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        tags$strong("当前筛选条件:", style = "color: #2c3e50; font-size: 14px;"),
        tags$span(icon("info-circle"), style = "color: #6c757d;")
      ),
      tags$div(
        style = "background-color: white; padding: 12px; border-radius: 4px; border: 1px solid #dee2e6; min-height: 60px; max-height: 120px; overflow-y: auto; font-family: monospace;",
        verbatimTextOutput(ns("current_filter"))
      ),
      # 添加条件关系显示
      tags$div(
        style = "margin-top: 10px; font-size: 12px; color: #6c757d;",
        textOutput(ns("relation_info"))
      )
    )
  )
}

#' data_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("数据筛选", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 筛选条件构建区域
      tags$div(
        style = "background-color: white; padding: 15px; border-radius: 6px; margin-bottom: 15px; border: 1px solid #eee;",
        fluidRow(
          column(4,
                 selectizeInput(ns("filter_var"), "选择变量",
                                choices = NULL, multiple = FALSE,
                                options = list(placeholder = '选择要筛选的变量'))
          ),
          column(3,
                 selectInput(ns("filter_operator"), "运算符",
                             choices = c("等于 ==", "不等于 !=", "包含 %in%", "大于 >", "大于等于 >=", "小于 <", "小于等于 <="))
          ),
          column(5,
                 conditionalPanel(
                   condition = paste0("input['", ns("filter_operator"), "'] != '包含 %in%'"),
                   textInput(ns("filter_value"), "筛选值", value = "",
                             placeholder = "例如: Y 或 1 或 '文本'")
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("filter_operator"), "'] == '包含 %in%'"),
                   textInput(ns("filter_values"), "多个值(逗号分隔)", value = "",
                             placeholder = "例如: A,B,C 或 1,2,3")
                 )
          )
        ),

        # 条件关系选择
        fluidRow(
          column(12,
                 selectInput(ns("condition_relation"), "条件关系",
                             choices = c("并且 (AND)" = "AND", "或者 (OR)" = "OR"),
                             selected = "AND"),
                 style = "margin-top: 10px;"
          )
        ),

        # 操作按钮
        tags$div(
          style = "display: flex; gap: 10px; justify-content: center; margin-top: 15px;",
          actionButton(ns("add_filter"), "添加条件",
                       icon = icon("plus"),
                       style = "background-color: #3498db; color: white;"),
          actionButton(ns("apply_filter"), "应用筛选",
                       icon = icon("check"),
                       style = "background-color: #27ae60; color: white;"),
          actionButton(ns("clear_filters"), "清空条件",
                       icon = icon("trash"),
                       style = "background-color: #e74c3c; color: white;")
        )
      ),

      # 调用当前筛选条件显示模块
      mod_current_filter_ui(ns("current_filter_1"))
    )
  )
}

#' Current Filter Display Server Functions
#'
#' @noRd
mod_current_filter_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 显示当前筛选条件
    output$current_filter <- renderText({
      filter_text <- filter_data()$filter_text
      if (is.null(filter_text) || filter_text == "") {
        "暂无筛选条件"
      } else {
        filter_text
      }
    })

    # 显示条件关系信息
    output$relation_info <- renderText({
      conditions <- filter_data()$filter_conditions
      relations <- filter_data()$condition_relations

      if (length(conditions) > 1) {
        paste("条件关系:", paste(relations, collapse = ", "))
      } else if (length(conditions) == 1) {
        "单个条件"
      } else {
        ""
      }
    })

    return(reactive({
      list(
        current_filter_text = filter_data()$filter_text,
        has_conditions = length(filter_data()$filter_conditions) > 0
      )
    }))
  })
}

#' data_filter Server Functions
#'
#' @noRd
mod_data_filter_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      filter_conditions = list(),      # 存储所有条件
      condition_relations = list(),    # 存储条件之间的关系
      current_filter_text = "",        # 完整的筛选表达式
      condition_count = 0,
      last_reset_trigger = 0  # 记录上次的重置信号
    )

    # 初始化当前筛选条件显示模块
    current_filter_module <- mod_current_filter_server("current_filter_1", reactive({
      list(
        filter_text = rv$current_filter_text,
        condition_relations = rv$condition_relations,
        filter_conditions = rv$filter_conditions
      )
    }))

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$raw_data) && nrow(data_upload_module()$raw_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 监听重置信号
    observe({
      req(data_upload_module()$reset_trigger)
      current_trigger <- data_upload_module()$reset_trigger

      # 如果重置信号有变化，清空筛选条件
      if (current_trigger != rv$last_reset_trigger) {
        rv$filter_conditions <- list()
        rv$condition_relations <- list()
        rv$current_filter_text <- ""
        rv$condition_count <- 0
        rv$last_reset_trigger <- current_trigger

        showNotification("筛选条件已重置", type = "message")
      }
    })

    # 更新变量选择列表
    observe({
      req(data_upload_module()$raw_data)
      current_data <- data_upload_module()$raw_data

      if (!is.null(current_data) && nrow(current_data) > 0 && ncol(current_data) > 0) {
        vars <- names(current_data)
        updateSelectizeInput(session, "filter_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择要筛选的变量'))
      }
    })

    # 添加筛选条件
    observeEvent(input$add_filter, {
      req(input$filter_var, input$filter_operator, data_upload_module()$raw_data)

      # 获取实际运算符
      operator_map <- c(
        "等于 ==" = "==",
        "不等于 !=" = "!=",
        "包含 %in%" = "%in%",
        "大于 >" = ">",
        "大于等于 >=" = ">=",
        "小于 <" = "<",
        "小于等于 <=" = "<="
      )
      actual_operator <- operator_map[input$filter_operator]

      # 验证输入
      if (actual_operator != "%in%" && input$filter_value == "") {
        showNotification("请输入筛选值", type = "warning")
        return()
      }

      if (actual_operator == "%in%" && input$filter_values == "") {
        showNotification("请输入筛选值", type = "warning")
        return()
      }

      # 构建条件表达式
      condition <- tryCatch({
        current_data <- data_upload_module()$raw_data

        if (actual_operator == "%in%") {
          values <- trimws(strsplit(input$filter_values, ",\\s*")[[1]])
          # 自动判断类型
          if (all(grepl("^\\d+(\\.\\d+)?$", values))) {
            # 数值类型
            paste0(input$filter_var, " %in% c(", paste(values, collapse = ","), ")")
          } else {
            # 字符类型
            paste0(input$filter_var, " %in% c('", paste(values, collapse = "','"), "')")
          }
        } else {
          # 简单操作符
          value <- input$filter_value
          var_type <- class(current_data[[input$filter_var]])

          if (var_type %in% c("numeric", "integer", "double")) {
            # 数值类型
            if (!grepl("^\\d+(\\.\\d+)?$", value)) {
              stop("数值变量需要输入数字")
            }
            paste0(input$filter_var, " ", actual_operator, " ", value)
          } else {
            # 字符类型
            paste0(input$filter_var, " ", actual_operator, " '", value, "'")
          }
        }
      }, error = function(e) {
        showNotification(paste("条件构建错误:", e$message), type = "error")
        return(NULL)
      })

      if (is.null(condition)) return()

      # 添加条件到列表
      rv$condition_count <- rv$condition_count + 1
      rv$filter_conditions <- c(rv$filter_conditions, condition)

      # 添加条件关系（第一个条件没有关系，后续条件使用选择的关系）
      if (rv$condition_count > 1) {
        rv$condition_relations <- c(rv$condition_relations, input$condition_relation)
      }

      # 更新筛选条件文本
      rv$current_filter_text <- build_filter_expression(rv$filter_conditions, rv$condition_relations)

      # 清空输入
      updateTextInput(session, "filter_value", value = "")
      updateTextInput(session, "filter_values", value = "")

      showNotification("筛选条件已添加", type = "message")
    })

    # 构建筛选表达式函数
    build_filter_expression <- function(conditions, relations) {
      if (length(conditions) == 0) {
        return("")
      }

      if (length(conditions) == 1) {
        return(conditions[[1]])
      }

      relation_map <- c("AND" = "&", "OR" = "|")

      expr <- paste0("(", conditions[[1]], ")")
      for (i in 2:length(conditions)) {
        raw_rel <- if (i <= length(relations) + 1) relations[[i - 1]] else "&"
        # 做一次映射：如果 raw_rel 是 "AND"/"OR" 则映射为 &/|；如果已经是 &/| 就原样使用
        rel <- if (!is.null(relation_map[[raw_rel]])) relation_map[[raw_rel]] else raw_rel
        expr <- paste0(expr, " ", rel, " (", conditions[[i]], ")")
      }
      expr
    }

    # 应用筛选条件
    observeEvent(input$apply_filter, {
      req(data_upload_module()$raw_data)

      if (rv$current_filter_text == "") {
        showNotification("请先添加筛选条件", type = "warning")
        return()
      }

      tryCatch({
        # 调试信息
        message("筛选条件: ", rv$current_filter_text)

        # 安全地执行筛选
        current_data <- data_upload_module()$raw_data
        filter_text <- rv$current_filter_text

        # 直接使用文本表达式
        filtered_df <- subset(current_data, eval(parse(text = filter_text)))

        if (nrow(filtered_df) == 0) {
          showNotification("筛选结果为空，请检查筛选条件", type = "warning")
          return()
        }

        # 更新筛选后的数据
        if (!is.null(data_upload_module()$updateFilteredData)) {
          data_upload_module()$updateFilteredData(filtered_df, filter_text)
        }

        showNotification(sprintf("筛选完成！从 %d 行筛选到 %d 行",
                                 nrow(current_data),
                                 nrow(filtered_df)),
                         type = "message")

      }, error = function(e) {
        error_msg <- paste("筛选错误:", e$message)
        showNotification(error_msg, type = "error")
        message(error_msg)
      })
    })

    observeEvent(input$clear_filters, {
      rv$filter_conditions <- list()
      rv$condition_relations <- list()
      rv$current_filter_text <- ""
      rv$condition_count <- 0
      showNotification("筛选条件已清空", type = "message")

      # 重置数据
      if (!is.null(data_upload_module()$updateFilteredData)) {
        data_upload_module()$updateFilteredData(NULL, "")
      }
    })

    return(reactive({
      list(
        current_filter_text = rv$current_filter_text,
        has_data = !is.null(data_upload_module()$raw_data)
      )
    }))
  })
}



## To be copied in the UI
# mod_data_filter_ui("data_filter_1")



## To be copied in the server
# mod_data_filter_server("data_filter_1")


