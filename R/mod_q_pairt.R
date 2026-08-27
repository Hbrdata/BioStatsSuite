#' q_pairt UI Function
#'
#' @description A shiny Module for paired t-test descriptive statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats na.omit var
mod_q_pairt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

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
                       options = list(
                         placeholder = '选择分组条件（可多选）',
                         maxItems = 10,
                         plugins = list('remove_button'),
                         create = FALSE
                       )
        ),
        tags$small(icon("info-circle"), "选择分组变量后，此处会自动显示可选项",
                   style = "color: #6c757d; font-size: 12px;")
      ),

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
                placeholder = "定义分析变量展示标签",
                width = "100%"),

      # 输出N标签
      checkboxInput(ns("outn"), "输出N标签", value = TRUE),

      # 显示列合计
      checkboxInput(ns("rowtotal"), "显示列合计", value = FALSE),

      # 配对t检验
      checkboxInput(ns("pairt"), "配对t检验", value = TRUE),

      # 表格标题
      textInput(ns("table_title"), "表格标题",
                value = "配对t检验分析结果",
                placeholder = "输入表格标题",
                width = "100%"),

      # 是否立即出表
      checkboxInput(ns("outyn"), "是否立即出表", value = TRUE),

      # 表格底注
      textInput(ns("ftnote"), "表格底注",
                value = "",
                placeholder = "输入表格底注",
                width = "100%")

    )
  )
}

#' q_pairt Server Functions
#'
#' @noRd
mod_q_pairt_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 使用响应式值跟踪数据和分析类型状态
    rv <- reactiveValues(
      last_data_hash = NULL,
      last_analysis_type = NULL,
      init_done = FALSE,
      clearing_params = FALSE,
      params_cleared = FALSE
    )

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 清空参数的方法
    clear_parameters <- function() {
      message("清空配对t检验参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")
      updateSelectizeInput(session, "var_name", selected = "")

      # 清空文本输入
      updateTextInput(session, "var_label", value = "")
      updateTextInput(session, "table_title", value = "配对t检验分析结果")
      updateTextInput(session, "ftnote", value = "")

      # 重置选择输入
      updateCheckboxInput(session, "outn", value = TRUE)
      updateCheckboxInput(session, "rowtotal", value = FALSE)
      updateCheckboxInput(session, "pairt", value = TRUE)
      updateCheckboxInput(session, "outyn", value = TRUE)

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("清空参数完成，重置标志")
      })

      message("配对t检验参数已清空")
      return(TRUE)
    }

    # 改进的数据初始化观察器 - 只在有数据时初始化，不自动清空参数
    observe({
      # 如果正在清空参数，跳过初始化
      if (rv$clearing_params) {
        message("正在清空参数，跳过初始化")
        return()
      }

      # 只有当有数据时才进行初始化
      if (!is.null(data_upload_module()$current_data) &&
          !is.null(data_upload_module()$current_analysis_type)) {

        current_data <- data_upload_module()$current_data
        current_analysis_type <- data_upload_module()$current_analysis_type

        # 生成数据哈希
        current_data_hash <- digest::digest(list(
          dim(current_data),
          names(current_data),
          current_analysis_type
        ))

        # 检查是否需要重新初始化
        needs_reinit <- FALSE

        if (!identical(current_data_hash, rv$last_data_hash)) {
          message("检测到数据变化，需要重新初始化")
          needs_reinit <- TRUE
        }

        if (!identical(current_analysis_type, rv$last_analysis_type)) {
          message("检测到分析类型变化，需要重新初始化")
          needs_reinit <- TRUE
        }

        if (!rv$init_done) {
          message("首次初始化")
          needs_reinit <- TRUE
        }

        if (needs_reinit && nrow(current_data) > 0) {
          vars <- names(current_data)

          # 更新变量选项
          updateSelectizeInput(session, "group_var", choices = vars, selected = "")
          updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")
          updateSelectizeInput(session, "var_name", choices = vars, selected = "")

          # 更新状态跟踪
          rv$last_data_hash <- current_data_hash
          rv$last_analysis_type <- current_analysis_type
          rv$init_done <- TRUE

          message("配对t检验初始化完成，分析类型: ", current_analysis_type)
        }
      } else {
        # 数据为空时，只更新状态，不清空参数输入
        if (rv$init_done) {
          message("数据为空，但保持参数不变（等待用户明确清空）")
          # 只重置初始化状态，不清空输入
          rv$init_done <- FALSE
          rv$last_data_hash <- NULL
          rv$last_analysis_type <- NULL
        }
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_var, data_upload_module()$current_data, rv$init_done)

      # 如果正在清空参数，跳过
      if (rv$clearing_params) {
        return()
      }

      current_data <- data_upload_module()$current_data
      group_var <- input$group_var

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(
          session,
          "group_cond",
          choices = as.character(unique_values),
          selected = character(0),
          options = list(
            placeholder = '选择分组条件',
            maxItems = 10,
            plugins = list('remove_button')
          )
        )
      }
    })

    return(reactive({
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
        ""
      }

      # 构建变量列表字符串
      varlist_text <- if (!is.null(input$var_label) && input$var_label != "") {
        paste0(input$var_name, "|", input$var_label)
      } else {
        input$var_name
      }

      # Check if the grouping condition is empty
      if (input$group_var != "" && (is.null(input$group_cond) || length(input$group_cond) == 0 || all(input$group_cond == ""))) {
        stop("分组条件无效或为空.请选择")
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          data_upload_module()$filter_text
        } else {
          "TRUE"
        },
        group = group_cond_text,
        varlist = varlist_text,
        outn = as.integer(input$outn),
        rowtotal = as.integer(input$rowtotal),
        pairt = as.integer(input$pairt),
        title = input$table_title,
        footnote = input$ftnote,
        outyn = as.integer(input$outyn),
        denominator_data = NULL,

        # 清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}


## To be copied in the UI
# mod_q_pairt_ui("q_pairt_1")

## To be copied in the server
# mod_q_pairt_server("q_pairt_1")
