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
                placeholder = "定义分析变量展示标签",
                width = "100%"),

      # 分组变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("group_var"), "分组变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择分组变量'))
      ),

      # 🟢 修复：分组条件选择器配置
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

      # 分组标签
      # textInput(ns("group_labels"), "分组标签", value = "",
      #           placeholder = "格式为:分组标签1/分组标签2/...",
      #           width = "100%"),

      # 统计选项
      checkboxInput(ns("rowtotal"), "显示行合计", value = TRUE),
      checkboxInput(ns("pairt"), "配对t检验", value = FALSE),
      checkboxInput(ns("test_between"), "组间检验", value = TRUE),

      # 表格设置
      textInput(ns("title"), "表格标题", value = "定量参数分析"),
      textInput(ns("footnote"), "表格底注", value = "")
    )
  )
}

#' q_param Server Functions
#'
#' @noRd
mod_q_param_server <- function(id, data_upload_module){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 🟢 新增：使用响应式值跟踪数据和分析类型状态
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

    # 🟢 新增：清空参数的方法
    clear_parameters <- function() {
      message("🧹 清空定量参数分析参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE
      rv$params_cleared <- TRUE

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 清空所有选择输入
      updateSelectizeInput(session, "var_name", selected = "")
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "var_label", value = "")
      updateTextInput(session, "group_labels", value = "")
      updateTextInput(session, "title", value = "定量参数分析")
      updateTextInput(session, "footnote", value = "")

      # 重置复选框
      updateCheckboxInput(session, "rowtotal", value = TRUE)
      updateCheckboxInput(session, "pairt", value = FALSE)
      updateCheckboxInput(session, "test_between", value = TRUE)

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 定量参数分析参数已清空")
      return(TRUE)
    }

    # 🟢 修复：改进的数据初始化观察器
    observe({
      req(data_upload_module()$current_data, data_upload_module()$current_analysis_type)

      # 如果正在清空参数，跳过初始化
      if (rv$clearing_params) {
        message("⏸️ 正在清空参数，跳过初始化")
        return()
      }

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
        message("📊 检测到数据变化，需要重新初始化")
        needs_reinit <- TRUE
      }

      if (!identical(current_analysis_type, rv$last_analysis_type)) {
        message("🔄 检测到分析类型变化，需要重新初始化")
        needs_reinit <- TRUE
      }

      if (!rv$init_done) {
        message("⚙️ 首次初始化")
        needs_reinit <- TRUE
      }

      if (rv$params_cleared) {
        message("🔄 参数已清空，需要重新初始化")
        needs_reinit <- TRUE
        rv$params_cleared <- FALSE
      }

      if (needs_reinit && nrow(current_data) > 0) {
        vars <- names(current_data)

        # 更新变量选项
        updateSelectizeInput(session, "var_name", choices = vars, selected = "")
        updateSelectizeInput(session, "group_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

        # 🟢 修复：只有当是定量参数分析、数据包含特定变量、且不是清空参数后的初始化时才设置默认值
        if (current_analysis_type == "q_param" &&
            all(c("URPVVtb", "arm3") %in% vars) &&
            !rv$clearing_params) {
          message("🎯 设置定量参数分析默认变量...")
          updateSelectizeInput(session, "var_name", selected = "URPVVtb")
          updateTextInput(session, "var_label", value = "基线")
          updateSelectizeInput(session, "group_var", selected = "arm3")
        }

        # 更新状态跟踪
        rv$last_data_hash <- current_data_hash
        rv$last_analysis_type <- current_analysis_type
        rv$init_done <- TRUE

        message("✅ 初始化完成，分析类型: ", current_analysis_type)
      }
    })

    # 🟢 监听分组变量变化，更新分组条件选项
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

        # 🟢 修复：正确更新分组条件选择
        updateSelectizeInput(
          session,
          "group_cond",
          choices = as.character(unique_values),
          selected = character(0),  # 清空当前选择
          options = list(
            placeholder = '选择分组条件',
            maxItems = 10,
            plugins = list('remove_button')
          )
        )
      }
    })

    # 🟢 修复：自动填充变量标签（在数据初始化后执行）
    observe({
      req(input$var_name, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

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

    # # 🟢 修复：自动填充分组标签（在数据初始化后执行）
    observe({
      req(input$group_var, data_upload_module()$current_data, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      group_var <- input$group_var
      current_data <- data_upload_module()$current_data

      if (group_var %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[group_var]]))
        if (length(unique_values) <= 5) {
          labels <- paste(unique_values, collapse = "/")
          updateTextInput(session, "group_labels", value = labels)
        }
      }
    })

    return(reactive({
      # req(data_upload_module()$current_data)

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
        denominator_cond = if (!is.null(data_upload_module()$denominator_filter_text) &&
                               data_upload_module()$denominator_filter_text != "") {
          data_upload_module()$denominator_filter_text
        } else {
          "TRUE"
        },
        group_c = group_cond_text,
        varlist = varlist_text,
        rowtotal = as.integer(input$rowtotal),
        pairt = as.integer(input$pairt),
        outyn = 1,
        test_between = as.integer(input$test_between),
        title = input$title,
        footnote = input$footnote,

        # 🟢 新增：清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}


## To be copied in the UI
# mod_q_param_ui("q_param_1")

## To be copied in the server
# mod_q_param_server("q_param_1")
