#' @description A shiny Module for quantitative variable descriptive statistics.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats na.omit var
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
                placeholder = "定义分析变量展示标签",
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
                       options = list(
                         placeholder = '选择分组条件（可多选）',
                         maxItems = 10,
                         plugins = list('remove_button'),
                         create = FALSE,
                         maxOptions = 100,  # 限制选项数量
                         loadThrottle = 300  # 减少卡顿
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
      textInput(ns("ftnote"), "表格底注",
                value = "我是底注",
                placeholder = "输入表格底注",
                width = "100%"),

      # 合计列选项
      checkboxInput(ns("totalyn"), "显示合计列", value = TRUE),

      # 🆕：表格叠加选项
      checkboxInput(ns("outyn"), "是否叠加表格", value = TRUE)
    )
  )
}

#' q_describe Server Functions
#'
#' @noRd
mod_q_describe_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
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

    # 🟢 修复：清空参数的方法 - 只在明确调用时执行
    clear_parameters <- function() {

      # 设置清空参数标志
      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "var_name", selected = "")
      updateSelectizeInput(session, "group_name", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "var_label", value = "")
      updateTextInput(session, "table_title", value = "描述性统计表")
      updateTextInput(session, "ftnote", value = "我是底注")

      # 重置复选框
      updateCheckboxInput(session, "totalyn", value = TRUE)

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      return(TRUE)
    }

    observe({
      # 如果正在清空参数，跳过初始化
      if (rv$clearing_params) {
        message("⏸️ 正在清空参数，跳过初始化")
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
          message("📊 检测到数据变化，需要重新初始化")
          needs_reinit <- TRUE
        }

        if (!identical(current_analysis_type, rv$last_analysis_type)) {
          message("🔄 检测到分析类型变化，需要重新初始化")
          needs_reinit <- TRUE
        }

        if (!rv$init_done) {
          message("⚙️ 首次初始化")
          needs_reinit = TRUE
        }

        if (needs_reinit && nrow(current_data) > 0) {
          vars <- names(current_data)

          # 更新变量选项
          updateSelectizeInput(session, "var_name", choices = vars, selected = "")
          updateSelectizeInput(session, "group_name", choices = vars, selected = "")
          updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

          # 🟢 修复：只有当是描述性统计、数据包含特定变量时才设置默认值
          if (current_analysis_type == "q_describe" && all(c("HEIGHT", "arm3") %in% vars)) {
            message("🎯 设置描述性统计默认变量...")
            updateSelectizeInput(session, "var_name", selected = "HEIGHT")
            updateTextInput(session, "var_label", value = "身高")
            updateSelectizeInput(session, "group_name", selected = "arm3")

            # 自动设置分组条件
            if ("arm3" %in% names(current_data)) {
              unique_groups <- unique(na.omit(current_data[["arm3"]]))
              updateSelectizeInput(session, "group_cond",
                                   choices = as.character(unique_groups),
                                   selected = as.character(unique_groups))
            }
          }

          # 更新状态跟踪
          rv$last_data_hash <- current_data_hash
          rv$last_analysis_type <- current_analysis_type
          rv$init_done <- TRUE

          message("✅ 初始化完成，分析类型: ", current_analysis_type)
        }
      } else {
        # 🟢 修改：数据为空时，只更新状态，不清空参数输入
        # 这样参数会保持原样，直到用户明确点击清空参数按钮
        if (rv$init_done) {
          message("📭 数据为空，但保持参数不变（等待用户明确清空）")
          # 只重置初始化状态，不清空输入
          rv$init_done <- FALSE
          rv$last_data_hash <- NULL
          rv$last_analysis_type <- NULL
        }
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_name, data_upload_module()$current_data, rv$init_done)

      # 如果正在清空参数，跳过
      if (rv$clearing_params) {
        return()
      }

      current_data <- data_upload_module()$current_data
      group_var <- input$group_name

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
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

      if (input$var_label == "") {
        label_mapping <- list(
          "age" = "年龄",
          "height" = "身高",
          "weight" = "体重",
          "SCORE" = "评分"
        )

        if (input$var_name %in% names(label_mapping)) {
          updateTextInput(session, "var_label", value = label_mapping[[input$var_name]])
        } else {
          updateTextInput(session, "var_label", value = input$var_name)
        }
      }
    })

    # 返回参数列表
    return(reactive({
      # req(input$var_name, input$group_name)

      group_cond_text <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste(input$group_cond, collapse = ",")
      } else {
        ""
      }

      group_cond_processed <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        if (is.character(input$group_cond) && length(input$group_cond) == 1) {
          # 分割字符串并去除前后空格
          group_cond <- unlist(strsplit(input$group_cond, ",\\s*"))
          group_cond <- trimws(group_cond)

          # 处理可能的中文引号或其他特殊字符
          group_cond <- gsub("['\"`]", "", group_cond)  # 移除引号
          group_cond
        } else {
          # 如果已经是向量，直接使用
          input$group_cond
        }
      } else {
        character(0)
      }

      # Check if the grouping condition is empty
      if (input$group_name != "" && (is.null(input$group_cond) || length(input$group_cond) == 0 || all(input$group_cond == ""))) {
        stop("分组条件无效或为空.请选择")
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          data_upload_module()$filter_text
        } else {
          "TRUE"
        },
        var_name = input$var_name,
        var_label = input$var_label,
        group_name = input$group_name,
        group_cond = group_cond_processed,
        table_title = input$table_title,
        ftnote = input$ftnote,
        totalyn = as.numeric(input$totalyn),
        # 🆕 新增：叠加表格选项
        outyn = as.numeric(input$outyn),


        clear_params = clear_parameters
      )
    }))
  })
}

## To be copied in the UI
# mod_q_describe_ui("q_describe_1")

## To be copied in the server
# mod_q_describe_server("q_describe_1")
