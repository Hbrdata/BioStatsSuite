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
      h5("分析参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

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
                placeholder = "为时间变量定义标签",
                width = "100%"),

      # 删失变量选择 - 使用 selectizeInput
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("censor_var"), "删失变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择删失变量'))
      ),

      # 分组变量选择 - 使用 selectizeInput
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

      # 分组标签
      # textInput(ns("group_labels"), "分组标签", value = "",
      #           placeholder = "格式为:分组标签1/分组标签2/...",
      #           width = "100%"),

      # 时间点列表
      textInput(ns("timelist"), "时间点列表", value = "",
                placeholder = "逗号分隔的时间点"),

      # 输出类型
      numericInput(ns("output_type"), "输出类型", value = 1, min = 0, max = 1),

      # 表格设置
      textInput(ns("topleftlabel"), "左列标签", value = "指标"),
      textInput(ns("title"), "表格标题", value = "生存分析结果"),
      textInput(ns("footnote"), "表格底注", value = "")
    )
  )
}

#' lifetest Server Functions
#'
#' @noRd
mod_lifetest_server <- function(id, data_upload_module){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 修改：使用响应式值跟踪数据和分析类型状态
    rv <- reactiveValues(
      last_data_hash = NULL,  # 使用数据哈希来检测数据变化
      last_analysis_type = NULL,
      init_done = FALSE,
      clearing_params = FALSE,  # 🟢 标记是否正在清空参数
      params_cleared = FALSE    # 🟢 新增：标记参数是否已被清空
    )

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 🟢 修复：清空参数的方法
    clear_parameters <- function() {
      message("🧹 清空生存分析参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE
      rv$params_cleared <- TRUE  # 🟢 标记参数已清空

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 清空所有选择输入
      updateSelectizeInput(session, "time_var", selected = "")
      updateSelectizeInput(session, "censor_var", selected = "")
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "time_label", value = "")
      updateTextInput(session, "group_labels", value = "")
      updateTextInput(session, "timelist", value = "")
      updateTextInput(session, "topleftlabel", value = "指标")
      updateTextInput(session, "title", value = "生存分析结果")
      updateTextInput(session, "footnote", value = "")

      # 重置数值输入
      updateNumericInput(session, "output_type", value = 1)

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 生存分析参数已清空")
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

      # 生成数据哈希（基于数据维度和列名）
      current_data_hash <- digest::digest(list(
        dim(current_data),
        names(current_data),
        current_analysis_type
      ))

      # 检查是否需要重新初始化
      needs_reinit <- FALSE

      # 条件1: 数据发生变化（新数据上传）
      if (!identical(current_data_hash, rv$last_data_hash)) {
        message("📊 检测到数据变化，需要重新初始化")
        needs_reinit <- TRUE
      }

      # 条件2: 分析类型发生变化
      if (!identical(current_analysis_type, rv$last_analysis_type)) {
        message("🔄 检测到分析类型变化，需要重新初始化")
        needs_reinit <- TRUE
      }

      # 条件3: 尚未初始化
      if (!rv$init_done) {
        message("⚙️ 首次初始化")
        needs_reinit <- TRUE
      }

      # 🟢 条件4: 参数已被清空，需要重新初始化
      if (rv$params_cleared) {
        message("🔄 参数已清空，需要重新初始化")
        needs_reinit <- TRUE
        rv$params_cleared <- FALSE  # 重置标记
      }

      if (needs_reinit && nrow(current_data) > 0) {
        vars <- names(current_data)

        # 更新变量选项
        updateSelectizeInput(session, "time_var", choices = vars, selected = "")
        updateSelectizeInput(session, "censor_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

        # 🟢 修复：只有当是生存分析、数据包含特定变量、且不是清空参数后的初始化时才设置默认值
        if (current_analysis_type == "lifetest" &&
            all(c("lgzzhj", "censor", "arm3") %in% vars) &&
            !rv$clearing_params) {
          message("🎯 设置生存分析默认变量...")
          updateSelectizeInput(session, "time_var", selected = "lgzzhj")
          updateTextInput(session, "time_label", value = "流感症状缓解时间（h）")
          updateSelectizeInput(session, "censor_var", selected = "censor")
          updateSelectizeInput(session, "group_var", selected = "arm3")
          updateTextInput(session, "timelist", value = "0,2,4,6,10,14,18,24,48,72")
        }

        # 更新状态跟踪
        rv$last_data_hash <- current_data_hash
        rv$last_analysis_type <- current_analysis_type
        rv$init_done <- TRUE

        message("✅ 初始化完成，分析类型: ", current_analysis_type)
      }
    })

    # 🟢 修复：监听分组变量变化（在数据初始化后执行）
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
          selected = character(0),  # 清空当前选择
          options = list(
            placeholder = '选择分组条件',
            maxItems = 10,
            plugins = list('remove_button')
          )
        )
      }
    })

    # 🟢 修复：自动填充时间变量标签（在数据初始化后执行）
    observe({
      req(input$time_var, rv$init_done)

      # 如果正在清空参数，跳过
      if (rv$clearing_params) {
        return()
      }

      time_var <- input$time_var
      current_label <- input$time_label

      # 只在标签为空时自动填充
      if (current_label == "") {
        label_mapping <- list(
          "lgzzhj" = "流感症状缓解时间（h）",
          "survival_time" = "生存时间",
          "time_to_event" = "事件发生时间"
        )

        if (time_var %in% names(label_mapping)) {
          updateTextInput(session, "time_label", value = label_mapping[[time_var]])
        } else {
          updateTextInput(session, "time_label", value = time_var)
        }
      }
    })

    # 🟢 修复：自动填充分组标签（在数据初始化后执行）
    observe({
      req(input$group_var, data_upload_module()$current_data, rv$init_done)

      # 如果正在清空参数，跳过
      if (rv$clearing_params) {
        return()
      }

      group_var <- input$group_var
      current_data <- data_upload_module()$current_data
      current_labels <- input$group_labels

      # 只在标签为空时自动填充
      if (current_labels == "" && group_var %in% names(current_data)) {
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
          data_upload_module()$filter_text
        } else {
          "TRUE"  # 默认选择所有行
        },
        group_c = group_cond_text,
        censor = input$censor_var,
        time_label = time_label_text,
        timelist = as.numeric(unlist(strsplit(input$timelist, ",\\s*"))),
        type = input$output_type,
        topleftlabel = input$topleftlabel,
        title = input$title,
        footnote = input$footnote,

        # 🟢 新增：清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}

## To be copied in the UI
# mod_lifetest_ui("lifetest_1")

## To be copied in the server
# mod_lifetest_server("lifetest_1")
