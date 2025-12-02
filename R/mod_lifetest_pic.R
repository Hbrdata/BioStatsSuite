#' lifetest_pic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lifetest_pic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("生存分析绘图参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

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

      # 删失变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("censor_var"), "删失变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择删失变量'))
      ),

      # 删失值设置
      numericInput(ns("censor_value"), "删失值", value = 0,
                   min = 0, max = 1, step = 1),

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

      # 时间点列表
      textInput(ns("timelist"), "时间点列表", value = "0,2,4,6,10,14,18,24,48,72",
                placeholder = "逗号分隔的时间点"),

      # 输出类型
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("output_type"), "输出类型",
                       choices = c("生存率" = 0, "失效率" = 1),
                       selected = 0,
                       multiple = FALSE,
                       options = list(placeholder = '选择输出类型'))
      ),

      # Y轴标签
      textInput(ns("y_label"), "Y轴标签", value = "生存率(%)",
                placeholder = "设置Y轴标签",
                width = "100%"),

      # 图片标题
      textInput(ns("pic_title"), "图片标题", value = "生存分析图",
                placeholder = "设置图片标题",
                width = "100%")

    )
  )
}

#' lifetest_pic Server Functions
#'
#' @noRd
mod_lifetest_pic_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 响应式值跟踪状态
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
      message("🧹 清空生存分析绘图参数")

      rv$clearing_params <- TRUE
      rv$params_cleared <- TRUE

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
      updateTextInput(session, "y_label", value = "生存率(%)")
      updateTextInput(session, "pic_title", value = "生存分析图")
      updateTextInput(session, "timelist", value = "")

      # 重置数值输入
      updateNumericInput(session, "censor_value", value = 0)
      updateSelectizeInput(session, "output_type", selected = 0)

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 生存分析绘图参数已清空")
      return(TRUE)
    }

    # 数据初始化观察器
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
        updateSelectizeInput(session, "time_var", choices = vars, selected = "")
        updateSelectizeInput(session, "censor_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

        # 设置默认值（如果是生存分析且包含特定变量）
        if (current_analysis_type == "lifetest_pic" &&
            all(c("lgzzhj", "censor", "arm3") %in% vars) &&
            !rv$clearing_params) {
          message("🎯 设置生存分析绘图默认变量...")
          updateSelectizeInput(session, "time_var", selected = "lgzzhj")
          updateTextInput(session, "time_label", value = "时间（h）")
          updateSelectizeInput(session, "censor_var", selected = "censor")
          updateSelectizeInput(session, "group_var", selected = "arm3")
          updateTextInput(session, "timelist", value = "0,2,4,6,10,14,18,24,48,72")
          updateTextInput(session, "y_label", value = "流感症状未缓解率(%)")
          updateTextInput(session, "pic_title", value = "各时点流感症状缓解率的Kaplan-Meier估计（FAS）")
        }

        # 更新状态跟踪
        rv$last_data_hash <- current_data_hash
        rv$last_analysis_type <- current_analysis_type
        rv$init_done <- TRUE

        message("✅ 生存分析绘图初始化完成，分析类型: ", current_analysis_type)
      }
    })

    # 监听分组变量变化
    observe({
      req(input$group_var, data_upload_module()$current_data, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      current_data <- data_upload_module()$current_data
      group_var <- input$group_var

      if (group_var %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

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

    # 自动填充时间变量标签
    observe({
      req(input$time_var, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      time_var <- input$time_var
      current_label <- input$time_label

      if (is.null(current_label) || current_label == "") {
        label_mapping <- list(
          "lgzzhj" = "时间（h）",
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

      # 取消:(只在标签为空时自动填充 current_labels == "" && )
      if (group_var %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[group_var]]))
        if (length(unique_values) <= 5) {
          labels <- paste(unique_values, collapse = "/")
          updateTextInput(session, "group_labels", value = labels)
        }
      }
    })

    return(reactive({
      # 构建分组条件字符串
      group_cond_text <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste0(input$group_var, "|", paste(input$group_cond, collapse = "/"))
      } else if (!is.null(input$group_var)) {
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

      # 验证分组条件
      if (input$group_var != "" && (is.null(input$group_cond) || length(input$group_cond) == 0 || all(input$group_cond == ""))) {
        stop("分组条件无效或为空，请选择分组条件")
      }

      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          data_upload_module()$filter_text
        } else {
          "TRUE"
        },
        group_c = group_cond_text,
        censor = input$censor_var,
        type = as.numeric(input$output_type),
        time_label = time_label_text,
        timelist = as.numeric(unlist(strsplit(input$timelist, ",\\s*"))),
        censorvalue = as.numeric(input$censor_value),
        ylabel = input$y_label,
        pic_title = input$pic_title,

        # 清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}

## To be copied in the UI
# mod_lifetest_pic_ui("lifetest_pic_1")

## To be copied in the server
# mod_lifetest_pic_server("lifetest_pic_1")
