#' pic_vline UI Function
#'
#' @description A shiny Module for line chart plotting.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats na.omit var
mod_pic_vline_ui <- function(id) {
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
                       options = list(placeholder = '选择分组变量',
                                      maxItems = 1))
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

      # X轴变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("xvar"), "X轴变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择X轴变量（需为数值型）',
                                      maxItems = 1))
      ),

      # Y轴变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("yvar"), "Y轴变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择Y轴变量（需为数值型）',
                                      maxItems = 1))
      ),

      # 统计量类型
      selectInput(ns("stat"), "统计量类型",
                  choices = c("均值" = "mean", "中位数" = "median",
                              "频数" = "freq", "百分比" = "percent",
                              "求和" = "sum"),
                  selected = "mean",
                  width = "100%"),

      # X轴标签
      textInput(ns("xlabel"), "X轴标签", value = "",
                placeholder = "设置X轴标签",
                width = "100%"),

      # Y轴标签
      textInput(ns("ylabel"), "Y轴标签", value = "",
                placeholder = "设置Y轴标签",
                width = "100%"),

      # 颜色
      textInput(ns("color"), "颜色(可选)", value = "",
                placeholder = "用|分隔，如: red|blue|green",
                width = "100%"),

      # 线型
      textInput(ns("lintype"), "线型(可选)", value = "",
                placeholder = "用|分隔，如: solid|dashed|dotted",
                width = "100%"),

      # 数据点形状
      textInput(ns("marktype"), "数据点形状(可选)", value = "",
                placeholder = "用|分隔，如: circle|triangle|square",
                width = "100%"),

      # 误差线
      selectInput(ns("limitstat"), "误差线",
                  choices = c("无" = "", "CLM" = "CLM",
                              "STDDEV" = "STDDEV",
                              "STDERR" = "STDERR"),
                  selected = "",
                  width = "100%"),

      # 显示数据标签
      selectInput(ns("showdata"), "显示数据标签",
                  choices = c("是" = "TRUE", "否" = "FALSE"),
                  selected = "FALSE",
                  width = "100%"),

      # 图表标题
      textInput(ns("table_title"), "图表标题",
                value = "折线图",
                placeholder = "输入图表标题",
                width = "100%"),

      # 图表脚注
      textInput(ns("ftnote"), "图表脚注",
                value = "",
                placeholder = "输入图表脚注",
                width = "100%")

    )
  )
}

#' pic_vline Server Functions
#'
#' @noRd
mod_pic_vline_server <- function(id, data_upload_module) {
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
      message("清空折线图参数")

      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")
      updateSelectizeInput(session, "xvar", selected = "")
      updateSelectizeInput(session, "yvar", selected = "")

      # 清空文本输入
      updateTextInput(session, "xlabel", value = "")
      updateTextInput(session, "ylabel", value = "")
      updateTextInput(session, "color", value = "")
      updateTextInput(session, "lintype", value = "")
      updateTextInput(session, "marktype", value = "")
      updateTextInput(session, "table_title", value = "折线图")
      updateTextInput(session, "ftnote", value = "")

      # 重置选择输入
      updateSelectInput(session, "stat", selected = "mean")
      updateSelectInput(session, "limitstat", selected = "")
      updateSelectInput(session, "showdata", selected = "FALSE")

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("清空参数完成，重置标志")
      })

      message("折线图参数已清空")
      return(TRUE)
    }

    # 数据初始化观察器
    observe({
      if (rv$clearing_params) {
        message("正在清空参数，跳过初始化")
        return()
      }

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
          updateSelectizeInput(session, "xvar", choices = vars, selected = "")
          updateSelectizeInput(session, "yvar", choices = vars, selected = "")

          # 设置默认值（大小写不敏感检测）
          if (current_analysis_type == "pic_vline") {
            message("设置折线图默认变量...")

            find_var <- function(data_vars, candidates) {
              for (v in candidates) {
                idx <- which(tolower(data_vars) == tolower(v))
                if (length(idx) > 0) return(data_vars[idx[1]])
              }
              return(NULL)
            }

            # 检测分组变量 (arm3)
            detected_group <- find_var(vars, c("arm3", "ARM3"))
            if (!is.null(detected_group)) {
              updateSelectizeInput(session, "group_var", selected = detected_group)

              if (detected_group %in% names(current_data)) {
                unique_groups <- unique(na.omit(current_data[[detected_group]]))
                updateSelectizeInput(session, "group_cond",
                                     choices = as.character(unique_groups),
                                     selected = as.character(unique_groups))
              }
            }

            # 检测X轴变量 (visitnum)
            detected_xvar <- find_var(vars, c("visitnum", "VISITNUM", "VISIT", "visit"))
            if (!is.null(detected_xvar)) {
              updateSelectizeInput(session, "xvar", selected = detected_xvar)
              updateTextInput(session, "xlabel", value = "访视阶段")
            }

            # 检测Y轴变量 (day)
            detected_yvar <- find_var(vars, c("day", "DAY", "ADY", "ady"))
            if (!is.null(detected_yvar)) {
              updateSelectizeInput(session, "yvar", selected = detected_yvar)
              updateTextInput(session, "ylabel", value = "平均天数")
            }
          }

          # 更新状态跟踪
          rv$last_data_hash <- current_data_hash
          rv$last_analysis_type <- current_analysis_type
          rv$init_done <- TRUE

          message("折线图初始化完成，分析类型: ", current_analysis_type)
        }
      } else {
        if (rv$init_done) {
          message("数据为空，但保持参数不变（等待用户明确清空）")
          rv$init_done <- FALSE
          rv$last_data_hash <- NULL
          rv$last_analysis_type <- NULL
        }
      }
    })

    # 监听分组变量变化，更新分组条件选项
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

    # 返回参数列表
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
        ""
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
        group = group_cond_text,
        color = input$color,
        lintype = input$lintype,
        marktype = input$marktype,
        title = input$table_title,
        stat = input$stat,
        xvar = input$xvar,
        yvar = input$yvar,
        xlabel = input$xlabel,
        ylabel = input$ylabel,
        showdata = as.logical(input$showdata),
        footnote = input$ftnote,
        limitstat = input$limitstat,

        # 清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}

## To be copied in the UI
# mod_pic_vline_ui("pic_vline_1")

## To be copied in the server
# mod_pic_vline_server("pic_vline_1")
