#' covancova UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_covancova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 因变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("dep_var"), "因变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择因变量',
                                      maxItems = 1))
      ),

      # 因变量标签
      textInput(ns("dep_label"), "因变量标签", value = "",
                placeholder = "定义因变量展示标签",
                width = "100%"),

      # 中心变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("site_var"), "中心变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择中心变量'))
      ),

      # 中心变量标签
      textInput(ns("site_label"), "中心变量标签", value = "",
                placeholder = "定义中心变量展示标签",
                width = "100%"),

      # 基线变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("base_var"), "基线变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择基线变量'))
      ),

      # 基线变量标签
      textInput(ns("base_label"), "基线变量标签", value = "",
                placeholder = "定义基线变量展示标签",
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

      # 表格标题
      textInput(ns("title1"), "表格1标题",
                value = "协方差分析结果－因素分析",
                placeholder = "输入表格1标题",
                width = "100%"),

      textInput(ns("title2"), "表格2标题",
                value = "协方差分析结果－组间比较",
                placeholder = "输入表格2标题",
                width = "100%"),

      # 底注
      textInput(ns("footnote1"), "表格1底注",
                value = "",
                placeholder = "输入表格1底注",
                width = "100%"),

      textInput(ns("footnote2"), "表格2底注",
                value = "",
                placeholder = "输入表格2底注",
                width = "100%")

    )
  )
}

#' covancova Server Functions
#'
#' @noRd
mod_covancova_server <- function(id, data_upload_module){
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
      message("🧹 清空协方差分析参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "dep_var", selected = "")
      updateSelectizeInput(session, "site_var", selected = "")
      updateSelectizeInput(session, "base_var", selected = "")
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "dep_label", value = "")
      updateTextInput(session, "site_label", value = "")
      updateTextInput(session, "base_label", value = "")
      updateTextInput(session, "title1", value = "协方差分析结果－因素分析")
      updateTextInput(session, "title2", value = "协方差分析结果－组间比较")
      updateTextInput(session, "footnote1", value = "")
      updateTextInput(session, "footnote2", value = "")

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 协方差分析参数已清空")
      return(TRUE)
    }

    # 🟢 修复：改进的数据初始化观察器 - 只在有数据时初始化，不自动清空参数
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
          needs_reinit <- TRUE
        }

        if (needs_reinit && nrow(current_data) > 0) {
          vars <- names(current_data)

          # 更新变量选项
          updateSelectizeInput(session, "dep_var", choices = vars, selected = "")
          updateSelectizeInput(session, "site_var", choices = vars, selected = "")
          updateSelectizeInput(session, "base_var", choices = vars, selected = "")
          updateSelectizeInput(session, "group_var", choices = vars, selected = "")
          updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

          # 🟢 修复：只有当是协方差分析、数据包含特定变量时才设置默认值
          if (current_analysis_type == "covancova" && all(c("difftbsum", "SITEID", "TSORRES0sum", "arm3") %in% vars)) {
            message("🎯 设置协方差分析默认变量...")
            updateSelectizeInput(session, "dep_var", selected = "difftbsum")
            updateTextInput(session, "dep_label", value = "访视2")
            updateSelectizeInput(session, "site_var", selected = "SITEID")
            updateTextInput(session, "site_label", value = "中心")
            updateSelectizeInput(session, "base_var", selected = "TSORRES0sum")
            updateTextInput(session, "base_label", value = "基线")
            updateSelectizeInput(session, "group_var", selected = "arm3")

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

    # 🟢 修复：自动填充变量标签（在数据初始化后执行）
    observe({
      req(input$dep_var, rv$init_done)

      # 如果正在清空参数，跳过
      if (rv$clearing_params) {
        return()
      }

      if (input$dep_label == "") {
        label_mapping <- list(
          "SCORE" = "用药后6周±3天",
          "VALUE" = "数值",
          "CHANGE" = "变化值"
        )

        if (input$dep_var %in% names(label_mapping)) {
          updateTextInput(session, "dep_label", value = label_mapping[[input$dep_var]])
        } else {
          updateTextInput(session, "dep_label", value = input$dep_var)
        }
      }
    })

    observe({
      req(input$site_var, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      if (input$site_label == "") {
        label_mapping <- list(
          "SITEID" = "中心",
          "CENTER" = "中心",
          "SITE" = "中心"
        )

        if (input$site_var %in% names(label_mapping)) {
          updateTextInput(session, "site_label", value = label_mapping[[input$site_var]])
        } else {
          updateTextInput(session, "site_label", value = input$site_var)
        }
      }
    })

    observe({
      req(input$base_var, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      if (input$base_label == "") {
        label_mapping <- list(
          "URPVVtb" = "基线",
          "BASELINE" = "基线",
          "BASE" = "基线"
        )

        if (input$base_var %in% names(label_mapping)) {
          updateTextInput(session, "base_label", value = label_mapping[[input$base_var]])
        } else {
          updateTextInput(session, "base_label", value = input$base_var)
        }
      }
    })

    return(reactive({
      # req(data_upload_module()$current_data)

      # 构建数据条件
      data_cond <- if (!is.null(data_upload_module()$filter_text) &&
                       data_upload_module()$filter_text != "") {
        data_upload_module()$filter_text
      } else {
        "TRUE"
      }

      # 构建分组条件
      group_c <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
        paste0(input$group_var, "|", paste(input$group_cond, collapse = "/"))
      } else {
        input$group_var
      }

      # 构建变量列表
      varlist <- paste0(
        input$dep_var, "/", input$dep_label, "|",
        input$site_var, "/", input$site_label, "|",
        input$base_var, "/", input$base_label
      )

      # Check if the grouping condition is empty
      if (input$group_var != "" && (is.null(input$group_cond) || length(input$group_cond) == 0 || all(input$group_cond == ""))) {
        stop("分组条件无效或为空.请选择")
      }

      list(
        data_cond = data_cond,
        group_c = group_c,
        varlist = varlist,
        title1 = input$title1,
        title2 = input$title2,
        footnote1 = input$footnote1,
        footnote2 = input$footnote2,

        # 🟢 新增：清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}

## To be copied in the UI
# mod_covancova_ui("covancova_1")

## To be copied in the server
# mod_covancova_server("covancova_1")
