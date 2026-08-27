#' c_srt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats na.omit var
mod_c_srt_ui <- function(id) {
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

          # 变量映射
          textInput(ns("var_mapping"), "变量映射", value = "",
                    placeholder = "格式为:变量值1=变量值标签1/变量值2=变量值标签2/...",
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
          checkboxInput(ns("coltotal"), "显示列合计", value = FALSE),
          checkboxInput(ns("rowtotal"), "显示行合计", value = TRUE),
          checkboxInput(ns("outyn"), "是否立即出表", value = TRUE),
          checkboxInput(ns("test_between"), "组间比较", value = 2),
          checkboxInput(ns("test_in"), "组内比较", value = FALSE),
          textInput(ns("table_title"), "表格标题", value = "秩和检验结果"),
          textInput(ns("ftnote"), "表格底注", value = "")

            )
  )
}

#' c_srt Server Functions
#'
#' @noRd
mod_c_srt_server <- function(id, data_upload_module){
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
      message("🧹 清空秩和检验参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "var_name", selected = "")
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "var_label", value = "")
      updateTextInput(session, "var_mapping", value = "")
      updateTextInput(session, "table_title", value = "秩和检验结果")
      updateTextInput(session, "ftnote", value = "")

      # 重置复选框
      updateCheckboxInput(session, "coltotal", value = FALSE)
      updateCheckboxInput(session, "rowtotal", value = TRUE)
      updateCheckboxInput(session, "test_between", value = TRUE)
      updateCheckboxInput(session, "test_in", value = FALSE)
      updateCheckboxInput(session, "outyn", value = TRUE)

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 秩和检验参数已清空")
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
          updateSelectizeInput(session, "var_name", choices = vars, selected = "")
          updateSelectizeInput(session, "group_var", choices = vars, selected = "")
          updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

          # 🟢 修复：只有当是秩和检验、数据包含特定变量时才设置默认值
          if (current_analysis_type == "c_srt" && all(c("DIFF_TB", "arm3") %in% vars)) {
            message("🎯 设置秩和检验默认变量...")
            updateSelectizeInput(session, "var_name", selected = "DIFF_TB")
            updateTextInput(session, "var_label", value = "访视4")
            updateTextInput(session, "var_mapping", value = "-3=改善3个等级/-2=改善2个等级/-1=改善1个等级/0=无变化/1=加重1个等级/2=加重2个等级/3=加重3个等级")
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
      req(input$var_name, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      # 🟢 修复：添加安全检查
      current_label <- input$var_label
      if (is.null(current_label) || is.na(current_label) || current_label == "") {
        label_mapping <- list(
          "DIFF_TB" = "用药后18周±3天-基线",
          "CHANGE" = "变化值",
          "SCORE_DIFF" = "评分变化"
        )

        if (input$var_name %in% names(label_mapping)) {
          updateTextInput(session, "var_label", value = label_mapping[[input$var_name]])
        } else {
          updateTextInput(session, "var_label", value = input$var_name)
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
      varlist_text <- if (!is.null(input$var_label) && input$var_label != "" &&
                          !is.null(input$var_mapping) && input$var_mapping != "") {
        paste0(input$var_name, "|", input$var_label, "|", input$var_mapping)
      } else if (!is.null(input$var_label) && input$var_label != "") {
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
        varlist = varlist_text,
        group_c = group_cond_text,
        coltotal = as.integer(input$coltotal),
        rowtotal = as.integer(input$rowtotal),
        outyn = as.integer(input$outyn),
        test_between = as.integer(input$test_between),
        test_in = as.integer(input$test_in),
        table_title = input$table_title,
        ftnote = input$ftnote,

        # 🟢 新增：清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}


## To be copied in the UI
# mod_c_ui("c_1")

## To be copied in the server
# mod_c_server("c_1")
