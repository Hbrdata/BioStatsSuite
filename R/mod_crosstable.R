#' crosstable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crosstable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 行变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("row_var"), "行变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择行变量',
                                      maxItems = 1))
      ),

      # 行变量标签
      textInput(ns("row_label"), "行变量标签", value = "",
                placeholder = "定义行变量展示标签",
                width = "100%"),

      # 列变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("col_var"), "列变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择列变量',
                                      maxItems = 1))
      ),

      # 列变量标签
      textInput(ns("col_label"), "列变量标签", value = "",
                placeholder = "定义列变量展示标签",
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

      # 缺失值填补
      textInput(ns("missing"), "缺失值填补", value = "", placeholder = "将缺失缺失值填补为XX；填写内容为格式定义中等号左边的内容"),
      # numericInput(ns("missing"), "缺失值填补", value = 4),

      # 格式定义
      textInput(ns("format"), "格式定义",
                value = "",
                placeholder = "分析变量的内容和标签；值1=标签1|值2=标签2|...",
                width = "100%"),

      # 表格标题
      textInput(ns("table_title"), "表格标题",
                value = "列连表分析",
                placeholder = "输入表格标题",
                width = "100%"),

      # 底注
      textInput(ns("footnote"), "表格底注",
                value = "",
                placeholder = "输入表格底注",
                width = "100%")
    )
  )
}

#' crosstable Server Functions
#'
#' @noRd
mod_crosstable_server <- function(id, data_upload_module){
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
      message("🧹 清空交叉表分析参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE
      rv$params_cleared <- TRUE

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 清空所有选择输入
      updateSelectizeInput(session, "row_var", selected = "")
      updateSelectizeInput(session, "col_var", selected = "")
      updateSelectizeInput(session, "group_var", selected = "")
      updateSelectizeInput(session, "group_cond", selected = "")

      # 清空文本输入
      updateTextInput(session, "row_label", value = "")
      updateTextInput(session, "col_label", value = "")
      updateTextInput(session, "table_title", value = "交叉表分析")
      updateTextInput(session, "footnote", value = "")

      # 重置数值输入
      updateNumericInput(session, "missing", value = 4)

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("✅ 清空参数完成，重置标志")
      })

      message("✅ 交叉表分析参数已清空")
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
        updateSelectizeInput(session, "row_var", choices = vars, selected = "")
        updateSelectizeInput(session, "col_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_var", choices = vars, selected = "")
        updateSelectizeInput(session, "group_cond", choices = character(0), selected = "")

        # 🟢 修复：只有当是交叉表分析、数据包含特定变量、且不是清空参数后的初始化时才设置默认值
        if (current_analysis_type == "crosstable" &&
            all(c("LBCLSIG_1", "LBCLSIG", "arm3") %in% vars) &&
            !rv$clearing_params) {
          message("🎯 设置交叉表分析默认变量...")
          updateSelectizeInput(session, "row_var", selected = "LBCLSIG_1")
          updateTextInput(session, "row_label", value = "治疗前")
          updateSelectizeInput(session, "col_var", selected = "LBCLSIG")
          updateTextInput(session, "col_label", value = "治疗后")
          updateSelectizeInput(session, "group_var", selected = "arm3")
          updateTextInput(session, "missing", value = "4")
          updateTextInput(session, "format", value = "1=正常|2=异常无临床意义|3=异常有临床意义|4=未查")
        }

        # 更新状态跟踪
        rv$last_data_hash <- current_data_hash
        rv$last_analysis_type <- current_analysis_type
        rv$init_done <- TRUE

        message("✅ 初始化完成，分析类型: ", current_analysis_type)
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
      req(input$row_var, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      if (input$row_label == "") {
        label_mapping <- list(
          "fxyn" = "治疗前",
          "BASELINE" = "基线",
          "PRE" = "治疗前"
        )

        if (input$row_var %in% names(label_mapping)) {
          updateTextInput(session, "row_label", value = label_mapping[[input$row_var]])
        } else {
          updateTextInput(session, "row_label", value = input$row_var)
        }
      }
    })

    observe({
      req(input$col_var, rv$init_done)

      if (rv$clearing_params) {
        return()
      }

      if (input$col_label == "") {
        label_mapping <- list(
          "flhj" = "治疗后",
          "POST" = "治疗后",
          "FOLLOWUP" = "随访"
        )

        if (input$col_var %in% names(label_mapping)) {
          updateTextInput(session, "col_label", value = label_mapping[[input$col_var]])
        } else {
          updateTextInput(session, "col_label", value = input$col_var)
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

      # 构建行列变量
      row_colvar <- paste0(
        input$row_var, "/", input$row_label, "|",
        input$col_var, "/", input$col_label
      )

      # Check if the grouping condition is empty
      if (input$group_var != "" && (is.null(input$group_cond) || length(input$group_cond) == 0 || all(input$group_cond == ""))) {
        stop("分组条件无效或为空.请选择")
      }

      list(
        data_cond = data_cond,
        group_c = group_c,
        missing = input$missing,
        row_colvar = row_colvar,
        format = input$format,
        table_title = input$table_title,
        footnote = input$footnote,

        # 🟢 新增：清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}


## To be copied in the UI
# mod_crosstable_ui("crosstable_1")

## To be copied in the server
# mod_crosstable_server("crosstable_1")
