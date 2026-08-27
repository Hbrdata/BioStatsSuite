#' Tmax UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats na.omit var
mod_Tmax_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 受试者编号
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("subject"), "受试者编号",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择受试者编号变量',
                                      maxItems = 1))
      ),

      # 制剂变量
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("formulation"), "制剂变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择制剂变量',
                                      maxItems = 1))
      ),

      # Tmax变量
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("tmax_var"), "Tmax变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = 'Tmax',
                                      maxItems = 1))
      ),

      # 受试制剂T的值
      textInput(ns("t_val"), "受试制剂T的值",
                value = "T",
                width = "100%"),

      # 参比制剂R的值
      textInput(ns("r_val"), "参比制剂R的值",
                value = "R",
                width = "100%"),

      # 受试制剂标签
      textInput(ns("t_label"), "受试制剂标签",
                value = "受试制剂",
                width = "100%"),

      # 参比制剂标签
      textInput(ns("r_label"), "参比制剂标签",
                value = "参比制剂",
                width = "100%"),

      # 表格标题
      textInput(ns("table_title"), "表格标题",
                value = "Tmax非参数检验分析结果",
                placeholder = "输入表格标题",
                width = "100%"),

      # 底注
      textInput(ns("ftnote"), "表格底注",
                value = "",
                placeholder = "输入表格底注",
                width = "100%")

    )
  )
}

#' Tmax Server Functions
#'
#' @noRd
mod_Tmax_server <- function(id, data_upload_module){
  moduleServer(id, function(input, output, session){
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
      message("清空Tmax参数")

      # 设置清空参数标志
      rv$clearing_params <- TRUE

      # 清空所有选择输入
      updateSelectizeInput(session, "subject", selected = "")
      updateSelectizeInput(session, "formulation", selected = "")
      updateSelectizeInput(session, "tmax_var", selected = "")

      # 清空文本输入
      updateTextInput(session, "t_val", value = "T")
      updateTextInput(session, "r_val", value = "R")
      updateTextInput(session, "t_label", value = "受试制剂")
      updateTextInput(session, "r_label", value = "参比制剂")
      updateTextInput(session, "table_title", value = "Tmax非参数检验分析结果")
      updateTextInput(session, "ftnote", value = "")

      # 重置初始化状态
      rv$init_done <- FALSE
      rv$last_data_hash <- NULL
      rv$last_analysis_type <- NULL

      # 延迟重置清空参数标志
      shinyjs::delay(500, {
        rv$clearing_params <- FALSE
        message("清空参数完成，重置标志")
      })

      message("Tmax参数已清空")
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
          updateSelectizeInput(session, "subject", choices = vars, selected = "")
          updateSelectizeInput(session, "formulation", choices = vars, selected = "")
          updateSelectizeInput(session, "tmax_var", choices = vars, selected = "")

          # 只有当是Tmax分析时才设置默认值（大小写不敏感检测）
          if (current_analysis_type == "Tmax") {
            message("设置Tmax默认变量...")

            find_var <- function(data_vars, candidates) {
              for (v in candidates) {
                idx <- which(tolower(data_vars) == tolower(v))
                if (length(idx) > 0) return(data_vars[idx[1]])
              }
              return(NULL)
            }

            # 检测受试者编号变量 (SUBNUM)
            detected_subject <- find_var(vars, c("SUBNUM", "subnum", "USUBJID", "usubjid"))
            if (!is.null(detected_subject)) {
              updateSelectizeInput(session, "subject", selected = detected_subject)
            }

            # 检测制剂变量 (FMTP)
            detected_formulation <- find_var(vars, c("FMTP", "fmtp", "formulation", "FORMULATION"))
            if (!is.null(detected_formulation)) {
              updateSelectizeInput(session, "formulation", selected = detected_formulation)
            }

            # 检测Tmax变量 (TMAX)
            detected_tmax <- find_var(vars, c("TMAX", "Tmax", "tmax"))
            if (!is.null(detected_tmax)) {
              updateSelectizeInput(session, "tmax_var", selected = detected_tmax)
            }
          }

          # 更新状态跟踪
          rv$last_data_hash <- current_data_hash
          rv$last_analysis_type <- current_analysis_type
          rv$init_done <- TRUE

          message("初始化完成，分析类型: ", current_analysis_type)
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

    return(reactive({
      list(
        data_cond = if (!is.null(data_upload_module()$filter_text) &&
                        data_upload_module()$filter_text != "") {
          data_upload_module()$filter_text
        } else {
          "TRUE"
        },
        subject = if (!is.null(input$subject)) input$subject else "",
        formulation = if (!is.null(input$formulation)) input$formulation else "",
        Tmax = if (!is.null(input$tmax_var)) input$tmax_var else "",
        T_val = input$t_val,
        R_val = input$r_val,
        Tlabel = input$t_label,
        Rlabel = input$r_label,
        title = input$table_title,
        footnote = input$ftnote,

        # 清空参数的方法
        clear_params = clear_parameters
      )
    }))
  })
}


## To be copied in the UI
# mod_Tmax_ui("Tmax_1")

## To be copied in the server
# mod_Tmax_server("Tmax_1")
