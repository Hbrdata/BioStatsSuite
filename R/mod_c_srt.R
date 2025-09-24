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
                    placeholder = "例如: 用药后18周±3天-基线",
                    width = "100%"),

          # 变量映射
          textInput(ns("var_mapping"), "变量映射", value = "",
                    placeholder = "例如: -3=改善3个等级/-2=改善2个等级/-1=改善1个等级/0=无变化/1=加重1个等级/2=加重2个等级/3=加重3个等级",
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
                           options = list(placeholder = '选择分组条件（可多选）',
                                          maxItems = 10,
                                          plugins = list('remove_button'),  # 添加移除按钮
                                          create = FALSE,  # 禁止用户输入新选项
                                          persist = FALSE  # 移除后不保持选项
                           )
            ),
            tags$small(icon("info-circle"), "选择分组变量后，此处会自动显示可选项",
                       style = "color: #6c757d; font-size: 12px;")
          ),
          checkboxInput(ns("coltotal"), "显示列合计", value = FALSE),
          checkboxInput(ns("rowtotal"), "显示行合计", value = TRUE),
          # checkboxInput(ns("outyn"), "输出表格", value = TRUE),
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

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 更新变量选择列表
    observe({
      req(data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data

      if (!is.null(current_data) && nrow(current_data) > 0 && ncol(current_data) > 0) {
        vars <- names(current_data)

        # 更新分析变量选择
        updateSelectizeInput(session, "var_name",
                             choices = vars,
                             server = TRUE,
                             selected = ifelse(length(vars) > 0, vars[1], ""),
                             options = list(placeholder = '选择分析变量'))


        # 更新分组变量选择
        updateSelectizeInput(session, "group_name",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分组变量'))
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_name, data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data
      group_var <- input$group_name

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(session, "group_cond",
                             choices = as.character(unique_values),
                             selected = input$group_cond,  # 保持之前的选择
                             options = list(placeholder = '选择分组条件',
                                            maxItems = 10))
      }
    })

    # 自动填充变量标签
    observe({
      req(input$var_name)
      var_name <- input$var_name

      # 这里可以添加一些常见的变量标签映射
      label_mapping <- list(
        "DIFF_TB" = "用药后18周±3天-基线",
        "CHANGE" = "变化值",
        "SCORE" = "评分",
        "GRADE" = "分级"
      )

      if (var_name %in% names(label_mapping)) {
        updateTextInput(session, "var_label", value = label_mapping[[var_name]])
      } else if (input$var_label == "") {
        updateTextInput(session, "var_label", value = var_name)
      }
    })

    # 自动填充变量映射
    observe({
      req(input$var_name, data_upload_module()$current_data)
      var_name <- input$var_name
      current_data <- data_upload_module()$current_data

      if (var_name %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[var_name]]))
        if (length(unique_values) <= 10) {  # 只对分类变量自动生成映射
          mapping <- paste(sapply(unique_values, function(x) {
            paste0(x, "=", x)
          }), collapse = "/")
          updateTextInput(session, "var_mapping", value = mapping)
        }
      }
    })

    return(
            reactive({

              req(input$var_name, input$group_name)

              group_cond_text <- if (!is.null(input$group_cond) && length(input$group_cond) > 0) {
                # 格式: 变量|组别1/组别2/组别3
                paste0(input$group_name, "|", paste(input$group_cond, collapse = "/"))
              } else if (!is.null(input$group_name)) {
                # 如果没有选择具体条件，使用所有唯一值
                current_data <- data_upload_module()$current_data
                if (!is.null(current_data) && input$group_name %in% names(current_data)) {
                  unique_values <- unique(na.omit(current_data[[input$group_name]]))
                  paste0(input$group_name, "|", paste(sort(unique_values), collapse = "/"))
                } else {
                  input$group_name
                }
              } else {
                ""  # 默认值
              }


              # 构建变量列表
              varlist <- if (!is.null(input$var_mapping) && input$var_mapping != "") {
                paste(input$var_name, input$var_label, input$var_mapping, sep = "|")
              } else {
                paste(input$var_name, input$var_label, "", sep = "|")
              }

              #
            list(
              data_cond <- if (!is.null(data_upload_module()$filter_text) &&
                               data_upload_module()$filter_text != "") {
                data_upload_module()$filter_text
              } else {
                "TRUE"  # 默认选择所有行
              },
              varlist = varlist,
              group_c = group_cond_text,
              coltotal = as.integer(input$coltotal),
              rowtotal = as.integer(input$rowtotal),
              outyn = 1,
              test_between = as.integer(input$test_between),
              test_in = as.integer(input$test_in),
              table_title = input$table_title,
              ftnote = input$ftnote
            )
          })
    )
  })
}

## To be copied in the UI
# mod_c_ui("c_1")

## To be copied in the server
# mod_c_server("c_1")
