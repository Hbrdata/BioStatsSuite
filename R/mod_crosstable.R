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
                placeholder = "例如: 治疗前",
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
                placeholder = "例如: 治疗后",
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
                       options = list(placeholder = '选择分组条件（可多选）',
                                      maxItems = 10,
                                      plugins = list('remove_button'),
                                      create = FALSE,
                                      persist = FALSE))
      ),

      # 缺失值填补
      numericInput(ns("missing"), "缺失值填补", value = 4),

      # 格式定义
      textInput(ns("format"), "格式定义",
                value = "1=正常|2=异常无临床意义|3=异常有临床意义|4=未查",
                placeholder = "值=标签|值=标签",
                width = "100%"),

      # 表格标题
      textInput(ns("table_title"), "表格标题",
                value = "交叉表分析",
                placeholder = "输入表格标题",
                width = "100%"),

      # 底注
      textInput(ns("footnote"), "底注",
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

        # 更新所有变量选择
        updateSelectizeInput(session, "row_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择行变量'))

        updateSelectizeInput(session, "col_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择列变量'))

        updateSelectizeInput(session, "group_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分组变量'))
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$group_var, data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data
      group_var <- input$group_var

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(session, "group_cond",
                             choices = as.character(unique_values),
                             selected = input$group_cond,
                             options = list(placeholder = '选择分组条件',
                                            maxItems = 10))
      }
    })

    # 自动填充变量标签
    observe({
      # 行变量标签
      req(input$row_var)
      if (input$row_label == "") {
        updateTextInput(session, "row_label", value = input$row_var)
      }

      # 列变量标签
      req(input$col_var)
      if (input$col_label == "") {
        updateTextInput(session, "col_label", value = input$col_var)
      }
    })

    return(reactive({
      req(input$row_var, input$col_var, input$group_var)

      # 构建数据条件
      data_cond <- if (!is.null(data_upload_module()$filter_text) &&
                       data_upload_module()$filter_text != "") {
        paste0(data_upload_module()$data_name, "|", data_upload_module()$filter_text)
      } else {
        paste0(data_upload_module()$data_name, "|RANDYN=='是' & SS=='是' & visitnum=='2' & lbtest=='白细胞数'")
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

      list(
        data_cond = data_cond,
        group_c = group_c,
        missing = input$missing,
        row_colvar = row_colvar,
        format = input$format,
        table_title = input$table_title,
        footnote = input$footnote
      )
    }))
  })
}


## To be copied in the UI
# mod_crosstable_ui("crosstable_1")

## To be copied in the server
# mod_crosstable_server("crosstable_1")
