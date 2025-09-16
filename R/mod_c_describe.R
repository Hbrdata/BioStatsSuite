#' c_describe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_c_describe_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
      h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),

      # 分析变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("c_var_name"), "分析变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择要分析的变量',
                                      maxItems = 1))
      ),

      # 变量标签
      textInput(ns("c_var_label"), "变量标签", value = "",
                placeholder = "例如: 性别, 种族, 地区",
                width = "100%"),

      # 变量映射
      textInput(ns("c_var_mapping"), "变量映射", value = "",
                placeholder = "例如: '男'=男/'女'=女 或 1=是/0=否",
                width = "100%"),

      # 分组变量选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("c_group_var"), "分组变量",
                       choices = NULL,
                       multiple = FALSE,
                       options = list(placeholder = '选择分组变量'))
      ),

      # 分组条件选择
      tags$div(
        style = "margin-bottom: 15px;",
        selectizeInput(ns("c_group_cond"), "分组条件",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(placeholder = '选择分组条件（可多选）',
                                      maxItems = 10,
                                      plugins = list('remove_button'),
                                      create = FALSE,
                                      persist = FALSE))
      ),

      # 基准筛选条件显示
      conditionalPanel(
        condition = paste0("output['", ns("has_data"), "']"),
        tags$div(
          style = "margin-bottom: 15px; padding: 15px; border-radius: 6px; border-left: 4px solid #3498db; background-color: #e8f4f8;",
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            tags$strong("基准筛选条件:", style = "color: #2c3e50; font-size: 14px;"),
            tags$span(icon("info-circle"), style = "color: #6c757d;",
                      title = "用于计算分母的筛选条件")
          ),
          tags$div(
            style = "margin-bottom: 10px;",
            selectizeInput(ns("c_denominator_cond"), NULL,
                           choices = NULL,
                           multiple = FALSE,
                           options = list(placeholder = '选择基准筛选条件',
                                          maxItems = 1))
          ),
          tags$div(
            style = "font-size: 12px; color: #6c757d;",
            textOutput(ns("denominator_info"))
          )
        )
      ),

      # 显示选项
      fluidRow(
        column(6,
               checkboxInput(ns("c_coltotal"), "显示列合计", value = TRUE)
        ),
        column(6,
               checkboxInput(ns("c_rowtotal"), "显示行合计", value = TRUE)
        )
      ),

      # 表格标题
      textInput(ns("c_table_title"), "表格标题",
                value = "分类变量描述表",
                placeholder = "输入表格标题",
                width = "100%"),

      # 底注
      textInput(ns("c_ftnote"), "底注",
                value = "",
                placeholder = "输入表格底注",
                width = "100%")
    )
  )
}


# mod_c_describe_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tags$div(
#       style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",
#       h5("统计参数", style = "margin-top: 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px;"),
#
#       # 统计筛选条件
#       # tags$div(
#       #   style = "margin-bottom: 15px;",
#       #   selectizeInput(ns("c_data_cond"), "统计筛选条件",
#       #                  choices = NULL,
#       #                  multiple = FALSE,
#       #                  options = list(placeholder = '选择统计筛选条件',
#       #                                 maxItems = 1))
#       # ),
#       #
#       # 基准筛选条件
#       tags$div(
#         style = "margin-bottom: 15px;",
#         selectizeInput(ns("c_denominator_cond"), "基准筛选条件",
#                        choices = NULL,
#                        multiple = FALSE,
#                        options = list(placeholder = '选择基准筛选条件',
#                                       maxItems = 1))
#       ),
#
#       # 分析变量选择
#       tags$div(
#         style = "margin-bottom: 15px;",
#         selectizeInput(ns("c_var_name"), "分析变量",
#                        choices = NULL,
#                        multiple = FALSE,
#                        options = list(placeholder = '选择要分析的变量',
#                                       maxItems = 1))
#       ),
#
#       # 变量标签
#       textInput(ns("c_var_label"), "变量标签", value = "",
#                 placeholder = "例如: 性别, 种族, 地区",
#                 width = "100%"),
#
#       # 变量映射
#       textInput(ns("c_var_mapping"), "变量映射", value = "",
#                 placeholder = "例如: '男'=男/'女'=女 或 1=是/0=否",
#                 width = "100%"),
#
#       # 分组变量选择
#       tags$div(
#         style = "margin-bottom: 15px;",
#         selectizeInput(ns("c_group_var"), "分组变量",
#                        choices = NULL,
#                        multiple = FALSE,
#                        options = list(placeholder = '选择分组变量'))
#       ),
#
#       # 分组条件选择
#       tags$div(
#         style = "margin-bottom: 15px;",
#         selectizeInput(ns("c_group_cond"), "分组条件",
#                        choices = NULL,
#                        multiple = TRUE,
#                        options = list(placeholder = '选择分组条件（可多选）',
#                                       maxItems = 10,
#                                       plugins = list('remove_button'),
#                                       create = FALSE,
#                                       persist = FALSE))
#       ),
#
#       # 显示选项
#       fluidRow(
#         column(6,
#                checkboxInput(ns("c_coltotal"), "显示列合计", value = TRUE)
#         ),
#         column(6,
#                checkboxInput(ns("c_rowtotal"), "显示行合计", value = TRUE)
#         )
#       ),
#
#       # 表格标题
#       textInput(ns("c_table_title"), "表格标题",
#                 value = "分类变量描述表",
#                 placeholder = "输入表格标题",
#                 width = "100%"),
#
#       # 底注
#       textInput(ns("c_ftnote"), "底注",
#                 value = "",
#                 placeholder = "输入表格底注",
#                 width = "100%")
#     )
#   )
# }

#' c_describe Server Functions
#'
#' @noRd
mod_c_describe_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 检查是否有数据
    output$has_data <- reactive({
      !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # 获取筛选条件选项（基于数据筛选模块的条件）
    get_filter_options <- reactive({
      req(data_upload_module()$filter_text)

      # 如果有筛选条件，提供选项
      options <- c("无筛选" = "FAS!=''")

      if (data_upload_module()$filter_text != "") {
        options <- c(options, "当前筛选条件" = data_upload_module()$filter_text)
      }

      # 添加一些常用条件
      common_options <- c(
        "全人群" = "FAS!=''",
        "安全性人群" = "SAFFL=='Y'",
        "有效性人群" = "FASFL=='Y'",
        "完成试验" = "COMPLFL=='Y'"
      )

      c(options, common_options)
    })

    # 更新基准筛选条件选择列表
    observe({
      req(data_upload_module()$current_data)

      filter_options <- get_filter_options()
      updateSelectizeInput(session, "c_denominator_cond",
                           choices = filter_options,
                           selected = ifelse("当前筛选条件" %in% names(filter_options),
                                             "当前筛选条件", "FAS!=''"),
                           options = list(placeholder = '选择基准筛选条件'))
    })

    # 更新变量选择列表
    observe({
      req(data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data

      if (!is.null(current_data) && nrow(current_data) > 0 && ncol(current_data) > 0) {
        vars <- names(current_data)

        # 更新分析变量选择
        updateSelectizeInput(session, "c_var_name",
                             choices = vars,
                             server = TRUE,
                             selected = ifelse(length(vars) > 0, vars[1], ""),
                             options = list(placeholder = '选择分析变量'))

        # 更新分组变量选择
        updateSelectizeInput(session, "c_group_var",
                             choices = vars,
                             server = TRUE,
                             options = list(placeholder = '选择分组变量'))
      }
    })

    # 监听分组变量变化，更新分组条件选项
    observe({
      req(input$c_group_var, data_upload_module()$current_data)
      current_data <- data_upload_module()$current_data
      group_var <- input$c_group_var

      if (group_var %in% names(current_data)) {
        # 获取分组变量的唯一值
        unique_values <- unique(na.omit(current_data[[group_var]]))
        unique_values <- sort(unique_values)

        # 更新分组条件选择
        updateSelectizeInput(session, "c_group_cond",
                             choices = as.character(unique_values),
                             selected = input$c_group_cond,
                             options = list(placeholder = '选择分组条件',
                                            maxItems = 10))
      }
    })

    # 自动填充变量标签
    observe({
      req(input$c_var_name)
      var_name <- input$c_var_name

      # 这里可以添加一些常见的变量标签映射
      label_mapping <- list(
        "sex" = "性别",
        "gender" = "性别",
        "race" = "种族",
        "age" = "年龄",
        "trt" = "治疗组",
        "arm" = "治疗分组",
        "country" = "国家"
      )

      if (var_name %in% names(label_mapping)) {
        updateTextInput(session, "c_var_label", value = label_mapping[[var_name]])
      } else if (input$c_var_label == "") {
        updateTextInput(session, "c_var_label", value = var_name)
      }
    })

    # 自动填充变量映射
    observe({
      req(input$c_var_name, data_upload_module()$current_data)
      var_name <- input$c_var_name
      current_data <- data_upload_module()$current_data

      if (var_name %in% names(current_data)) {
        unique_values <- unique(na.omit(current_data[[var_name]]))
        if (length(unique_values) <= 10) {  # 只对分类变量自动生成映射
          mapping <- paste(sapply(unique_values, function(x) {
            paste0("'", x, "'=", x)
          }), collapse = "/")
          updateTextInput(session, "c_var_mapping", value = mapping)
        }
      }
    })

    # 显示基准筛选条件信息
    output$denominator_info <- renderText({
      if (!is.null(input$c_denominator_cond) && input$c_denominator_cond != "") {
        paste("当前选择:", input$c_denominator_cond)
      } else {
        "请选择基准筛选条件"
      }
    })

    # 返回参数列表
    reactive({
      req(input$c_var_name, input$c_group_var)

      # 构建数据条件（数据集名称 + 筛选条件）
      data_name <- data_upload_module()$data_name

      # 统计筛选条件使用数据筛选模块的当前条件
      data_cond <- if (!is.null(data_upload_module()$filter_text) &&
                       data_upload_module()$filter_text != "") {
        paste0(data_name, "|", data_upload_module()$filter_text)
      } else {
        paste0(data_name, "|FAS!=''")
      }

      # 基准筛选条件使用用户选择的条件
      denominator_cond <- if (!is.null(input$c_denominator_cond) && input$c_denominator_cond != "") {
        paste0(data_name, "|", input$c_denominator_cond)
      } else {
        paste0(data_name, "|FAS!=''")
      }

      # 构建变量列表
      varlist <- if (!is.null(input$c_var_mapping) && input$c_var_mapping != "") {
        paste(input$c_var_name, input$c_var_label, input$c_var_mapping, sep = "|")
      } else {
        paste(input$c_var_name, input$c_var_label, "", sep = "|")
      }

      # 构建分组变量
      group_c <- if (!is.null(input$c_group_cond) && length(input$c_group_cond) > 0) {
        paste(input$c_group_var, paste(input$c_group_cond, collapse = "/"), sep = "|")
      } else {
        input$c_group_var
      }

      list(
        data_cond = data_cond,
        denominator_cond = denominator_cond,
        varlist = varlist,
        group_c = group_c,
        coltotal = as.integer(input$c_coltotal),
        rowtotal = as.integer(input$c_rowtotal),
        outyn = 1,
        table_title = input$c_table_title,
        ftnote = input$c_ftnote
      )
    })
  })
}

# mod_c_describe_server <- function(id, data_upload_module) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     # 检查是否有数据
#     output$has_data <- reactive({
#       !is.null(data_upload_module()$current_data) && nrow(data_upload_module()$current_data) > 0
#     })
#     outputOptions(output, "has_data", suspendWhenHidden = FALSE)
#
#     # 获取筛选条件选项（基于数据筛选模块的条件）
#     get_filter_options <- reactive({
#       req(data_upload_module()$filter_text)
#
#       # 如果有筛选条件，提供选项
#       options <- c("无筛选" = "FAS!=''")
#
#       if (data_upload_module()$filter_text != "") {
#         options <- c(options, "当前筛选条件" = data_upload_module()$filter_text)
#       }
#
#       # 添加一些常用条件
#       common_options <- c(
#         "全人群" = "FAS!=''",
#         "安全性人群" = "SAFFL=='Y'",
#         "有效性人群" = "FASFL=='Y'",
#         "完成试验" = "COMPLFL=='Y'"
#       )
#
#       c(options, common_options)
#     })
#
#     # 更新筛选条件选择列表
#     # observe({
#     #   req(data_upload_module()$current_data)
#     #
#     #   filter_options <- get_filter_options()
#     #   updateSelectizeInput(session, "c_data_cond",
#     #                        choices = filter_options,
#     #                        selected = ifelse("当前筛选条件" %in% names(filter_options),
#     #                                          "当前筛选条件", "FAS!=''"),
#     #                        options = list(placeholder = '选择统计筛选条件'))
#     #
#     #   updateSelectizeInput(session, "c_denominator_cond",
#     #                        choices = filter_options,
#     #                        selected = "FAS!=''",
#     #                        options = list(placeholder = '选择基准筛选条件'))
#     # })
#
#     # 更新变量选择列表
#     observe({
#       req(data_upload_module()$current_data)
#       current_data <- data_upload_module()$current_data
#
#       if (!is.null(current_data) && nrow(current_data) > 0 && ncol(current_data) > 0) {
#         vars <- names(current_data)
#
#         # 更新分析变量选择
#         updateSelectizeInput(session, "c_var_name",
#                              choices = vars,
#                              server = TRUE,
#                              selected = ifelse(length(vars) > 0, vars[1], ""),
#                              options = list(placeholder = '选择分析变量'))
#
#         # 更新分组变量选择
#         updateSelectizeInput(session, "c_group_var",
#                              choices = vars,
#                              server = TRUE,
#                              options = list(placeholder = '选择分组变量'))
#       }
#     })
#
#     # 监听分组变量变化，更新分组条件选项
#     observe({
#       req(input$c_group_var, data_upload_module()$current_data)
#       current_data <- data_upload_module()$current_data
#       group_var <- input$c_group_var
#
#       if (group_var %in% names(current_data)) {
#         # 获取分组变量的唯一值
#         unique_values <- unique(na.omit(current_data[[group_var]]))
#         unique_values <- sort(unique_values)
#
#         # 更新分组条件选择
#         updateSelectizeInput(session, "c_group_cond",
#                              choices = as.character(unique_values),
#                              selected = input$c_group_cond,
#                              options = list(placeholder = '选择分组条件',
#                                             maxItems = 10))
#       }
#     })
#
#     # 自动填充变量标签
#     observe({
#       req(input$c_var_name)
#       var_name <- input$c_var_name
#
#       # 这里可以添加一些常见的变量标签映射
#       label_mapping <- list(
#         "sex" = "性别",
#         "gender" = "性别",
#         "race" = "种族",
#         "age" = "年龄",
#         "trt" = "治疗组",
#         "arm" = "治疗分组",
#         "country" = "国家"
#       )
#
#       if (var_name %in% names(label_mapping)) {
#         updateTextInput(session, "c_var_label", value = label_mapping[[var_name]])
#       } else if (input$c_var_label == "") {
#         updateTextInput(session, "c_var_label", value = var_name)
#       }
#     })
#
#     # 自动填充变量映射
#     observe({
#       req(input$c_var_name, data_upload_module()$current_data)
#       var_name <- input$c_var_name
#       current_data <- data_upload_module()$current_data
#
#       if (var_name %in% names(current_data)) {
#         unique_values <- unique(na.omit(current_data[[var_name]]))
#         if (length(unique_values) <= 10) {  # 只对分类变量自动生成映射
#           mapping <- paste(sapply(unique_values, function(x) {
#             paste0("'", x, "'=", x)
#           }), collapse = "/")
#           updateTextInput(session, "c_var_mapping", value = mapping)
#         }
#       }
#     })
#
#     # 返回参数列表
#     reactive({
#       req(input$c_var_name, input$c_group_var)
#
#       # 构建数据条件（数据集名称 + 筛选条件）
#       data_name <- data_upload_module()$data_name
#
#       data_cond <- if (!is.null(data_upload_module()$filter_text) &&
#                        data_upload_module()$filter_text != "") {
#         paste0(data_name, "|", data_upload_module()$filter_text)
#       } else {
#         paste0(data_name, "|FAS!=''")
#       }
#
#       denominator_cond <- if (!is.null(input$c_denominator_cond) && input$c_denominator_cond != "") {
#         paste0(data_name, "|", input$c_denominator_cond)
#       } else {
#         paste0(data_name, "|FAS!=''")
#       }
#
#       # 构建变量列表
#       varlist <- if (!is.null(input$c_var_mapping) && input$c_var_mapping != "") {
#         paste(input$c_var_name, input$c_var_label, input$c_var_mapping, sep = "|")
#       } else {
#         paste(input$c_var_name, input$c_var_label, "", sep = "|")
#       }
#
#       # 构建分组变量
#       group_c <- if (!is.null(input$c_group_cond) && length(input$c_group_cond) > 0) {
#         paste(input$c_group_var, paste(input$c_group_cond, collapse = "/"), sep = "|")
#       } else {
#         input$c_group_var
#       }
#
#       list(
#         data_cond = data_cond,
#         denominator_cond = denominator_cond,
#         varlist = varlist,
#         group_c = group_c,
#         coltotal = as.integer(input$c_coltotal),
#         rowtotal = as.integer(input$c_rowtotal),
#         outyn = 1,
#         table_title = input$c_table_title,
#         ftnote = input$c_ftnote
#       )
#     })
#   })
# }

## To be copied in the UI
# mod_c_describe_ui("c_describe_1")

## To be copied in the server
# mod_c_describe_server("c_describe_1")
