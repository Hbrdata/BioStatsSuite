#' analyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("analysis_type"), "选择分析类型",
                choices = c("描述性统计" = "q_describe",
                            "分类变量描述" = "c_describe",
                            "秩和检验" = "c_srt",
                            "协方差分析" = "covancova",
                            "差异检验" = "q_param",
                            "实验室交叉表分析" = "crosstable",
                            "生存分析" = "lifetest")),

    # 条件面板
    uiOutput(ns("analysis_params")),
    actionButton(ns("run"), "运行分析")
  )
}

mod_analyze_tabPanel_ui <- function(id) {
  ns <- NS(id)

  tabPanel("分析结果",
           h4("分析结果"),
           # 使用绝对定位填充整个标签页
           div(style = "width: 100%; height: 100%; overflow: auto;",
               uiOutput(ns("table_output"))
           ),
           fluidRow(
             column(12,
                    downloadButton(ns("download_result"), "下载结果",
                                   class = "btn-primary",
                                   style = "margin-bottom: 10px;")
             )
           )
  )
}


#' analyze Server Functions
#'
#' @noRd
mod_analyze_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- reactiveVal(NULL)

    # 动态渲染参数UI
    output$analysis_params <- renderUI({
      req(input$analysis_type)
      switch(input$analysis_type,
             "q_describe" = mod_q_describe_ui(ns("q_describe_1")),
             "c_describe" = mod_c_describe_ui(ns("c_describe_1")),
             "c_srt" = mod_c_srt_ui(ns("c_srt_1")),
             "covancova" = mod_covancova_ui(ns("covancova_1")),
             "q_param" = mod_q_param_ui(ns("q_param_1")),
             "crosstable" = mod_crosstable_ui(ns("crosstable_1")),
             "lifetest" = mod_lifetest_ui(ns("lifetest_1"))
      )
    })

    # 初始化分析模块
    q_describe_params <- mod_q_describe_server("q_describe_1", data_upload_module)
    c_describe_params <- mod_c_describe_server("c_describe_1", data_upload_module)
    c_srt_params <- mod_c_srt_server("c_srt_1", data_upload_module)
    covancova_params <- mod_covancova_server("covancova_1", data_upload_module)
    q_param_params <- mod_q_param_server("q_param_1", data_upload_module)
    crosstable_params <- mod_crosstable_server("crosstable_1", data_upload_module)
    lifetest_params <- mod_lifetest_server("lifetest_1", data_upload_module)

    observeEvent(input$run, {
      req(data_upload_module()$current_data)
      req(data_upload_module()$data_name)
      req(input$analysis_type)

      tryCatch({
        data_name <- data_upload_module()$data_name
        current_data <- data_upload_module()$current_data


        print(paste("正在执行分析:", input$analysis_type))
        print(paste("数据名称:", data_name))
        print(paste("数据维度:", dim(data_upload_module()$current_data), collapse = "x"))
        print(paste("数据状态:", ifelse(data_upload_module()$is_filtered, "已筛选", "原始")))

        analysis_func <- switch(input$analysis_type,
                                "q_describe" = function() {
                                  params <- q_describe_params()
                                  do.call(q_describe, c(list(data_name = data_name), params))
                                },
                                "c_describe" = function() {
                                  params <- c_describe_params()
                                  do.call(c_describe, params)
                                },
                                "c_srt" = function() {
                                  params <- c_srt_params()
                                  do.call(c_srt, params)
                                },
                                "covancova" = function() {
                                  params <- covancova_params()
                                  do.call(covancova, params)
                                },
                                "q_param" = function() {
                                  params <- q_param_params()
                                  do.call(q_param, params)
                                },
                                "crosstable" = function() {
                                  params <- crosstable_params()
                                  do.call(c_crosstable, params)
                                },
                                "lifetest" = function() {
                                  params <- lifetest_params()
                                  do.call(lifetest, params)
                                }
        )

        if (!is.null(analysis_func)) {
          result(analysis_func())

          # 渲染分析结果
          output$table_output <- renderUI({
            ft <- result()
            if (!is.null(ft)) {
              if (is.list(ft) && length(ft) == 2) {
                htmltools::HTML(
                  paste(
                    as.character(flextable::htmltools_value(ft[[1]])),
                    as.character(flextable::htmltools_value(ft[[2]])),
                    sep = "<br><br>"
                  )
                )
              } else {
                htmltools::HTML(as.character(flextable::htmltools_value(ft)))
              }
            }
          })

          showNotification("分析完成！", type = "message")
        }

      }, error = function(e) {
        showNotification(paste("分析错误:", e$message), type = "error")
      })
    })


    # 辅助函数：估算表格总宽度
    estimate_table_width <- function(ft) {
      if (inherits(ft, "flextable")) {
        # 估算每列的宽度（假设平均字符宽度）
        total_width <- 0
        for (col_key in ft$col_keys) {
          # 获取列名长度
          col_name_width <- nchar(col_key) * 0.15  # 每个字符约0.15英寸
          # 估算数据内容的最大宽度
          data_width <- if (!is.null(ft$body$dataset)) {
            max(nchar(as.character(ft$body$dataset[[col_key]])), na.rm = TRUE) * 0.12
          } else {
            1.0  # 默认宽度
          }
          # 取较大的值，加上一些边距
          col_width <- max(col_name_width, data_width, 0.8) + 0.2
          total_width <- total_width + col_width
        }
        return(total_width)
      }
      return(0)
    }

    # 辅助函数：检查表格是否需要横向页面
    needs_landscape <- function(ft) {
      if (inherits(ft, "flextable")) {
        # 估算表格总宽度
        table_width <- estimate_table_width(ft)
        print(paste("表格估算宽度:", round(table_width, 2), "英寸"))

        # 纵向页面可用宽度约为6.5英寸（考虑页边距）
        # 如果表格宽度超过5.5英寸，使用横向页面
        return(table_width > 5.5)
      }
      return(FALSE)
    }

    # 辅助函数：获取最宽的表格方向需求
    get_orientation_for_tables <- function(ft_list) {
      if (is.list(ft_list)) {
        # 检查所有表格，如果有任何一个需要横向，就使用横向
        any_landscape <- any(sapply(ft_list, needs_landscape))
        return(ifelse(any_landscape, "landscape", "portrait"))
      } else if (inherits(ft_list, "flextable")) {
        return(ifelse(needs_landscape(ft_list), "landscape", "portrait"))
      }
      return("portrait")
    }

    # 辅助函数：自动调整表格宽度以适应页面
    adjust_table_width <- function(ft, orientation) {
      if (inherits(ft, "flextable")) {
        # 根据页面方向设置最大宽度
        max_width <- if (orientation == "landscape") 9.0 else 6.0

        # 估算当前表格宽度
        current_width <- estimate_table_width(ft)

        if (current_width > max_width) {
          # 需要缩放表格
          scale_factor <- max_width / current_width
          print(paste("表格缩放比例:", round(scale_factor, 2)))

          # 应用缩放
          ft <- flextable::width(ft, width = ft$col_keys %>%
                                   lapply(function(x) scale_factor * 1.0) %>%
                                   unlist())
        }

        # 设置自动换行
        ft <- flextable::set_table_properties(ft, layout = "autofit")
      }
      return(ft)
    }

    # 下载处理函数
    output$download_result <- downloadHandler(
      filename = function() {
        paste0("analysis_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        req(result())
        ft <- result()

        tryCatch({
          # 如果结果是列表，合并所有表格
          if (is.list(ft) && all(sapply(ft, function(x) inherits(x, "flextable")))) {
            # 创建临时文件
            temp_docx <- tempfile(fileext = ".docx")

            # 创建新的Word文档
            doc <- officer::read_docx()

            # 添加每个表格
            for (i in seq_along(ft)) {
              if (i > 1) {
                # 在表格之间添加分页符
                doc <- officer::body_add_break(doc)
              }
              doc <- flextable::body_add_flextable(doc, value = ft[[i]])
            }

            # 保存文档
            print(doc, target = temp_docx)
            file.copy(temp_docx, file)
            unlink(temp_docx)
          } else if (inherits(ft, "flextable")) {
            # 单个表格的情况
            doc <- officer::read_docx()
            doc <- flextable::body_add_flextable(doc, value = ft)
            print(doc, target = file)
          } else {
            showNotification("无法下载：结果格式不支持", type = "error")
          }
        }, error = function(e) {
          showNotification(paste("下载错误:", e$message), type = "error")
        })
      }
    )



  })
}

## To be copied in the UI
# mod_analyze_sidebar_ui("analyze_1")
# mod_analyze_tabPanel_ui("analyze_1")

## To be copied in the server
# mod_analyze_server("analyze_1")
