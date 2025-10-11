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
    # 分析模块 - 美化版
    tags$div(
      style = "border: 2px solid #e9ecef;
               padding: 20px;
               margin-bottom: 25px;
               border-radius: 10px;
               background: linear-gradient(to bottom, #ffffff, #f8f9fa);
               box-shadow: 0 2px 4px rgba(0,0,0,0.05);
               transition: all 0.3s ease;",

      # 模块标题
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #27ae60;",
        icon("chart-bar", style = "color: #27ae60; margin-right: 10px; font-size: 18px;"),
        h5("统计分析设置", style = "margin: 0; color: #2c3e50; font-weight: 600;")
      ),

      selectInput(ns("analysis_type"), "选择分析类型",
                  choices = c("请选择..." = "",
                              "描述性统计" = "q_describe",
                              "分类变量描述" = "c_describe",
                              "秩和检验" = "c_srt",
                              "协方差分析" = "covancova",
                              "组间/组内比较" = "q_param",
                              "2*2列联表" = "crosstable",
                              "生存分析" = "lifetest")),

      # 条件面板
      uiOutput(ns("analysis_params")),

      # 操作按钮区域
      tags$div(
        style = "display: flex; justify-content: space-between; margin-top: 15px;",
        actionButton(ns("run"), "运行分析",
                     icon = icon("play-circle"),
                     style = "background-color: #27ae60; color: white; border: none; font-weight: bold; padding: 8px 16px; border-radius: 4px; flex: 1; margin-right: 5px;"
        ),
        actionButton(ns("clear_params"), "清空参数",
                     icon = icon("broom"),
                     style = "background-color: #e74c3c; color: white; border: none; flex: 1; margin-left: 5px;")
      )
    )
  )
}

mod_analyze_tabPanel_ui <- function(id) {
  ns <- NS(id)

  tabPanel("分析结果",
           # 分析结果容器 - 自适应高度
           tags$div(
             style = "border: 2px solid #e9ecef;
                      padding: 20px;
                      margin: 10px;
                      border-radius: 10px;
                      background: linear-gradient(to bottom, #ffffff, #f8f9fa);
                      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
                      min-height: 400px;
                      display: flex;
                      flex-direction: column;",

             # 模块标题
             tags$div(
               style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #27ae60; flex-shrink: 0;",
               icon("chart-line", style = "color: #27ae60; margin-right: 10px; font-size: 18px;"),
               h4("分析结果", style = "margin: 0; color: #2c3e50; font-weight: 600;")
             ),

             # 结果展示区域 - 完全自适应
             tags$div(
               id = ns("result_container"),
               style = "flex: 1;
                        min-height: 200px;
                        border: 1px solid #e9ecef;
                        border-radius: 5px;
                        background-color: white;
                        padding: 15px;
                        overflow: visible;",  # 改为 visible 允许内容扩展
               uiOutput(ns("table_output"))
             ),

             # 操作按钮区域 - 跟随内容
             tags$div(
               id = ns("button_container"),
               style = "padding: 12px;
                        background-color: #f8f9fa;
                        border-radius: 5px;
                        border: 1px solid #dee2e6;
                        margin-top: 15px;
                        flex-shrink: 0;",
               fluidRow(
                 column(12,
                        tags$div(
                          style = "display: flex; gap: 10px; align-items: center; justify-content: flex-start;",
                          downloadButton(ns("download_result"), "下载结果",
                                         class = "btn-primary",
                                         style = "background-color: #3498db; border-color: #3498db;"),
                          actionButton(ns("clear_result"), "清除结果",
                                       icon = icon("trash"),
                                       style = "background-color: #e74c3c; color: white; border: none;")
                        )
                 )
               )
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

    # # 🟢 新增：定义 rv 响应式值
    rv <- reactiveValues(
      clearing_params = FALSE
    )

    # 创建响应式值来跟踪各个分析模块的服务器实例
    analysis_servers <- reactiveValues(
      q_describe = NULL,
      c_describe = NULL,
      c_srt = NULL,
      covancova = NULL,
      q_param = NULL,
      crosstable = NULL,
      lifetest = NULL
    )

    # 🟢 暴露当前分析类型
    current_analysis_type <- reactive({
      input$analysis_type
    })

    # 监听分析类型变化
    observe({
      req(current_analysis_type())

      # 调试信息
      message("Analysis type changed to: ", current_analysis_type())

    })


    # 动态渲染参数UI
    output$analysis_params <- renderUI({
      req(input$analysis_type)

      # 如果分析类型为空或为默认选项，显示提示信息
      if (input$analysis_type == "") {
        return(
          tags$div(
            style = "text-align: center; padding: 40px; color: #6c757d;",
            icon("hand-pointer", style = "font-size: 48px; margin-bottom: 20px;"),
            tags$h4("请选择分析方法"),
            tags$p("从左侧下拉菜单中选择您要使用的统计分析方法")
          )
        )
      }

      # 根据选择的分析类型渲染对应的UI
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

    # 🟢 新增：初始化分析模块（延迟执行，确保UI已渲染）
    observeEvent(input$analysis_type, {
      req(input$analysis_type)

      # 更新数据上传模块中的分析类型
      if (!is.null(data_upload_module()$updateAnalysisType)) {
        data_upload_module()$updateAnalysisType(input$analysis_type)
        message("📤 传递分析类型到数据模块: ", input$analysis_type)
      }

      # 根据当前分析类型初始化对应的服务器模块
      current_module <- switch(input$analysis_type,
                               "q_describe" = "q_describe",
                               "c_describe" = "c_describe",
                               "c_srt" = "c_srt",
                               "covancova" = "covancova",
                               "q_param" = "q_param",
                               "crosstable" = "crosstable",
                               "lifetest" = "lifetest")

      # 如果该模块尚未初始化，则初始化
      if (is.null(analysis_servers[[current_module]])) {
        message("初始化分析模块: ", current_module)

        analysis_servers[[current_module]] <- switch(input$analysis_type,
                                                     "q_describe" = mod_q_describe_server("q_describe_1", data_upload_module),
                                                     "c_describe" = mod_c_describe_server("c_describe_1", data_upload_module),
                                                     "c_srt" = mod_c_srt_server("c_srt_1", data_upload_module),
                                                     "covancova" = mod_covancova_server("covancova_1", data_upload_module),
                                                     "q_param" = mod_q_param_server("q_param_1", data_upload_module),
                                                     "crosstable" = mod_crosstable_server("crosstable_1", data_upload_module),
                                                     "lifetest" = mod_lifetest_server("lifetest_1", data_upload_module)
        )
      }
    })

    # 🟢 修复：清空参数按钮功能 - 只在用户点击时执行
    observeEvent(input$clear_params, {
      req(input$analysis_type)

      message("🧹 用户点击清空参数按钮: ", input$analysis_type)

      # 根据当前分析类型获取对应的参数函数
      current_server <- switch(input$analysis_type,
                               "q_describe" = analysis_servers$q_describe,
                               "c_describe" = analysis_servers$c_describe,
                               "c_srt" = analysis_servers$c_srt,
                               "covancova" = analysis_servers$covancova,
                               "q_param" = analysis_servers$q_param,
                               "crosstable" = analysis_servers$crosstable,
                               "lifetest" = analysis_servers$lifetest
      )

      # 如果该分析模块已初始化，则调用清空参数方法
      if (!is.null(current_server)) {
        tryCatch({
          module_result <- current_server()

          message("✅ 找到分析模块: ", input$analysis_type)

          # 检查是否有清空参数的方法
          if (!is.null(module_result$clear_params)) {
            message("✅ 找到 clear_params 方法，开始执行...")
            # 调用清空参数方法
            module_result$clear_params()
            showNotification(paste("已清空", get_analysis_name(input$analysis_type), "参数"),
                             type = "message")
          } else {
            message("❌ 未找到 clear_params 方法")
            showNotification("该分析方法暂无清空参数功能", type = "warning")
          }
        }, error = function(e) {
          message("❌ 清空参数错误: ", e$message)
          showNotification(paste("清空参数失败:", e$message), type = "error")
        })
      } else {
        message("❌ 分析模块尚未初始化: ", input$analysis_type)
        showNotification("分析模块尚未初始化，无法清空参数", type = "warning")
      }
    })

    # 🟢 辅助函数：获取分析类型的中文名称
    get_analysis_name <- function(type) {
      switch(type,
             "q_describe" = "描述性统计",
             "c_describe" = "分类变量描述",
             "c_srt" = "秩和检验",
             "covancova" = "协方差分析",
             "q_param" = "组间/组内比较",
             "crosstable" = "2*2列联表",
             "lifetest" = "生存分析")
    }

    # 🟢 获取当前分析模块的参数
    get_current_params <- reactive({
      req(input$analysis_type)

      switch(input$analysis_type,
             "q_describe" = if (!is.null(analysis_servers$q_describe)) analysis_servers$q_describe() else NULL,
             "c_describe" = if (!is.null(analysis_servers$c_describe)) analysis_servers$c_describe() else NULL,
             "c_srt" = if (!is.null(analysis_servers$c_srt)) analysis_servers$c_srt() else NULL,
             "covancova" = if (!is.null(analysis_servers$covancova)) analysis_servers$covancova() else NULL,
             "q_param" = if (!is.null(analysis_servers$q_param)) analysis_servers$q_param() else NULL,
             "crosstable" = if (!is.null(analysis_servers$crosstable)) analysis_servers$crosstable() else NULL,
             "lifetest" = if (!is.null(analysis_servers$lifetest)) analysis_servers$lifetest() else NULL
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
      # -------------
      message("=== 分析模块调试信息 ===")
      message("点击运行分析时间: ", Sys.time())

      # 检查数据上传模块的返回值
      data_info <- data_upload_module()
      message("data_upload_module()是否为NULL: ", is.null(data_info))

      if (!is.null(data_info)) {
        message("data_info中的元素: ", paste(names(data_info), collapse = ", "))
        message("current_data是否为NULL: ", is.null(data_info$current_data))
        if (!is.null(data_info$current_data)) {
          message("current_data维度: ", nrow(data_info$current_data), " x ", ncol(data_info$current_data))
          message("current_data列名: ", paste(names(data_info$current_data), collapse = ", "))
        }
        message("data_name: ", data_info$data_name)
        message("is_filtered: ", data_info$is_filtered)
      }
      # -------------




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

        # 🟢 新增：辅助函数，从参数中移除clear_params
        remove_clear_params <- function(params) {
          if (!is.null(params$clear_params)) {
            params$clear_params <- NULL
          }
          return(params)
        }

        analysis_func <- switch(input$analysis_type,
                                "q_describe" = function() {
                                  params <- q_describe_params()
                                  do.call(q_describe, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "c_describe" = function() {
                                  params <- c_describe_params()
                                  do.call(c_describe, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "c_srt" = function() {
                                  params <- c_srt_params()
                                  do.call(c_srt, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "covancova" = function() {
                                  params <- covancova_params()
                                  do.call(covancova, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "q_param" = function() {
                                  params <- q_param_params()
                                  do.call(q_param, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "crosstable" = function() {
                                  params <- crosstable_params()
                                  do.call(c_crosstable, c(list(inds = current_data), remove_clear_params(params)))
                                },
                                "lifetest" = function() {
                                  params <- lifetest_params()
                                  do.call(lifetest, c(list(inds = current_data), remove_clear_params(params)))
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

        # -----------------------------------
        message("分析错误详情: ", e$message)
        message("错误调用栈:")
        print(traceback())
        # -----------------------------------

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

    # 🟢 新增：清除结果的观察器
    observeEvent(input$clear_result, {
      # 清除结果
      result(NULL)

      # 清除表格输出
      output$table_output <- renderUI({
        tags$div(
          style = "text-align: center; padding: 40px; color: #6c757d;",
          icon("chart-bar", style = "font-size: 48px; margin-bottom: 20px;"),
          tags$h4("暂无分析结果"),
          tags$p("请在上传数据并设定好统计参数后，点击\"运行分析\"按钮生成分析结果")
        )
      })

      showNotification("分析结果已清除", type = "message")
    })

    # 🟢 修改：在运行分析时，如果结果区域为空，显示提示信息
    output$table_output <- renderUI({
      if (is.null(result())) {
        tags$div(
          style = "text-align: center; padding: 40px; color: #6c757d;",
          icon("chart-bar", style = "font-size: 48px; margin-bottom: 20px;"),
          tags$h4("暂无分析结果"),
          tags$p("请点击\"运行分析\"按钮生成分析结果")
        )
      } else {
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
      }
    })

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

    return(reactive({
      list(
        result = result(),
        current_analysis_type = current_analysis_type()
      )
    }))


  })
}

## To be copied in the UI
# mod_analyze_sidebar_ui("analyze_1")
# mod_analyze_tabPanel_ui("analyze_1")

## To be copied in the server
# mod_analyze_server("analyze_1")
