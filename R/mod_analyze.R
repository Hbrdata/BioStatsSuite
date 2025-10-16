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
                          # 替换清除按钮为管理结果按钮
                          actionButton(ns("manage_results"), "管理结果",
                                       icon = icon("list"),
                                       style = "background-color: #f39c12; color: white; border: none;")
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
    # 🟢 修改：将result改为列表，支持增量存储
    results_list <- reactiveVal(list())

    # 🟢 新增：存储当前显示的结果索引
    current_display_indices <- reactiveVal(integer(0))

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

    # 🟢 新增：生成结果ID的函数
    generate_result_id <- function() {
      paste0("result_", as.integer(Sys.time()), "_", sample(1000, 1))
    }

    # 🟢 新增：生成结果标题的函数
    generate_result_title <- function(analysis_type, data_name, timestamp) {
      analysis_name <- get_analysis_name(analysis_type)
      time_str <- format(timestamp, "%m-%d %H:%M")
      paste0(analysis_name, " - ", data_name, " (", time_str, ")")
    }

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
          new_result <- analysis_func()

          # 🟢 修改：将新结果添加到结果列表中
          if (!is.null(new_result)) {
            result_id <- generate_result_id()
            timestamp <- Sys.time()
            result_title <- generate_result_title(input$analysis_type, data_name, timestamp)

            # 创建结果对象
            result_obj <- list(
              id = result_id,
              title = result_title,
              analysis_type = input$analysis_type,
              data_name = data_name,
              timestamp = timestamp,
              result = new_result,
              displayed = TRUE  # 默认显示新结果
            )

            # 获取当前结果列表
            current_results <- results_list()

            # 添加新结果
            current_results[[result_id]] <- result_obj

            # 更新结果列表
            results_list(current_results)

            # 🟢 更新当前显示索引：默认显示所有结果
            current_display_indices(seq_along(current_results))

            showNotification("分析完成！结果已添加到分析列表", type = "message")
          }
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

    # 🟢 新增：管理结果对话框
    observeEvent(input$manage_results, {
      req(results_list())

      current_results <- results_list()

      if (length(current_results) == 0) {
        showNotification("暂无分析结果可管理", type = "warning")
        return()
      }

      # 创建对话框内容
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$span("管理分析结果"),
          tags$div(
            style = "display: flex; gap: 10px;",
            actionButton(ns("select_all_results"), "全选",
                         style = "background-color: #3498db; color: white; border: none; padding: 4px 8px; font-size: 12px;"),
            actionButton(ns("deselect_all_results"), "全不选",
                         style = "background-color: #95a5a6; color: white; border: none; padding: 4px 8px; font-size: 12px;")
          )
        ),
        size = "l",
        footer = tagList(
          actionButton(ns("delete_selected_results"), "删除选中结果",
                       icon = icon("trash"),
                       style = "background-color: #e74c3c; color: white; border: none;"),
          modalButton("关闭")
        ),

        # 结果列表
        tags$div(
          style = "max-height: 400px; overflow-y: auto;",
          lapply(seq_along(current_results), function(i) {
            result <- current_results[[i]]
            result_id <- names(current_results)[i]

            tags$div(
              style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
              tags$div(
                style = "display: flex; align-items: center;",
                checkboxInput(ns(paste0("result_check_", result_id)),
                              label = NULL,
                              value = TRUE,
                              width = "20px"),
                tags$div(
                  style = "margin-left: 10px; flex: 1;",
                  tags$strong(result$title),
                  tags$br(),
                  tags$small(paste("分析类型:", get_analysis_name(result$analysis_type)),
                             style = "color: #666;"),
                  tags$br(),
                  tags$small(paste("时间:", format(result$timestamp, "%Y-%m-%d %H:%M:%S")),
                             style = "color: #999;")
                )
              )
            )
          })
        )
      ))
    })

    # 🟢 新增：全选结果
    observeEvent(input$select_all_results, {
      current_results <- results_list()
      if (length(current_results) > 0) {
        # 更新所有复选框为选中状态
        for (i in seq_along(current_results)) {
          result_id <- names(current_results)[i]
          updateCheckboxInput(session, paste0("result_check_", result_id), value = TRUE)
        }
      }
    })

    # 🟢 新增：全不选结果
    observeEvent(input$deselect_all_results, {
      current_results <- results_list()
      if (length(current_results) > 0) {
        # 更新所有复选框为未选中状态
        for (i in seq_along(current_results)) {
          result_id <- names(current_results)[i]
          updateCheckboxInput(session, paste0("result_check_", result_id), value = FALSE)
        }
      }
    })

    # 🟢 新增：删除选中结果
    observeEvent(input$delete_selected_results, {
      current_results <- results_list()
      if (length(current_results) == 0) return()

      # 收集要删除的结果ID
      results_to_delete <- c()

      for (i in seq_along(current_results)) {
        result_id <- names(current_results)[i]
        checkbox_id <- paste0("result_check_", result_id)

        # 检查复选框状态
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]] == TRUE) {
          results_to_delete <- c(results_to_delete, result_id)
        }
      }

      if (length(results_to_delete) == 0) {
        showNotification("请先选择要删除的结果", type = "warning")
        return()
      }

      # 从结果列表中移除选中的结果
      updated_results <- current_results
      for (result_id in results_to_delete) {
        updated_results[[result_id]] <- NULL
      }

      # 更新结果列表
      results_list(updated_results)

      # 移除模态框
      removeModal()

      showNotification(paste("已删除", length(results_to_delete), "个分析结果"), type = "message")
    })

    # 🟢 修改：渲染分析结果 - 显示所有选中的结果
    output$table_output <- renderUI({
      current_results <- results_list()

      if (length(current_results) == 0) {
        return(
          tags$div(
            style = "text-align: center; padding: 40px; color: #6c757d;",
            icon("chart-bar", style = "font-size: 48px; margin-bottom: 20px;"),
            tags$h4("暂无分析结果"),
            tags$p("请点击\"运行分析\"按钮生成分析结果")
          )
        )
      }

      # 收集所有要显示的结果
      display_contents <- list()

      for (i in seq_along(current_results)) {
        result_id <- names(current_results)[i]
        result <- current_results[[result_id]]
        checkbox_id <- paste0("result_check_", result_id)

        # 检查复选框状态，如果不存在则默认为TRUE（显示）
        should_display <- if (!is.null(input[[checkbox_id]])) {
          input[[checkbox_id]]
        } else {
          TRUE
        }

        if (should_display) {
          # 添加结果标题
          display_contents <- c(display_contents, list(
            tags$div(
              style = "border-bottom: 2px solid #3498db; padding: 10px 0; margin-bottom: 15px;",
              tags$h5(result$title, style = "color: #2c3e50; margin: 0;")
            )
          ))

          # 添加结果内容
          ft <- result$result
          if (!is.null(ft)) {
            if (is.list(ft) && length(ft) == 2) {
              display_contents <- c(display_contents, list(
                htmltools::HTML(
                  paste(
                    as.character(flextable::htmltools_value(ft[[1]])),
                    as.character(flextable::htmltools_value(ft[[2]])),
                    sep = "<br><br>"
                  )
                )
              ))
            } else {
              display_contents <- c(display_contents, list(
                htmltools::HTML(as.character(flextable::htmltools_value(ft)))
              ))
            }
          }

          # 添加分隔线（除了最后一个结果）
          if (i < length(current_results)) {
            display_contents <- c(display_contents, list(
              tags$hr(style = "border-top: 2px dashed #ddd; margin: 20px 0;")
            ))
          }
        }
      }

      if (length(display_contents) == 0) {
        return(
          tags$div(
            style = "text-align: center; padding: 40px; color: #6c757d;",
            icon("eye-slash", style = "font-size: 48px; margin-bottom: 20px;"),
            tags$h4("没有选中的结果可显示"),
            tags$p("请在\"管理结果\"中选择要显示的分析结果")
          )
        )
      }

      # 返回所有显示内容
      tagList(display_contents)
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

    # 🟢 修改：下载处理函数 - 下载所有选中的结果
    output$download_result <- downloadHandler(
      filename = function() {
        paste0("analysis_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        current_results <- results_list()
        if (length(current_results) == 0) {
          showNotification("没有可下载的分析结果", type = "warning")
          return()
        }

        tryCatch({
          # 创建新的Word文档
          doc <- officer::read_docx()

          # 收集所有要下载的结果
          results_to_download <- list()

          for (i in seq_along(current_results)) {
            result_id <- names(current_results)[i]
            result <- current_results[[result_id]]
            checkbox_id <- paste0("result_check_", result_id)

            # 检查复选框状态，如果不存在则默认为TRUE（下载）
            should_download <- if (!is.null(input[[checkbox_id]])) {
              input[[checkbox_id]]
            } else {
              TRUE
            }

            if (should_download) {
              results_to_download <- c(results_to_download, list(result))
            }
          }

          if (length(results_to_download) == 0) {
            showNotification("请先在管理结果中选择要下载的分析结果", type = "warning")
            return()
          }

          # 添加每个结果到文档
          for (i in seq_along(results_to_download)) {
            result <- results_to_download[[i]]

            # 添加结果标题
            doc <- officer::body_add_par(doc, result$title, style = "heading 1")
            doc <- officer::body_add_par(doc, paste("分析类型:", get_analysis_name(result$analysis_type)), style = "Normal")
            doc <- officer::body_add_par(doc, paste("数据名称:", result$data_name), style = "Normal")
            doc <- officer::body_add_par(doc, paste("分析时间:", format(result$timestamp, "%Y-%m-%d %H:%M:%S")), style = "Normal")
            doc <- officer::body_add_par(doc, "", style = "Normal")  # 空行

            # 添加结果表格
            ft <- result$result
            if (!is.null(ft)) {
              if (is.list(ft) && all(sapply(ft, function(x) inherits(x, "flextable")))) {
                # 多个表格的情况
                for (table_idx in seq_along(ft)) {
                  doc <- flextable::body_add_flextable(doc, value = ft[[table_idx]])
                  if (table_idx < length(ft)) {
                    doc <- officer::body_add_break(doc)  # 表格间添加分页
                  }
                }
              } else if (inherits(ft, "flextable")) {
                # 单个表格的情况
                doc <- flextable::body_add_flextable(doc, value = ft)
              }
            }

            # 在结果之间添加分页（除了最后一个结果）
            if (i < length(results_to_download)) {
              doc <- officer::body_add_break(doc)
            }
          }

          # 保存文档
          print(doc, target = file)
          showNotification(paste("成功下载", length(results_to_download), "个分析结果"), type = "message")

        }, error = function(e) {
          showNotification(paste("下载错误:", e$message), type = "error")
        })
      }
    )

    return(reactive({
      list(
        results_list = results_list(),
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
