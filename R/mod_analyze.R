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
             ),

             tags$br(),

             tags$div(
               style = "background-color: #e8f4f8;
               padding: 12px;
               border-radius: 6px;
               margin-bottom: 15px;
               border-left: 4px solid #3498db;
               border: 1px solid #b8e0f0;",
               tags$div(
                 style = "display: flex; align-items: flex-start;",
                 icon("info-circle", style = "color: #3498db; margin-right: 8px; margin-top: 2px; flex-shrink: 0;"),
                 tags$div(
                   style = "flex: 1;",
                   tags$small("有多个分析结果时，可自行选择需要下载和清除的结果",
                              style = "color: #2c3e50; line-height: 1.4;")
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
    # 🟢 修改：将单个结果改为结果列表
    results_list <- reactiveVal(list())
    # 🟢 新增：选中的结果索引
    selected_results <- reactiveVal(integer(0))

    # 🟢 定义 rv 响应式值
    rv <- reactiveValues(
      clearing_params = FALSE,
      # 🟢 新增：跟踪结果ID计数器
      next_result_id = 1,
      # 🟢 新增：用于防止重复触发的标志
      toggle_processing = FALSE
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
      message("Analysis type changed to: ", current_analysis_type())
    })

    # 动态渲染参数UI
    output$analysis_params <- renderUI({
      req(input$analysis_type)

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

          # 🟢 修复：将新结果添加到结果列表中
          current_results <- results_list()
          result_id <- rv$next_result_id
          rv$next_result_id <- rv$next_result_id + 1

          result_name <- paste0(get_analysis_name(input$analysis_type), " #", result_id)

          # 🟢 修复：使用命名列表存储结果
          current_results[[as.character(result_id)]] <- list(
            id = result_id,
            name = result_name,
            analysis_type = input$analysis_type,
            timestamp = Sys.time(),
            flextable = new_result
          )

          results_list(current_results)

          # 🟢 修复：自动选中新添加的结果
          current_selected <- selected_results()
          selected_results(c(current_selected, result_id))

          showNotification("分析完成！结果已添加到列表中", type = "message")
        }

      }, error = function(e) {
        message("分析错误详情: ", e$message)
        message("错误调用栈:")
        print(traceback())
        showNotification(paste("分析错误:", e$message), type = "error")
      })
    })

    # 🟢 修复：全选/取消全选功能
    observeEvent(input$select_all_results, {
      current_results <- results_list()
      if (length(current_results) > 0) {
        current_selected <- selected_results()
        all_result_ids <- as.integer(names(current_results))

        if (length(current_selected) == length(all_result_ids)) {
          # 如果已经全选，则取消全选
          selected_results(integer(0))
        } else {
          # 否则全选所有结果
          selected_results(all_result_ids)
        }
      }
    })

    # 🟢 修复：清除选中的结果
    observeEvent(input$clear_result, {
      current_selected <- selected_results()
      if (length(current_selected) == 0) {
        showNotification("请先选择要清除的结果", type = "warning")
        return()
      }

      current_results <- results_list()
      if (length(current_results) > 0) {
        # 🟢 修复：正确移除选中的结果
        remaining_results <- current_results[!as.integer(names(current_results)) %in% current_selected]
        results_list(remaining_results)
        selected_results(integer(0))  # 清空选择

        showNotification(paste("已清除", length(current_selected), "个结果"), type = "message")
      }
    })

    # 🟢 修复：使用动态观察器处理单个选择
    observe({
      current_results <- results_list()

      # 为每个结果创建观察器
      lapply(current_results, function(result) {
        toggle_id <- paste0("toggle_", result$id)

        # 使用局部变量捕获当前结果ID
        local_result_id <- result$id

        observeEvent(input[[toggle_id]], {
          # 防止重复处理
          if (rv$toggle_processing) return()
          rv$toggle_processing <- TRUE

          # 延迟释放锁
          on.exit({
            invalidateLater(50)
            observe({ rv$toggle_processing <- FALSE })
          })

          current_selected <- selected_results()

          if (local_result_id %in% current_selected) {
            # 取消选择
            selected_results(current_selected[current_selected != local_result_id])
            message("🔘 取消选择结果: ", local_result_id)
          } else {
            # 选择
            selected_results(c(current_selected, local_result_id))
            message("🔘 选择结果: ", local_result_id)
          }
        }, ignoreInit = TRUE)
      })
    })

    # 🟢 修复：渲染结果列表和选择控件
    output$table_output <- renderUI({
      current_results <- results_list()
      current_selected <- selected_results()

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

      # 🟢 修复：选择控制面板
      all_result_ids <- as.integer(names(current_results))
      is_all_selected <- length(current_selected) == length(all_result_ids)

      selection_panel <- tags$div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #dee2e6;",
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
          tags$div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$strong("结果管理: "),
            actionButton(ns("select_all_results"),
                         label = if (is_all_selected) "取消全选" else "全选",
                         style = "padding: 4px 8px; font-size: 12px;"),
            tags$span(paste("已选择", length(current_selected), "/", length(current_results), "个结果"),
                      style = "color: #6c757d; font-size: 14px;")
          )
        )
      )

      # 🟢 修复：按ID排序渲染所有结果
      sorted_results <- current_results[order(as.integer(names(current_results)))]

      result_elements <- lapply(sorted_results, function(result) {
        is_selected <- result$id %in% current_selected

        # 🟢 修复：使用正确的ID生成方式
        toggle_id <- paste0("toggle_", result$id)

        tags$div(
          id = ns(paste0("result_", result$id)),
          style = paste("border: 2px solid", if (is_selected) "#3498db" else "#e9ecef",
                        "; padding: 15px; margin-bottom: 15px; border-radius: 8px;
                       background-color:", if (is_selected) "#f0f8ff" else "white", ";"),

          # 结果标题和选择按钮
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; padding-bottom: 8px; border-bottom: 1px solid #dee2e6;",
            tags$div(
              tags$strong(paste("分析类型:",result$name), style = "color: #2c3e50;"),
              tags$br(),
              # tags$small(paste("分析类型:", get_analysis_name(result$analysis_type)),
              #            style = "color: #6c757d;"),
              # tags$br(),
              tags$small(paste("生成时间:", format(result$timestamp, "%Y-%m-%d %H:%M:%S")),
                         style = "color: #6c757d;")
            ),
            tags$div(
              actionButton(ns(toggle_id),
                           label = if (is_selected) "取消选择" else "选择",
                           style = paste("padding: 4px 8px; font-size: 12px;",
                                         if (is_selected) "background-color: #e74c3c; color: white;"
                                         else "background-color: #3498db; color: white;"))
            )
          ),

          # 结果内容
          if (is_selected) {
            if (is.list(result$flextable) && length(result$flextable) == 2) {
              htmltools::HTML(
                paste(
                  as.character(flextable::htmltools_value(result$flextable[[1]])),
                  as.character(flextable::htmltools_value(result$flextable[[2]])),
                  sep = "<br><br>"
                )
              )
            } else {
              htmltools::HTML(as.character(flextable::htmltools_value(result$flextable)))
            }
          } else {
            tags$div(
              style = "text-align: center; padding: 20px; color: #6c757d;",
              icon("eye-slash"), "结果未选中 - 点击\"选择\"按钮显示内容"
            )
          }
        )
      })

      # 组合所有元素
      tagList(
        selection_panel,
        result_elements
      )
    })

    # 🟢 修复：下载处理函数 - 只下载选中的结果
    output$download_result <- downloadHandler(
      filename = function() {
        paste0("analysis_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      },
      content = function(file) {
        current_results <- results_list()
        current_selected <- selected_results()

        if (length(current_selected) == 0) {
          showNotification("请先选择要下载的结果", type = "warning")
          return()
        }

        tryCatch({
          # 创建新的Word文档
          doc <- officer::read_docx()

          # 🟢 修复：按ID排序获取选中的结果
          selected_results_list <- current_results[as.character(current_selected)]
          selected_results_list <- selected_results_list[order(as.integer(names(selected_results_list)))]

          # 在添加表格前调整表格大小
          for (i in seq_along(selected_results_list)) {
            result <- selected_results_list[[i]]

            # 调整表格以适应页面
            if (is.list(result$flextable) && length(result$flextable) == 2) {

              ft1 <- result$flextable[[1]]
              doc <- flextable::body_add_flextable(doc, value = ft1)

              ft2 <- result$flextable[[2]]
              doc <- flextable::body_add_flextable(doc, value = ft2)
            } else {
              ft <- result$flextable
              doc <- flextable::body_add_flextable(doc, value = ft)
            }

            # 添加连续分节符
            if (i < length(selected_results_list)) {
              ps <- officer::prop_section(type = "continuous")
              doc <- officer::body_end_block_section(doc, officer::block_section(ps))
            }
          }

          # 保存文档
          print(doc, target = file)

          showNotification(paste("已成功下载", length(selected_results_list), "个分析结果"), type = "message")

        }, error = function(e) {
          message("下载错误详情: ", e$message)
          showNotification(paste("下载错误:", e$message), type = "error")
        })
      }
    )

    return(reactive({
      list(
        results_list = results_list(),
        selected_results = selected_results(),
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
