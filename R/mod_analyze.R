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
                              "生存分析" = "lifetest",
                              "生存分析可视化" = "lifetest_pic"
                              # ,"自定义R脚本" = "custom_script"
                              )
                  ),

      # 条件面板
      uiOutput(ns("analysis_params")),

      # 🟢 修改：动态渲染操作按钮
      uiOutput(ns("action_buttons"))
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
                              style = "color: #2c3e50; line-height: 1.4;"),
                   tags$br(),
                   tags$small("📊 下载结果: 获取RTF格式的分析表格",
                              style = "color: #2c3e50; line-height: 1.4; display: block;")
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
      lifetest = NULL,
      lifetest_pic = NULL
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
             "lifetest" = mod_lifetest_ui(ns("lifetest_1")),
             "lifetest_pic" = mod_lifetest_pic_ui(ns("lifetest_pic_1"))
      )
    })

    # 在 mod_analyze_server 函数中添加
    output$action_buttons <- renderUI({
      req(input$analysis_type)

      if (input$analysis_type != "custom_script") {
        analysis_action_buttons(ns)
      } else {
        # 自定义脚本时不显示任何按钮，或者显示其他内容
        NULL
      }
    })

    # 🟢 新增：初始化分析模块（延迟执行，确保UI已渲染）
    observeEvent(input$analysis_type, {
      req(input$analysis_type)

      # 更新数据上传模块中的分析类型
      if (!is.null(data_upload_module()$updateAnalysisType)) {
        data_upload_module()$updateAnalysisType(input$analysis_type)
        message("📤 传递分析类型到数据模块: ", input$analysis_type)
      }

      initialize_analysis_server(input$analysis_type, analysis_servers, data_upload_module, ns)

    })

    # 🟢 修复：清空参数按钮功能 - 只在用户点击时执行
    observeEvent(input$clear_params, {
      req(input$analysis_type)

      clear_analysis_params(input$analysis_type, analysis_servers, ns)

    })

    # 🟢 获取当前分析模块的参数
    get_current_params <- reactive({
      req(input$analysis_type)

      get_current_params_logic(input$analysis_type, analysis_servers)
    })

    # 初始化分析模块
    q_describe_params <- mod_q_describe_server("q_describe_1", data_upload_module)
    c_describe_params <- mod_c_describe_server("c_describe_1", data_upload_module)
    c_srt_params <- mod_c_srt_server("c_srt_1", data_upload_module)
    covancova_params <- mod_covancova_server("covancova_1", data_upload_module)
    q_param_params <- mod_q_param_server("q_param_1", data_upload_module)
    crosstable_params <- mod_crosstable_server("crosstable_1", data_upload_module)
    lifetest_params <- mod_lifetest_server("lifetest_1", data_upload_module)
    lifetest_pic_params <- mod_lifetest_pic_server("lifetest_pic_1", data_upload_module)

    observeEvent(input$run, {
      req(data_upload_module()$current_data)
      req(data_upload_module()$data_name)
      req(input$analysis_type)

      tryCatch({
        result_id <- rv$next_result_id
        rv$next_result_id <- result_id + 1  # 更新为下一个ID

        execute_analysis(input$analysis_type, data_upload_module, analysis_servers,
                         results_list, selected_results, result_id)
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

      render_plot_outputs(output, current_results, ns)

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

      selection_panel <- create_selection_panel(current_results, current_selected, ns)

      # 🟢 修复：按ID排序渲染所有结果
      sorted_results <- current_results[order(as.integer(names(current_results)))]

      result_elements <- lapply(sorted_results, function(result) {
        create_result_element(result, current_selected, ns)
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
        paste0("analysis_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rtf")
      },
      content = function(file) {
        current_results <- results_list()
        current_selected <- selected_results()

        download_selected_results(current_selected, current_results, file)
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
