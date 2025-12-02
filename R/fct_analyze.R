#' Analysis Helper Functions
#'
#' @description Helper functions for analysis module event handling and logic
#'
#' @noRd
analysis_action_buttons <- function(ns, run_id = "run", clear_id = "clear_params") {
  tags$div(
    style = "display: flex; justify-content: space-between; margin-top: 15px;",
    actionButton(ns(run_id), "运行分析",
                 icon = icon("play-circle"),
                 style = "background-color: #27ae60; color: white; border: none; font-weight: bold; padding: 8px 16px; border-radius: 4px; flex: 1; margin-right: 5px;"
    ),
    actionButton(ns(clear_id), "清空参数",
                 icon = icon("broom"),
                 style = "background-color: #e74c3c; color: white; border: none; flex: 1; margin-left: 5px;")
  )
}



get_analysis_name <- function(type) {
  switch(type,
         "q_describe" = "描述性统计",
         "c_describe" = "分类变量描述",
         "c_srt" = "秩和检验",
         "covancova" = "协方差分析",
         "q_param" = "组间/组内比较",
         "crosstable" = "2*2列联表",
         "lifetest" = "生存分析",
         "lifetest_pic" = "生存分析可视化"
         )
}

#' Remove Clear Params Function
#'
#' @param params Parameters list
#' @return Parameters list with clear_params removed
#'
#' @noRd
remove_clear_params <- function(params) {
  if (!is.null(params$clear_params)) {
    params$clear_params <- NULL
  }
  return(params)
}

#' Initialize Analysis Server
#'
#' @param analysis_type Type of analysis to initialize
#' @param analysis_servers ReactiveValues object containing server instances
#' @param data_upload_module Data upload module reactive
#' @param ns Namespace function
#'
#' @noRd
initialize_analysis_server <- function(analysis_type, analysis_servers, data_upload_module, ns) {
  current_module <- switch(analysis_type,
                           "q_describe" = "q_describe",
                           "c_describe" = "c_describe",
                           "c_srt" = "c_srt",
                           "covancova" = "covancova",
                           "q_param" = "q_param",
                           "crosstable" = "crosstable",
                           "lifetest" = "lifetest",
                           "lifetest_pic" = "lifetest_pic"
                           )

  if (is.null(analysis_servers[[current_module]])) {
    message("初始化分析模块: ", current_module)

    analysis_servers[[current_module]] <- switch(analysis_type,
                                                 "q_describe" = mod_q_describe_server("q_describe_1", data_upload_module),
                                                 "c_describe" = mod_c_describe_server("c_describe_1", data_upload_module),
                                                 "c_srt" = mod_c_srt_server("c_srt_1", data_upload_module),
                                                 "covancova" = mod_covancova_server("covancova_1", data_upload_module),
                                                 "q_param" = mod_q_param_server("q_param_1", data_upload_module),
                                                 "crosstable" = mod_crosstable_server("crosstable_1", data_upload_module),
                                                 "lifetest" = mod_lifetest_server("lifetest_1", data_upload_module),
                                                 "lifetest_pic" = mod_lifetest_pic_server("lifetest_pic_1", data_upload_module)
                                                 )
  }
}

#' Clear Analysis Parameters
#'
#' @param analysis_type Type of analysis to clear parameters for
#' @param analysis_servers ReactiveValues object containing server instances
#' @param ns Namespace function
#'
#' @noRd
clear_analysis_params <- function(analysis_type, analysis_servers, ns) {
  message("🧹 用户点击清空参数按钮: ", analysis_type)

  current_server <- switch(analysis_type,
                           "q_describe" = analysis_servers$q_describe,
                           "c_describe" = analysis_servers$c_describe,
                           "c_srt" = analysis_servers$c_srt,
                           "covancova" = analysis_servers$covancova,
                           "q_param" = analysis_servers$q_param,
                           "crosstable" = analysis_servers$crosstable,
                           "lifetest" = analysis_servers$lifetest,
                           "lifetest_pic" = analysis_servers$lifetest_pic
                           )

  if (!is.null(current_server)) {
    tryCatch({
      module_result <- current_server()
      message("✅ 找到分析模块: ", analysis_type)

      if (!is.null(module_result$clear_params)) {
        message("✅ 找到 clear_params 方法，开始执行...")
        module_result$clear_params()
        showNotification(paste("已清空", get_analysis_name(analysis_type), "参数"),
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
    message("❌ 分析模块尚未初始化: ", analysis_type)
    showNotification("分析模块尚未初始化，无法清空参数", type = "warning")
  }
}

#' Get Current Analysis Parameters Logic
#'
#' @param analysis_type Type of analysis
#' @param analysis_servers ReactiveValues object containing server instances
#'
#' @noRd
get_current_params_logic <- function(analysis_type, analysis_servers) {
  switch(analysis_type,
         "q_describe" = if (!is.null(analysis_servers$q_describe)) analysis_servers$q_describe() else NULL,
         "c_describe" = if (!is.null(analysis_servers$c_describe)) analysis_servers$c_describe() else NULL,
         "c_srt" = if (!is.null(analysis_servers$c_srt)) analysis_servers$c_srt() else NULL,
         "covancova" = if (!is.null(analysis_servers$covancova)) analysis_servers$covancova() else NULL,
         "q_param" = if (!is.null(analysis_servers$q_param)) analysis_servers$q_param() else NULL,
         "crosstable" = if (!is.null(analysis_servers$crosstable)) analysis_servers$crosstable() else NULL,
         "lifetest" = if (!is.null(analysis_servers$lifetest)) analysis_servers$lifetest() else NULL,
         "lifetest_pic" = if (!is.null(analysis_servers$lifetest_pic)) analysis_servers$lifetest_pic() else NULL
         )
}

#' Execute Analysis
#'
#' @param analysis_type Type of analysis to execute
#' @param data_upload_module Data upload module reactive
#' @param analysis_servers ReactiveValues object containing server instances
#' @param results_list ReactiveVal for results list
#' @param selected_results ReactiveVal for selected results
#' @param next_result_id Reactive value for next result ID
#'
#' @noRd
execute_analysis <- function(analysis_type, data_upload_module, analysis_servers,
                             results_list, selected_results, result_id) {

  message("=== 分析模块调试信息 ===")
  message("点击运行分析时间: ", Sys.time())

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

  data_name <- data_upload_module()$data_name
  current_data <- data_upload_module()$current_data

  print(paste("正在执行分析:", analysis_type))
  print(paste("数据名称:", data_name))
  print(paste("数据维度:", dim(data_upload_module()$current_data), collapse = "x"))
  print(paste("数据状态:", ifelse(data_upload_module()$is_filtered, "已筛选", "原始")))

  analysis_func <- switch(analysis_type,
                          "q_describe" = function() {
                            params <- analysis_servers$q_describe()
                            # 🆕 生成代码调用
                            code_call <- generate_analysis_code(analysis_type, params, data_upload_module()$data_name)
                            result <- do.call(q_describe, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "c_describe" = function() {
                            params <- analysis_servers$c_describe()
                            code_call <- generate_analysis_code(analysis_type, params, data_upload_module()$data_name)
                            result <- do.call(c_describe, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "c_srt" = function() {
                            params <- analysis_servers$c_srt()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)
                            result <- do.call(c_srt, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "covancova" = function() {
                            params <- analysis_servers$covancova()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)
                            result <- do.call(covancova, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "q_param" = function() {
                            params <- analysis_servers$q_param()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)
                            result <- do.call(q_param, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "crosstable" = function() {
                            params <- analysis_servers$crosstable()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)
                            result <- do.call(c_crosstable, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "lifetest" = function() {
                            params <- analysis_servers$lifetest()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)
                            result <- do.call(lifetest, c(list(inds = current_data), remove_clear_params(params)))
                            return(list(flextable = result, code_call = code_call))
                          },
                          "lifetest_pic" = function() {  # 🆕 新增生存分析可视化
                            params <- analysis_servers$lifetest_pic()
                            code_call <- generate_analysis_code(analysis_type, params, data_name)

                            # 调用生存分析绘图函数
                            plot_result <- do.call(lifetest_pic, c(list(inds = current_data), remove_clear_params(params)))

                            # 对于绘图结果，我们需要特殊处理
                            # 返回一个包含绘图对象的列表
                            return(list(
                              plot = plot_result,  # 保存绘图对象
                              code_call = code_call,
                              is_plot = TRUE  # 标记这是绘图结果
                            ))
                          }
                          )

  if (!is.null(analysis_func)) {
    analysis_output <- analysis_func()

    # 🆕 特殊处理绘图结果
    if (!is.null(analysis_output$is_plot) && analysis_output$is_plot) {
      new_result <- analysis_output$plot
      is_plot_result <- TRUE
    } else {
      new_result <- analysis_output$flextable
      is_plot_result <- FALSE
    }


    # new_result <- analysis_output$flextable
    code_call <- analysis_output$code_call

    current_results <- results_list()

    result_name <- paste0(get_analysis_name(analysis_type), " #", result_id)

    current_results[[as.character(result_id)]] <- list(
      id = result_id,
      name = result_name,
      analysis_type = analysis_type,
      timestamp = Sys.time(),
      flextable = new_result,
      data_name = data_upload_module()$data_name,  # 🆕 保存数据名称
      is_plot = is_plot_result
    )

    results_list(current_results)

    current_selected <- selected_results()
    selected_results(c(current_selected, result_id))

    showNotification("分析完成！结果已添加到列表中", type = "message")
  }
}

#' Create Selection Panel
#'
#' @param current_results Current results list
#' @param current_selected Currently selected result IDs
#' @param ns Namespace function
#'
#' @noRd
create_selection_panel <- function(current_results, current_selected, ns) {
  all_result_ids <- as.integer(names(current_results))
  is_all_selected <- length(current_selected) == length(all_result_ids)

  tags$div(
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
}

#' Create Result Element
#'
#' @param result Result object
#' @param current_selected Currently selected result IDs
#' @param ns Namespace function
#'
#' @noRd
create_result_element <- function(result, current_selected, ns) {
  is_selected <- result$id %in% current_selected
  toggle_id <- paste0("toggle_", result$id)

  tags$div(
    id = ns(paste0("result_", result$id)),
    style = paste("border: 2px solid", if (is_selected) "#3498db" else "#e9ecef",
                  "; padding: 15px; margin-bottom: 15px; border-radius: 8px;
                 background-color:", if (is_selected) "#f0f8ff" else "white", ";"),

    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; padding-bottom: 8px; border-bottom: 1px solid #dee2e6;",
      tags$div(
        tags$strong(paste("分析类型:", result$name), style = "color: #2c3e50;"),
        tags$br(),
        tags$small(paste("生成时间:", format(result$timestamp, "%Y-%m-%d %H:%M:%S")),
                   style = "color: #6c757d;"),
        # 🆕 显示结果类型标记
        if (!is.null(result$is_plot) && result$is_plot) {
          tags$small("📊 可视化结果", style = "color: #e67e22; font-weight: bold; margin-left: 10px;")
        } else {
          tags$small("📋 表格结果", style = "color: #3498db; font-weight: bold; margin-left: 10px;")
        }
      ),
      tags$div(
        actionButton(ns(toggle_id),
                     label = if (is_selected) "取消选择" else "选择",
                     style = paste("padding: 4px 8px; font-size: 12px;",
                                   if (is_selected) "background-color: #e74c3c; color: white;"
                                   else "background-color: #3498db; color: white;"))
      )
    ),

    if (is_selected) {
      # 🆕 特殊处理绘图结果
      if (!is.null(result$is_plot) && result$is_plot) {
        # 对于绘图结果，直接渲染ggplot对象
        plotOutput(ns(paste0("plot_", result$id)), height = "400px")
      }
      else if (is.list(result$flextable) && length(result$flextable) == 2) {
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
}

#' Render Plot Outputs
#'
#' @param output Shiny output object
#' @param results_list Reactive results list
#' @param ns Namespace function
#'
#' @noRd
render_plot_outputs <- function(output, results_list, ns) {
  current_results <- results_list

  # 为每个绘图结果创建渲染器
  for (result_id in names(current_results)) {
    result <- current_results[[result_id]]

    if (!is.null(result$is_plot) && result$is_plot) {
      # 使用局部变量避免闭包问题
      local_plot <- result$flextable
      output_id <- paste0("plot_", result_id)

      output[[output_id]] <- renderPlot({
        if (!is.null(local_plot)) {
          local_plot  # 直接返回ggplot对象
        } else {
          # 如果绘图对象为空，显示提示信息
          plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
          text(0, 0, "绘图数据为空", cex = 1.5, col = "gray")
        }
      })
    }
  }
}

#' Download Selected Results
#'
#' @param selected_results Selected result IDs
#' @param results_list Results list
#' @param file Output file path
#'
#' @noRd
download_selected_results <- function(selected_results, results_list, file) {
  if (length(selected_results) == 0) {
    showNotification("请先选择要下载的结果", type = "warning")
    return()
  }

  tryCatch({
    rtf_doc <- officer::rtf_doc()

    selected_results_list <- results_list[as.character(selected_results)]
    selected_results_list <- selected_results_list[order(as.integer(names(selected_results_list)))]

    for (i in seq_along(selected_results_list)) {
      result <- selected_results_list[[i]]
      empty_para <- officer::fpar(officer::ftext(""))

      if (!is.null(result$is_plot) && result$is_plot) {
        tryCatch({
          # 方法1：直接添加（首选）
          rtf_doc <- officer::rtf_add(rtf_doc,
                                      result$flextable,
                                      width = 7,
                                      height = 4.5)

        }, error = function(e) {
          message("直接添加生存分析图失败: ", e$message)

          # 方法2：转换为ggplot对象再添加
          tryCatch({
            if (inherits(result$flextable, "ggsurv")) {
              # 如果已经是ggplot兼容对象，直接添加
              rtf_doc <- officer::rtf_add(rtf_doc,
                                          result$flextable,
                                          width = 6,
                                          height = 4.5)
            }
          }, error = function(e2) {
            # 最终错误处理
            error_text <- officer::fpar(
              officer::ftext("生存分析图导出失败，建议使用图片下载功能",
                             prop = officer::fp_text(color = "red"))
            )
            rtf_doc <- officer::rtf_add(rtf_doc, error_text)
          })
        })
      } else if (is.list(result$flextable) && length(result$flextable) == 2) {
        ft1 <- result$flextable[[1]]
        rtf_doc <- officer::rtf_add(rtf_doc, ft1)

        ft2 <- result$flextable[[2]]
        rtf_doc <- officer::rtf_add(rtf_doc, ft2)
      } else {
        ft <- result$flextable
        rtf_doc <- officer::rtf_add(rtf_doc, ft)
      }

      if (i < length(selected_results_list)) {
        for (j in 1:4) {
          rtf_doc <- officer::rtf_add(rtf_doc, empty_para)
        }

        rtf_doc <- officer::rtf_add(rtf_doc,
                                    officer::block_section(
                                      officer::prop_section(type = "continuous")
                                    ))
      }
    }

    print(rtf_doc, target = file)
    showNotification(paste("已成功下载", length(selected_results_list), "个分析结果"), type = "message")

  }, error = function(e) {
    message("下载错误详情: ", e$message)
    showNotification(paste("下载错误:", e$message), type = "error")
  })
}
