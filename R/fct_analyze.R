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



.analysis_registry <- function(type = NULL) {
  registry <- list(
    q_describe = list(label = "描述性统计", module = "q_describe", ui = mod_q_describe_ui, server = mod_q_describe_server,
                      compute = q_describe, denominator_data = TRUE),
    c_describe = list(label = "分类变量描述", module = "c_describe", ui = mod_c_describe_ui, server = mod_c_describe_server,
                      compute = c_describe, denominator_data = TRUE),
    c_srt = list(label = "秩和检验", module = "c_srt", ui = mod_c_srt_ui, server = mod_c_srt_server, compute = c_srt),
    covancova = list(label = "协方差分析", module = "covancova", ui = mod_covancova_ui, server = mod_covancova_server,
                     compute = covancova),
    q_param = list(label = "组间/组内比较", module = "q_param", ui = mod_q_param_ui, server = mod_q_param_server,
                   compute = q_param, denominator_data = TRUE),
    crosstable = list(label = "2*2列联表", module = "crosstable", ui = mod_crosstable_ui, server = mod_crosstable_server,
                      compute = c_crosstable, function_name = "c_crosstable"),
    lifetest = list(label = "生存分析", module = "lifetest", ui = mod_lifetest_ui, server = mod_lifetest_server,
                    compute = lifetest),
    lifetest_pic = list(label = "生存分析可视化", module = "lifetest_pic", ui = mod_lifetest_pic_ui, server = mod_lifetest_pic_server,
                        compute = lifetest_pic, is_plot = TRUE),
    c_cmh = list(label = "CMH检验", module = "c_cmh", ui = mod_c_cmh_ui, server = mod_c_cmh_server, compute = c_cmh),
    c_chisq = list(label = "卡方检验", module = "c_chisq", ui = mod_c_chisq_ui, server = mod_c_chisq_server, compute = c_chisq),
    riskdiff = list(label = "率差分析", module = "riskdiff", ui = mod_riskdiff_ui, server = mod_riskdiff_server, compute = riskdiff),
    aecnp = list(label = "不良事件频数分析", module = "aecnp", ui = mod_aecnp_ui, server = mod_aecnp_server,
                 compute = aecnp, denominator_data = TRUE),
    cnpsummary = list(label = "事件发生率", module = "cnpsummary", ui = mod_cnpsummary_ui, server = mod_cnpsummary_server,
                      compute = cnpsummary, denominator_data = TRUE),
    Tmax = list(label = "Tmax非参数检验", module = "Tmax", ui = mod_Tmax_ui, server = mod_Tmax_server, compute = Tmax),
    pic_vline = list(label = "折线图可视化", module = "pic_vline", ui = mod_pic_vline_ui, server = mod_pic_vline_server,
                     compute = pic_vline, is_plot = TRUE),
    q_pairt = list(label = "配对t检验", module = "q_pairt", ui = mod_q_pairt_ui, server = mod_q_pairt_server,
                   compute = q_pairt, denominator_data = TRUE),
    q_nonparam = list(label = "非参数检验", module = "q_nonparam", ui = mod_q_nonparam_ui, server = mod_q_nonparam_server,
                      compute = q_nonparam, denominator_data = TRUE),
    custom_script = list(label = "自定义R脚本", module = "custom_script", ui = mod_custom_script_ui, server = mod_custom_script_server,
                         custom = TRUE)
  )

  if (is.null(type)) {
    return(registry)
  }
  registry[[type]]
}

.analysis_log <- function(...) {
  if (isTRUE(getOption("BioStatsSuite.verbose", FALSE))) {
    message(...)
  }
}

.analysis_server_get <- function(analysis_type, analysis_servers) {
  info <- .analysis_registry(analysis_type)
  if (is.null(info)) {
    return(NULL)
  }
  analysis_servers[[info$module]]
}

get_analysis_name <- function(type) {
  info <- .analysis_registry(type)
  if (!is.null(info)) info$label else NULL
}

render_analysis_ui <- function(analysis_type, ns) {
  info <- .analysis_registry(analysis_type)
  if (is.null(info) || is.null(info$ui)) {
    return(NULL)
  }
  info$ui(ns(paste0(info$module, "_1")))
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
  info <- .analysis_registry(analysis_type)
  if (is.null(info)) {
    return(invisible(NULL))
  }

  current_module <- info$module
  if (is.null(analysis_servers[[current_module]])) {
    .analysis_log("初始化分析模块: ", current_module)
    analysis_servers[[current_module]] <- info$server(paste0(current_module, "_1"), data_upload_module)
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
  .analysis_log("🧹 用户点击清空参数按钮: ", analysis_type)

  current_server <- .analysis_server_get(analysis_type, analysis_servers)

  if (!is.null(current_server)) {
    tryCatch({
      module_result <- current_server()
      .analysis_log("✅ 找到分析模块: ", analysis_type)

      if (!is.null(module_result$clear_params)) {
        .analysis_log("✅ 找到 clear_params 方法，开始执行...")
        module_result$clear_params()
        showNotification(paste("已清空", get_analysis_name(analysis_type), "参数"),
                         type = "message")
      } else {
        .analysis_log("❌ 未找到 clear_params 方法")
        showNotification("该分析方法暂无清空参数功能", type = "warning")
      }
    }, error = function(e) {
      .analysis_log("❌ 清空参数错误: ", e$message)
      showNotification(paste("清空参数失败:", e$message), type = "error")
    })
  } else {
    .analysis_log("❌ 分析模块尚未初始化: ", analysis_type)
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
  current_server <- .analysis_server_get(analysis_type, analysis_servers)
  if (!is.null(current_server)) current_server() else NULL
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

  data_info <- data_upload_module()
  .analysis_log("=== 分析模块调试信息 ===")
  .analysis_log("点击运行分析时间: ", Sys.time())
  .analysis_log("data_upload_module()是否为NULL: ", is.null(data_info))

  if (!is.null(data_info)) {
    .analysis_log("data_info中的元素: ", paste(names(data_info), collapse = ", "))
    .analysis_log("current_data是否为NULL: ", is.null(data_info$current_data))
    if (!is.null(data_info$current_data)) {
      .analysis_log("current_data维度: ", nrow(data_info$current_data), " x ", ncol(data_info$current_data))
      .analysis_log("current_data列名: ", paste(names(data_info$current_data), collapse = ", "))
    }
    .analysis_log("data_name: ", data_info$data_name)
    .analysis_log("is_filtered: ", data_info$is_filtered)
  }

  info <- .analysis_registry(analysis_type)
  if (is.null(info)) {
    stop("未知分析类型: ", analysis_type)
  }

  data_name <- data_info$data_name
  current_data <- data_info$current_data

  .analysis_log("正在执行分析: ", analysis_type)
  .analysis_log("数据名称: ", data_name)
  .analysis_log("数据维度: ", paste(dim(current_data), collapse = "x"))
  .analysis_log("数据状态: ", ifelse(data_info$is_filtered, "已筛选", "原始"))

  current_params <- get_current_params_logic(analysis_type, analysis_servers)
  outyn_value <- if (!is.null(current_params) && !is.null(current_params$outyn)) current_params$outyn else 1

  if (isTRUE(info$custom)) {
    params <- .analysis_server_get(analysis_type, analysis_servers)()
    script_result <- params()
    if (is.null(script_result)) {
      stop("自定义脚本执行失败")
    }
    analysis_output <- list(
      flextable = script_result$flextable,
      code_call = "# 自定义R脚本执行\n# 代码内容请参考自定义脚本模块"
    )
  } else {
    params <- current_params
    clean_params <- remove_clear_params(params)
    if (isTRUE(info$denominator_data) && is.null(clean_params$denominator_data)) {
      clean_params$denominator_data <- current_data
    }

    code_call <- generate_analysis_code(analysis_type, params, data_name)
    result <- do.call(info$compute, c(list(inds = current_data), clean_params))

    if (isTRUE(info$is_plot)) {
      analysis_output <- list(plot = result, code_call = code_call, is_plot = TRUE)
    } else {
      analysis_output <- list(flextable = result, code_call = code_call)
    }
  }

  # outyn=0 时仅累积不渲染，outyn=1 时正常出表
  if (outyn_value == 0) {
    showNotification("结果已累积保存（未勾选立即出表），下次勾选后出表将包含本次结果。请注意：叠加表格需使用相同的数据集和同类分析方法，以确保表头格式及样本量N一致", type = "message")
    return(invisible(NULL))
  }

  is_plot_result <- FALSE
  table_result <- NULL
  plot_result <- NULL

  if (!is.null(analysis_output$is_plot) && analysis_output$is_plot) {
    is_plot_result <- TRUE
    plot_result <- analysis_output$plot
  } else {
    table_result <- analysis_output$flextable
  }

  code_call <- analysis_output$code_call

  current_results <- results_list()
  result_name <- paste0(get_analysis_name(analysis_type), " #", result_id)

  current_results[[as.character(result_id)]] <- list(
    id = result_id,
    name = result_name,
    analysis_type = analysis_type,
    timestamp = Sys.time(),
    table = table_result,
    plot = plot_result,
    code_call = code_call,
    data_name = data_info$data_name,
    is_plot = is_plot_result
  )

  results_list(current_results)

  current_selected <- selected_results()
  selected_results(c(current_selected, result_id))

  showNotification("分析完成！结果已添加到列表中", type = "message")
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
      # 🟢 修改：根据结果类型分别渲染
      if (!is.null(result$is_plot) && result$is_plot) {
        # 对于绘图结果，渲染plotOutput
        plotOutput(ns(paste0("plot_", result$id)), height = "600px")
      } else if (!is.null(result$table)) {
        # 对于表格结果，渲染flextable
        if (is.list(result$table) && length(result$table) == 2) {
          htmltools::HTML(
            paste(
              as.character(flextable::htmltools_value(result$table[[1]])),
              as.character(flextable::htmltools_value(result$table[[2]])),
              sep = "<br><br>"
            )
          )
        } else {
          htmltools::HTML(as.character(flextable::htmltools_value(result$table)))
        }
      } else {
        # 无结果的情况
        tags$div(
          style = "text-align: center; padding: 20px; color: #6c757d;",
          icon("exclamation-triangle"), "结果为空"
        )
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

  # 清空之前的绘图输出
  plot_output_ids <- grep("^plot_\\d+$", names(output), value = TRUE)
  for (output_id in plot_output_ids) {
    output[[output_id]] <- NULL
  }

  # 为每个绘图结果创建渲染器
  for (result_id in names(current_results)) {
    result <- current_results[[result_id]]

    if (!is.null(result$is_plot) && result$is_plot) {
      # 创建独立的作用域，复制绘图对象
      local({
        local_result_id <- result_id
        local_plot <- result$plot
        output_id <- paste0("plot_", local_result_id)

        # 使用 renderCachedPlot 或创建一个封闭的环境
        output[[output_id]] <- renderPlot({
          if (!is.null(local_plot)) {
            # 直接返回ggplot对象
            local_plot
          } else {
            # 如果绘图对象为空，显示提示信息
            plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
            text(0, 0, "绘图数据为空", cex = 1.5, col = "gray")
          }
        })
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
# 单列最小宽度（英寸），避免字体度量失败时列被压成一条竖线。
.RTF_MIN_COL_WIDTH_IN <- 0.4

# 构造 RTF 文档的页面节，与 docx 输出保持同一套页面尺寸和页边距。
#
# 不能依赖 officer::rtf_doc() 的默认节：其默认为 A4（8.27in）配 0.98in 页边距，
# 与 hbr3 样式（2.5cm / 2.0cm 页边距）不一致，会导致表格和图形按错误宽度排版。
.rtf_prop_section <- function(sp = NULL) {
  if (is.null(sp)) sp <- hbr3_style()
  officer::prop_section(
    page_size    = sp$page_size_port,
    page_margins = sp$page_mar
  )
}

# RTF 正文可用宽度（英寸）= 页宽 - 左右页边距，与 .rtf_prop_section() 对应。
.rtf_content_width <- function(sp = NULL) {
  if (is.null(sp)) sp <- hbr3_style()

  width <- tryCatch(
    sp$page_size_port$width - sp$page_mar$left - sp$page_mar$right,
    error = function(e) NA_real_
  )

  if (!is.finite(width) || width <= 0) {
    # A4 纵向 8.27in 减去 hbr3 的左右页边距（2.5cm / 2.0cm）。
    width <- 210 / 25.4 - 2.5 / 2.54 - 2.0 / 2.54
  }
  width
}

# 计算 RTF 表格的显式列宽。
#
# flextable 的 dim_pretty() 依据字体度量估算每列所需宽度；若度量不可用
# （例如中文字体未安装），回退到按字符数估算，并统一施加最小宽度下限。
# 最后按页面可用宽度等比缩放，保证表格不超出纸张。
.rtf_col_widths <- function(ft, max_width = NULL) {
  n_col <- length(ft$col_keys)
  if (n_col == 0) return(numeric(0))

  if (is.null(max_width)) max_width <- .rtf_content_width(.rtf_style())

  # 只按 header + body 估算：footer 是跨列合并的长脚注，
  # 计入后会把第一列撑得极宽，把其余列挤成竖条。
  widths <- tryCatch({
    w_head <- flextable::dim_pretty(ft, part = "header")$widths
    w_body <- flextable::dim_pretty(ft, part = "body")$widths
    pmax(w_head, w_body)
  }, error = function(e) NULL)

  if (is.null(widths) || length(widths) != n_col || !all(is.finite(widths))) {
    widths <- rep(max_width / n_col, n_col)
  }

  # 列过多时最小宽度之和已超过页宽，直接均分，避免缩放后仍然溢出。
  if (n_col * .RTF_MIN_COL_WIDTH_IN >= max_width) {
    return(rep(max_width / n_col, n_col))
  }

  widths <- pmax(widths, .RTF_MIN_COL_WIDTH_IN)

  # 先缩放再兜底重分配：直接缩放会把已达下限的窄列再次压到下限以下。
  total <- sum(widths)
  if (is.finite(total) && total > max_width) {
    widths <- widths * (max_width / total)

    # 缩放后仍需保证下限：把超出下限的列按超出量按比例让出空间。
    short <- widths < .RTF_MIN_COL_WIDTH_IN
    if (any(short)) {
      deficit <- sum(.RTF_MIN_COL_WIDTH_IN - widths[short])
      widths[short] <- .RTF_MIN_COL_WIDTH_IN

      slack <- widths[!short] - .RTF_MIN_COL_WIDTH_IN
      total_slack <- sum(slack)
      if (total_slack > 0) {
        widths[!short] <- widths[!short] - slack * (deficit / total_slack)
      } else {
        widths <- rep(max_width / n_col, n_col)
      }
    }
  }

  widths
}

# 为 RTF 导出准备 flextable
#
# 1. 字体：RTF 写出时若未显式声明中西文字体，中文会因缺少字形而显示异常，
#    因此统一按 .rtf_style() 补齐字体设置。
# 2. 列宽：RTF 不支持 Word 的 autofit 布局（report_table() 会设置
#    set_table_properties(layout = "autofit")），必须改为 fixed 布局
#    并写入显式列宽，否则所有列会塌缩成一个字符宽。
.rtf_style <- function() {
  sp <- tryCatch(.doc_ctx_get_style(), error = function(e) NULL)

  # RTF 下载通常在未初始化报告文档时触发，此时回退到报告配置里常用的中文字体。
  font_cn <- tryCatch(.doc_ctx_get_params()$font, error = function(e) NULL)
  font_cn <- .blank_default(font_cn, "SimSun")

  if (is.null(sp)) sp <- hbr3_style(font_cn = font_cn)

  # RTF 不支持 Word 的 eastAsia 字体分槽：officer 写 RTF 时只使用 font.family，
  # 若保持西文字体（Times New Roman），中文会因缺少字形而输出乱码。
  # 这里将主字体切换为中文字体，使中英文都能正常显示。
  sp$font_cn <- .blank_default(sp$font_cn, font_cn)
  sp$font_en <- sp$font_cn
  sp
}

# 仅替换字体族，保留已有的对齐、加粗、边框等样式。
#
# 不能直接复用 .apply_style_to_ft()：report_table() 内部已调用过它，
# 之后还叠加了 doubleheader 细线、分组表头居中和 bold_rows 加粗。
# 再整套套用一次会把这些全部擦除。
#
# 用 flextable::font() 而非 style()：后者会整体重写 fp_text，
# 连带把 bold_rows 的加粗一起清掉。
.rtf_apply_font <- function(ft, sp) {
  set_font <- function(ft, part, size) {
    ft <- flextable::font(ft, part = part,
                          fontname = sp$font_en,
                          eastasia.family = sp$font_cn,
                          cs.family = sp$font_en)
    flextable::fontsize(ft, part = part, size = size)
  }

  ft <- set_font(ft, "header", sp$font_size)
  ft <- set_font(ft, "body", sp$font_size)
  if (!is.null(ft$footer) && length(ft$footer$dataset) > 0) {
    ft <- set_font(ft, "footer", max(sp$font_size - 1, 8))
  }
  ft
}

.prepare_ft_for_rtf <- function(ft, restyle = TRUE, styling = NULL) {
  if (is.null(ft) || !inherits(ft, "flextable")) return(ft)
  tryCatch({
    sp <- .rtf_style()

    if (isTRUE(restyle)) {
      # 未经 report_table() 渲染的表格：套用完整 hbr3 样式。
      # styling 需由调用方传入：report_table() 返回的新表不带 hbr_* 属性，
      # 只读 attr() 会拿到 NULL，导致居中的表被重置成左对齐。
      if (is.null(styling)) styling <- attr(ft, "hbr_styling_params")
      ft <- .apply_style_to_ft(
        ft,
        sp         = sp,
        headerjust = styling$headerjust %||% "left",
        col1just   = styling$col1just   %||% "left",
        bodyjust   = styling$columnjust %||% "left"
      )
    } else {
      # 已由 report_table() 渲染：只换字体，保留其对齐/加粗/边框。
      ft <- .rtf_apply_font(ft, sp)
    }

    # 关键：清除 Word 的 autofit 布局，改用 fixed 布局 + 显式列宽。
    # RTF 不支持 autofit，保留该属性会导致所有列塌缩成一个字符宽。
    widths <- .rtf_col_widths(ft)
    ft <- flextable::set_table_properties(ft, layout = "fixed", align = "left")
    for (j in seq_along(widths)) {
      ft <- flextable::width(ft, j = j, width = widths[j])
    }

    ft <- flextable::fix_border_issues(ft)
    ft
  }, error = function(e) {
    message("RTF flextable 预处理失败: ", e$message)
    ft
  })
}

# 将分析结果的 flextable 重新走 report_table() 渲染，使 RTF 内容与 docx 对齐。
# 分析函数会在 flextable 上附着 hbr_varlist / hbr_title / hbr_footnote，
# 这里复用同一套参数重建表格，避免 RTF 丢失表头标签与三线表样式。
.rebuild_ft_for_rtf <- function(ft) {
  if (is.null(ft) || !inherits(ft, "flextable")) return(ft)

  varlist <- attr(ft, "hbr_varlist")
  if (is.null(varlist)) {
    return(.prepare_ft_for_rtf(ft))
  }

  data <- tryCatch(as.data.frame(ft$body$dataset, stringsAsFactors = FALSE),
                   error = function(e) NULL)
  if (is.null(data) || nrow(data) == 0) {
    return(.prepare_ft_for_rtf(ft))
  }

  styling_params <- attr(ft, "hbr_styling_params")

  rebuilt <- tryCatch(
    do.call(report_table, c(list(
      data         = data,
      varlist      = varlist,
      title        = attr(ft, "hbr_title"),
      footnote     = attr(ft, "hbr_footnote"),
      # RTF 下载是独立于报告导出的旁路输出，
      # 关闭自动编号可避免污染报告文档的表格计数。
      autoaddnum   = "no",
      write_to_doc = FALSE
    ), styling_params)),
    error = function(e) {
      message("RTF 表格重建失败，使用原始表格: ", e$message)
      NULL
    }
  )

  # 重建成功时表格已带完整 report_table 样式，只需换字体 + 定列宽；
  # 重建失败回退到原表时才需要套用整套样式（并显式传入原表的样式参数）。
  if (!is.null(rebuilt)) {
    .prepare_ft_for_rtf(rebuilt, restyle = FALSE)
  } else {
    .prepare_ft_for_rtf(ft, restyle = TRUE, styling = styling_params)
  }
}

# 表格标题段落。
#
# 注意：多数分析函数（如 q_describe）已把表格标题写进 varlist 的第一列表头
# （见 utils_q_describe.R 的 paste0(".label/", table_title)），此时再额外输出
# 一行标题会造成重复。因此仅当标题未出现在表头中时才补一个标题段落。
.rtf_table_title <- function(ft, fallback) {
  title <- .blank_default(attr(ft, "hbr_title"), NULL)
  if (is.null(title)) title <- .blank_default(fallback, NULL)
  if (is.null(title)) return(NULL)

  title <- trimws(as.character(title))

  # 标题已作为某一列的表头输出时不再重复。
  # varlist 形如 "col1/标签1|col2/标签2|..."，逐项取标签精确比较，
  # 避免子串匹配把短标题误判成已存在。
  varlist <- attr(ft, "hbr_varlist")
  if (!is.null(varlist)) {
    parts <- strsplit(as.character(varlist), "|", fixed = TRUE)[[1]]
    labels <- vapply(parts, function(p) {
      kv <- strsplit(p, "/", fixed = TRUE)[[1]]
      if (length(kv) >= 2) trimws(paste(kv[-1], collapse = "/")) else ""
    }, character(1), USE.NAMES = FALSE)

    if (title %in% labels) return(NULL)
  }

  .rtf_plot_title(title, .rtf_style())
}

# 构造 RTF 中的加粗标题段落（表格与图形共用）。
.rtf_plot_title <- function(title, sp = NULL) {
  title <- .blank_default(title, NULL)
  if (is.null(title)) return(NULL)
  if (is.null(sp)) sp <- .rtf_style()

  officer::fpar(
    officer::ftext(
      trimws(as.character(title)),
      prop = officer::fp_text(
        bold            = TRUE,
        font.size       = sp$font_size,
        font.family     = sp$font_en,
        eastasia.family = sp$font_cn,
        cs.family       = sp$font_en
      )
    )
  )
}

# 下载失败时仍需写出目标文件：downloadHandler 的 content 若不写文件，
# 浏览器会拿到一个 0 字节且无法打开的附件。
.write_rtf_placeholder <- function(file, msg) {
  tryCatch({
    doc <- officer::rtf_doc()
    doc <- officer::rtf_add(doc, officer::fpar(officer::ftext(msg)))
    print(doc, target = file)
  }, error = function(e) {
    writeLines(msg, file, useBytes = TRUE)
  })
  invisible(NULL)
}

download_selected_results <- function(selected_results, results_list, file) {
  if (length(selected_results) == 0) {
    showNotification("请先选择要下载的结果", type = "warning")
    .write_rtf_placeholder(file, "未选择任何分析结果，请先在结果列表中勾选后再下载。")
    return(invisible(NULL))
  }

  tryCatch({
    sp <- .rtf_style()

    # 显式设置页面节，使 RTF 与 docx 使用同一套页面尺寸/页边距。
    rtf_doc <- tryCatch(
      officer::rtf_doc(def_sec = .rtf_prop_section(sp)),
      error = function(e) {
        message("RTF 页面节设置失败，使用默认页面: ", e$message)
        officer::rtf_doc()
      }
    )

    plot_width <- .rtf_content_width(sp)

    selected_results_list <- results_list[as.character(selected_results)]
    selected_results_list <- selected_results_list[order(as.integer(names(selected_results_list)))]

    for (i in seq_along(selected_results_list)) {
      result <- selected_results_list[[i]]
      empty_para <- officer::fpar(officer::ftext(""))

      if (!is.null(result$is_plot) && result$is_plot) {
        # 图标题：docx 侧由 figtitle() 输出编号标题，RTF 侧同样补一行标题，
        # 否则多张图连续输出时无法区分。
        plot_title <- .rtf_plot_title(result$name, sp)
        if (!is.null(plot_title)) {
          rtf_doc <- officer::rtf_add(rtf_doc, plot_title)
        }

        # 注意：必须通过返回值更新 rtf_doc。
        # tryCatch 的 error handler 是独立闭包，在其中用 <- 赋值只会写入
        # 闭包自身的作用域，外层 rtf_doc 不会被更新（降级内容会被静默丢弃）。
        rtf_doc <- tryCatch({
          if (!is.null(result$plot)) {
            # 宽度不能超过正文区，否则图会溢出页面被裁切。
            officer::rtf_add(rtf_doc, result$plot,
                             width  = plot_width,
                             height = plot_width / 2)
          } else {
            rtf_doc
          }
        }, error = function(e) {
          message("直接添加生存分析图失败: ", e$message)

          tryCatch({
            officer::rtf_add(rtf_doc, result$plot,
                             width  = plot_width * 0.8,
                             height = plot_width * 0.8 / 2)
          }, error = function(e2) {
            message("绘图降级导出失败: ", e2$message)
            error_text <- officer::fpar(
              officer::ftext("生存分析图导出失败，建议使用图片下载功能",
                             prop = officer::fp_text(color = "red"))
            )
            officer::rtf_add(rtf_doc, error_text)
          })
        })
      } else if (!is.null(result$table)) {
        # 🟢 修改：处理表格结果（与 docx 导出一致，走 report_table 渲染）
        tables <- if (is.list(result$table) && !inherits(result$table, "flextable")) {
          result$table
        } else {
          list(result$table)
        }

        for (tbl in tables) {
          if (!inherits(tbl, "flextable")) next

          title_par <- .rtf_table_title(tbl, result$name)
          if (!is.null(title_par)) {
            rtf_doc <- officer::rtf_add(rtf_doc, title_par)
          }

          rtf_doc <- officer::rtf_add(rtf_doc, .rebuild_ft_for_rtf(tbl))
        }
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
    .write_rtf_placeholder(file, paste0("结果导出失败：", e$message))
  })
}

#' Generate Analysis Code
#'
#' @param analysis_type Type of analysis
#' @param params Analysis parameters
#' @param data_name Name of the dataset used
#' @return Formatted R code string
#'
#' @noRd
generate_analysis_code <- function(analysis_type, params, data_name) {

  info <- .analysis_registry(analysis_type)
  function_name <- if (!is.null(info$function_name)) info$function_name else info$module

  # 清理参数
  clean_params <- remove_clear_params(params)

  # 构建参数字符串
  param_strings <- character()

  # 添加数据参数
  param_strings <- c(param_strings, paste0("inds = ", data_name))

  # 添加其他参数
  for (param_name in names(clean_params)) {
    param_value <- clean_params[[param_name]]

    if (is.character(param_value) && length(param_value) == 1) {
      # 字符串参数
      param_strings <- c(param_strings, paste0(param_name, " = '", param_value, "'"))
    } else if (is.numeric(param_value)) {
      # 数值参数
      param_strings <- c(param_strings, paste0(param_name, " = ", param_value))
    } else if (is.logical(param_value)) {
      # 逻辑值
      param_strings <- c(param_strings, paste0(param_name, " = ", as.character(param_value)))
    } else if (is.character(param_value) && length(param_value) > 1) {
      # 字符向量
      param_strings <- c(param_strings, paste0(param_name, " = c('", paste(param_value, collapse = "', '"), "')"))
    } else if (is.null(param_value)) {
      # 跳过NULL值
      next
    } else {
      # 其他类型转为字符串
      param_strings <- c(param_strings, paste0(param_name, " = '", as.character(param_value), "'"))
    }
  }

  # 构建完整的函数调用
  function_call <- paste0(function_name, "(", paste(param_strings, collapse = ", "), ")")

  # 添加注释和使用说明
  formatted_code <- paste(
    "# 统计分析代码 - 生成时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "# 分析类型:", get_analysis_name(analysis_type),
    "# 数据:", data_name,
    "",
    "# 使用说明:",
    "# 1. 请确保已加载所需的数据和包",
    "# 2. 将 'your_data' 替换为实际的数据变量名",
    "# 3. 复制以下代码到R环境中执行",
    "",
    "# 分析代码:",
    function_call,
    "",
    "# 注意: 此代码由小海统计助手生成",
    sep = "\n"
  )

  return(formatted_code)
}

#' Generate Batch Analysis Code
#'
#' @param selected_results Selected result IDs
#' @param results_list Results list
#' @return Combined R code for all selected analyses
#'
#' @noRd
generate_batch_analysis_code <- function(selected_results, results_list) {
  if (length(selected_results) == 0) {
    return("# 请先选择要生成代码的分析结果")
  }

  selected_results_list <- results_list[as.character(selected_results)]
  selected_results_list <- selected_results_list[order(as.integer(names(selected_results_list)))]

  code_sections <- character()

  # 添加文件头
  code_sections <- c(code_sections,
                     "# 批量分析代码 - 小海统计助手",
                     paste("# 生成时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                     paste("# 包含", length(selected_results), "个分析"),
                     "",
                     "# 使用说明:",
                     "# 1. 请确保已加载所需的数据和包",
                     "# 2. 将 'your_data' 替换为实际的数据变量名",
                     "# 3. 按顺序执行以下代码块",
                     ""
  )

  # 为每个分析生成代码
  for (i in seq_along(selected_results_list)) {
    result <- selected_results_list[[i]]

    code_sections <- c(code_sections,
                       paste("#", "=" %>% rep(50) %>% paste(collapse = "")),
                       paste("# 分析", i, ":", result$name),
                       paste("# 生成时间:", format(result$timestamp, "%Y-%m-%d %H:%M:%S")),
                       paste("# 数据类型:", result$data_name),
                       "",
                       result$code_call,
                       "",
                       "# 结果显示",
                       "print(result)",
                       ""
    )
  }

  # 添加文件尾
  code_sections <- c(code_sections,
                     "# 代码生成完成",
                     "# 感谢使用小海统计助手"
  )

  return(paste(code_sections, collapse = "\n"))
}

#' Download Selected Code
#'
#' @param selected_results Selected result IDs
#' @param results_list Results list
#' @param file Output file path
#'
#' @noRd
download_selected_code <- function(selected_results, results_list, file) {
  if (length(selected_results) == 0) {
    showNotification("请先选择要下载的结果", type = "warning")
    return()
  }

  tryCatch({
    # 生成批量代码
    batch_code <- generate_batch_analysis_code(selected_results, results_list)

    # 写入文件
    writeLines(batch_code, file, useBytes = TRUE)

    showNotification(paste("已成功下载", length(selected_results), "个分析的R代码"), type = "message")

  }, error = function(e) {
    message("代码下载错误: ", e$message)
    showNotification(paste("代码下载失败:", e$message), type = "error")
  })
}
