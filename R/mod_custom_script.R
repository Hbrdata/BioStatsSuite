#' custom_script UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_custom_script_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #fafafa;",

      # 模块标题
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #9b59b6;",
        icon("code", style = "color: #9b59b6; margin-right: 10px; font-size: 18px;"),
        h5("自定义R脚本分析", style = "margin: 0; color: #2c3e50; font-weight: 600;")
      ),

      # 文件上传区域
      tags$div(
        style = "margin-bottom: 15px;",
        fileInput(ns("script_file"), "上传R脚本文件",
                  accept = c(".R", ".r"),
                  buttonLabel = "选择R脚本...",
                  placeholder = "支持 .R 文件")
      ),

      # 脚本编辑区域
      tags$div(
        style = "margin-bottom: 15px;",
        tags$label("或直接编辑脚本:"),
        tags$div(
          style = "border: 1px solid #ddd; border-radius: 4px; margin-top: 5px;",
          shinyAce::aceEditor(ns("script_editor"),
                              mode = "r",
                              height = "300px",
                              value = "# 在这里编写您的R脚本\n# 可以使用以下变量：\n# - data: 当前数据集\n# - 必须返回一个flextable对象或包含flextable的列表\n\n# 示例：简单的描述性统计\nif (exists('data') && !is.null(data)) {\n  # 选择数值列\n  numeric_cols <- sapply(data, is.numeric)\n  if (sum(numeric_cols) > 0) {\n    result <- psych::describe(data[, numeric_cols, drop = FALSE])\n    \n    # 转换为flextable\n    ft <- flextable::flextable(result)\n    ft <- flextable::set_caption(ft, \"自定义分析结果\")\n    ft <- flextable::autofit(ft)\n    \n    # 返回结果\n    return(ft)\n  } else {\n    stop(\"没有找到数值列进行分析\")\n  }\n} else {\n  stop(\"没有可用的数据\")\n}")
        )
      ),

      # 参数设置区域
      tags$div(
        style = "background-color: #e8f4f8; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
        tags$div(
          style = "display: flex; align-items: flex-start;",
          icon("info-circle", style = "color: #3498db; margin-right: 8px; margin-top: 2px;"),
          tags$div(
            style = "flex: 1;",
            tags$small("脚本要求：", style = "color: #2c3e50; font-weight: bold;"),
            tags$br(),
            tags$small("• 可以使用 'data' 变量访问当前数据集", style = "color: #2c3e50;"),
            tags$br(),
            tags$small("• 必须返回 flextable 对象或包含 flextable 的列表", style = "color: #2c3e50;"),
            tags$br(),
            tags$small("• 支持基本的R语法和统计分析", style = "color: #2c3e50;")
          )
        )
      )
    ),

    # 操作按钮
    tags$div(
      style = "display: flex; gap: 10px; justify-content: space-between;",
      actionButton(ns("run_script"), "运行脚本",
                   icon = icon("play"),
                   style = "background-color: #27ae60; color: white; flex: 1;"),
      actionButton(ns("clear_script"), "清空脚本",
                   icon = icon("broom"),
                   style = "background-color: #e74c3c; color: white; flex: 1;"),
      actionButton(ns("load_example"), "加载示例",
                   icon = icon("lightbulb"),
                   style = "background-color: #f39c12; color: white; flex: 1;")
    )
  )
}


#' custom_script Server Functions
#'
#' @noRd
mod_custom_script_server <- function(id, data_upload_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 响应式值存储脚本内容
    rv <- reactiveValues(
      script_content = "",
      last_uploaded_file = NULL
    )

    # 监听文件上传
    observeEvent(input$script_file, {
      req(input$script_file)

      tryCatch({
        # 读取脚本文件内容
        script_content <- readLines(input$script_file$datapath, warn = FALSE)
        script_content <- paste(script_content, collapse = "\n")

        # 更新Ace编辑器
        shinyAce::updateAceEditor(session, "script_editor", value = script_content)

        rv$script_content <- script_content
        rv$last_uploaded_file <- input$script_file$name

        showNotification("脚本文件加载成功!", type = "message")

      }, error = function(e) {
        showNotification(paste("脚本文件读取错误:", e$message), type = "error")
      })
    })

    # 监听编辑器内容变化
    observe({
      rv$script_content <- input$script_editor
    })

    # 加载示例脚本
    observeEvent(input$load_example, {
      example_script <- '# 自定义分析示例：分组描述性统计
# 这个示例展示如何对数据进行分组统计

if (exists(\'data\') && !is.null(data)) {
  # 检查数据
  if (ncol(data) < 2) {
    stop(\"数据需要至少包含一个分组变量和一个数值变量\")
  }

  # 自动识别分组变量（字符或因子）和数值变量
  group_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
  numeric_vars <- names(data)[sapply(data, is.numeric)]

  if (length(group_vars) == 0 | length(numeric_vars) == 0) {
    stop(\"需要至少一个分组变量和一个数值变量\")
  }

  # 使用第一个分组变量和第一个数值变量
  group_var <- group_vars[1]
  numeric_var <- numeric_vars[1]

  # 分组统计
  library(dplyr)
  result <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      观测数 = n(),
      均值 = round(mean(!!sym(numeric_var), 2),
      标准差 = round(sd(!!sym(numeric_var), 2),
      中位数 = round(median(!!sym(numeric_var), 2),
      最小值 = round(min(!!sym(numeric_var), 2),
      最大值 = round(max(!!sym(numeric_var), 2)
    ) %>%
    as.data.frame()

  # 创建flextable
  ft <- flextable::flextable(result)
  ft <- flextable::set_caption(ft, paste(\"分组描述性统计:\", numeric_var, \"by\", group_var))
  ft <- flextable::add_header_row(ft, values = c(\"分组变量\", \"统计量\"), colwidths = c(1, 5))
  ft <- flextable::theme_zebra(ft)
  ft <- flextable::autofit(ft)

  return(ft)

} else {
  stop(\"没有可用的数据\")
}'

      shinyAce::updateAceEditor(session, "script_editor", value = example_script)
      showNotification("示例脚本已加载!", type = "message")
    })

    # 清空脚本
    observeEvent(input$clear_script, {
      shinyAce::updateAceEditor(session, "script_editor", value = "")
      showNotification("脚本已清空!", type = "message")
    })

    # 运行脚本
    observeEvent(input$run_script, {
      req(rv$script_content)
      req(data_upload_module())
      req(data_upload_module()$current_data)

      tryCatch({
        # 获取当前数据
        current_data <- data_upload_module()$current_data

        # 创建安全的执行环境
        env <- new.env()
        env$data <- current_data

        # 执行脚本
        result <- eval(parse(text = rv$script_content), envir = env)

        # 验证结果类型
        if (!inherits(result, c("flextable", "list"))) {
          stop("脚本必须返回 flextable 对象或包含 flextable 的列表")
        }

        # 返回结果
        return(list(
          flextable = result,
          script_content = rv$script_content,
          file_name = rv$last_uploaded_file
        ))

      }, error = function(e) {
        showNotification(paste("脚本执行错误:", e$message), type = "error")
        return(NULL)
      })
    })

    # 清空参数方法（供外部调用）
    clear_params <- function() {
      shinyAce::updateAceEditor(session, "script_editor", value = "")
      rv$script_content <- ""
      rv$last_uploaded_file <- NULL
    }

    # 返回模块接口
    return(reactive({
      list(
        run_script = input$run_script,
        script_content = rv$script_content,
        clear_params = clear_params
      )
    }))
  })
}

## To be copied in the UI
# mod_custom_script_ui("custom_script_1")

## To be copied in the server
# mod_custom_script_server("custom_script_1")
