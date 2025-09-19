#' data_reader
#'
#' @description 统一处理各种格式的数据文件读取
#'
#' @param file_path 文件路径
#' @param file_name 文件名（用于确定文件类型）
#' @param csv_separator CSV分隔符（可选）
#' @param csv_decimal CSV小数点（可选）
#' @param csv_header CSV是否包含表头（可选）
#'
#' @return 数据框对象
#' @export
#'
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom haven read_sas
#'
#' @examples
#' \dontrun{
#' # 读取Excel文件
#' df <- read_data_file("data.xlsx", "data.xlsx")
#'
#' # 读取CSV文件
#' df <- read_data_file("data.csv", "data.csv", ",", ".", TRUE)
#' }
read_data_file <- function(file_path, file_name,
                           csv_separator = ",",
                           csv_decimal = ".",
                           csv_header = TRUE) {

  # 验证文件存在性
  if (!file.exists(file_path)) {
    stop(paste("文件不存在:", file_path))
  }

  # 获取文件扩展名
  file_ext <- tolower(tools::file_ext(file_name))

  tryCatch({
    # 根据文件类型选择读取方式
    df <- switch(file_ext,
                 "xlsx" = ,
                 "xls" = {
                   if (!requireNamespace("readxl", quietly = TRUE)) {
                     stop("请安装readxl包来读取Excel文件: install.packages('readxl')")
                   }
                   readxl::read_excel(file_path)
                 },
                 "sas7bdat" = {
                   if (!requireNamespace("haven", quietly = TRUE)) {
                     stop("请安装haven包来读取SAS文件: install.packages('haven')")
                   }
                   df_sas <- haven::read_sas(file_path)
                   if (!is.data.frame(df_sas)) {
                     stop("读取的SAS文件没有返回有效的数据框")
                   }
                   df_sas
                 },
                 "rda" = ,
                 "rdata" = {
                   # 从R数据文件中提取数据框
                   extract_data_from_rda(file_path)
                 },
                 "csv" = ,
                 "txt" = {
                   # 读取CSV文件
                   df_csv <- read.csv(file_path,
                                      sep = csv_separator,
                                      dec = csv_decimal,
                                      header = csv_header,
                                      stringsAsFactors = FALSE,
                                      fileEncoding = "UTF-8")
                   # 验证数据框
                   if (!is.data.frame(df_csv) || nrow(df_csv) == 0 || ncol(df_csv) == 0) {
                     stop("读取的CSV文件没有返回有效的数据框或数据为空")
                   }
                   df_csv
                 },
                 {
                   stop(sprintf("不支持的文件格式: %s。请上传Excel、SAS、CSV或R数据文件", file_ext))
                 }
    )

    # 最终验证
    if (!is.data.frame(df)) {
      stop("读取的文件没有返回有效的数据框")
    }

    if (nrow(df) == 0 || ncol(df) == 0) {
      stop("读取的数据框为空")
    }

    message(sprintf("成功读取文件: %s (%d行 x %d列)",
                    file_name, nrow(df), ncol(df)))

    return(df)

  }, error = function(e) {
    stop(paste("读取文件错误:", e$message))
  })
}

#' 从Rda文件中提取数据框
#'
#' @description 从.Rda或.RData文件中提取第一个数据框对象
#'
#' @param file_path Rda文件路径
#'
#' @return 数据框对象
#' @export
#'
#' @examples
#' \dontrun{
#' df <- extract_data_from_rda("data.Rda")
#' }
extract_data_from_rda <- function(file_path) {
  # 创建新环境加载数据
  env <- new.env()
  load(file_path, envir = env)

  # 获取环境中所有的对象
  objects <- ls(env)

  # 查找数据框对象
  data_frames <- objects[sapply(objects, function(x) {
    obj <- get(x, envir = env)
    is.data.frame(obj) || tibble::is_tibble(obj)
  })]

  if (length(data_frames) == 0) {
    stop("Rda文件中没有找到数据框对象")
  }

  # 如果有多个数据框，选择第一个
  if (length(data_frames) > 1) {
    warning(sprintf("Rda文件中包含多个数据框，选择了第一个: %s", data_frames[1]))
  }

  # 返回第一个数据框
  return(get(data_frames[1], envir = env))
}

#' 加载示例数据
#'
#' @description 从预设路径加载示例数据文件
#'
#' @param session Shiny session对象（可选）
#' @param update_data_name 是否更新数据名称输入框（可选）
#' @param data_name 数据名称（可选，默认为"adsl"）
#'
#' @return 数据框对象，如果加载失败则返回NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # 在Shiny模块中使用
#' adsl_data <- load_example_data(session)
#'
#' # 在普通R环境中使用
#' adsl_data <- load_example_data()
#' }
load_example_data <- function(session = NULL, update_data_name = TRUE, data_name = "adsl") {
  tryCatch({
    # 尝试多种路径查找示例数据
    example_paths <- c(
      "data/adsl.Rda",
      system.file("data", "adsl.Rda", package = "BioStatsSuite")
    )

    found_path <- NULL
    for (path in example_paths) {
      if (file.exists(path)) {
        found_path <- path
        break
      }
    }

    if (is.null(found_path)) {
      warning("示例数据文件未找到，请检查路径。查找的路径: ", paste(example_paths, collapse = ", "))
      return(NULL)
    }

    message("Loading example data from: ", found_path)

    # 使用统一的函数读取数据
    df <- read_data_file(found_path, basename(found_path))

    # 更新数据名称（如果在Shiny环境中）
    if (!is.null(session) && update_data_name) {
      updateTextInput(session, "data_name", value = data_name)
    }

    # 打印调试信息
    message("Example data loaded successfully")
    message("Dimensions: ", nrow(df), " x ", ncol(df))
    message("Column names: ", paste(names(df), collapse = ", "))

    return(df)

  }, error = function(e) {
    error_msg <- paste("加载示例数据错误:", e$message)
    if (!is.null(session)) {
      showNotification(error_msg, type = "error")
    }
    warning(error_msg)
    return(NULL)
  })
}

#' 检查示例数据是否可用
#'
#' @description 检查示例数据文件是否存在
#'
#' @return 逻辑值，TRUE表示示例数据可用
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (example_data_available()) {
#'   message("Example data is available")
#' }
#' }
example_data_available <- function() {
  example_paths <- c(
    "data/adsl.Rda",
    system.file("data", "adsl.Rda", package = "BioStatsSuite")
  )

  any(file.exists(example_paths))
}



#' 获取支持的文件格式信息
#'
#' @description 返回应用支持的文件格式信息
#'
#' @return 包含支持格式信息的列表
#' @export
#'
#' @examples
#' \dontrun{
#' formats <- get_supported_formats()
#' }
get_supported_formats <- function() {
  list(
    extensions = c(".xlsx", ".xls", ".sas7bdat", ".rda", ".RData", ".csv", ".txt"),
    descriptions = c(
      "Excel文件 (.xlsx, .xls)",
      "SAS数据文件 (.sas7bdat)",
      "R数据文件 (.rda, .RData)",
      "CSV文本文件 (.csv, .txt)"
    ),
    max_size = 100 * 1024 * 1024  # 100MB
  )
}

#' 验证数据框有效性
#'
#' @description 检查数据框是否有效（非空且有数据）
#'
#' @param df 数据框对象
#' @param df_name 数据框名称（用于错误消息）
#'
#' @return 逻辑值，TRUE表示有效
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_dataframe(df, "我的数据")
#' }
validate_dataframe <- function(df, df_name = "数据框") {
  if (is.null(df)) {
    stop(paste(df_name, "不能为NULL"))
  }

  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    stop(paste(df_name, "必须是数据框或tibble"))
  }

  if (nrow(df) == 0) {
    stop(paste(df_name, "没有行数据"))
  }

  if (ncol(df) == 0) {
    stop(paste(df_name, "没有列数据"))
  }

  return(TRUE)
}
