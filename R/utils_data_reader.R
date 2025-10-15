#' Data Reading Utilities
#'
#' @description Utility functions for reading various data file formats and loading example data
#'
#' Extract data frame from Rda file
#'
#' @noRd
#' @param file_path Path to the Rda file
#' @return Data frame extracted from the Rda file

extract_data_from_rda <- function(file_path) {
  # 创建一个新的环境来加载Rda文件
  env <- new.env()
  load(file_path, envir = env)

  # 获取环境中所有的对象
  objects <- ls(env)

  # 查找数据框对象
  data_frames <- objects[sapply(objects, function(x) is.data.frame(get(x, envir = env)))]

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

#' 自动检测CSV/TXT文件的分隔符
#'
#' @param file_path 文件路径
#' @param n_lines 用于检测的行数
#' @return 检测到的分隔符
#' @noRd
detect_delimiter <- function(file_path, n_lines = 10) {
  # 读取前几行进行检测
  con <- file(file_path, "r", encoding = "UTF-8")
  lines <- readLines(con, n = n_lines, warn = FALSE)
  close(con)

  if (length(lines) == 0) {
    return(",")  # 默认逗号分隔
  }

  # 统计各种分隔符的出现频率
  delimiters <- c(",", ";", "\t", "|")
  delimiter_counts <- sapply(delimiters, function(delim) {
    sum(sapply(lines, function(line) {
      # 排除引号内的内容，避免将引号内的分隔符计入
      line_clean <- gsub('"[^"]*"', '', line)  # 移除双引号内的内容
      line_clean <- gsub("'[^']*'", '', line_clean)  # 移除单引号内的内容
      lengths(regmatches(line_clean, gregexpr(delim, line_clean, fixed = TRUE)))
    }))
  })

  # 选择出现频率最高的分隔符
  if (max(delimiter_counts) > 0) {
    detected_delim <- delimiters[which.max(delimiter_counts)]
  } else {
    # 如果没有检测到常见分隔符，尝试检测空格
    space_count <- sum(sapply(lines, function(line) {
      lengths(regmatches(line, gregexpr("\\s+", line)))
    }))
    if (space_count > length(lines)) {
      detected_delim <- " "  # 空格分隔
    } else {
      detected_delim <- ","  # 默认逗号分隔
    }
  }

  message("检测到分隔符: '", detected_delim, "'")
  return(detected_delim)
}

#' 自动检测小数点和千位分隔符
#'
#' @param file_path 文件路径
#' @param delimiter 字段分隔符
#' @param n_lines 用于检测的行数
#' @return 列表包含decimal和grouping
#' @noRd
detect_decimal_separator <- function(file_path, delimiter, n_lines = 10) {
  con <- file(file_path, "r", encoding = "UTF-8")
  lines <- readLines(con, n = n_lines, warn = FALSE)
  close(con)

  if (length(lines) == 0) {
    return(list(decimal = ".", grouping = ","))
  }

  # 合并所有行进行分析
  all_text <- paste(lines, collapse = " ")

  # 统计点号和逗号在数字中的出现情况
  # 匹配数字模式：数字+分隔符+数字
  number_patterns <- c(
    "\\d+\\.\\d+",  # 点号作为小数点
    "\\d+,\\d+"    # 逗号作为小数点
  )

  dot_count <- sum(lengths(regmatches(all_text, gregexpr(number_patterns[1], all_text))))
  comma_count <- sum(lengths(regmatches(all_text, gregexpr(number_patterns[2], all_text))))

  # 决定小数点符号
  if (comma_count > dot_count * 2) {
    # 只有当逗号明显多于点号时才使用逗号作为小数点
    decimal <- ","
    grouping <- "."
  } else {
    decimal <- "."
    grouping <- ","
  }

  message("检测到小数点: '", decimal, "', 千位分隔符: '", grouping, "'")
  return(list(decimal = decimal, grouping = grouping))
}

#' Read data file based on file extension with auto-detection
#' @importFrom utils read.csv
#'
#' @param file_path Path to the file
#' @param file_name Name of the file
#' @param file_header Whether the file has header
#' @return Data frame containing the read data
#' @noRd
read_data_file <- function(file_path, file_name, file_header = TRUE) {
  file_ext <- tolower(tools::file_ext(file_name))

  if (file_ext %in% c("xlsx", "xls")) {
    # Excel文件
    df <- readxl::read_excel(file_path, col_names = file_header)

  } else if (file_ext %in% c("sas7bdat")) {
    # SAS文件
    df <- haven::read_sas(file_path)
    if (!is.data.frame(df)) {
      stop("读取的SAS文件没有返回有效的数据框")
    }

  } else if (file_ext %in% c("rda", "rdata")) {
    # R数据文件
    df <- extract_data_from_rda(file_path)

  } else if (file_ext %in% c("csv", "txt")) {
    # CSV/TXT文件 - 自动检测分隔符和数字格式
    delimiter <- detect_delimiter(file_path)
    decimal_info <- detect_decimal_separator(file_path, delimiter)

    message(sprintf("读取CSV/TXT文件 - 分隔符: '%s', 小数点: '%s', 表头: %s",
                    delimiter, decimal_info$decimal, file_header))

    # 🟢 修复：如果小数点是逗号，需要特殊处理
    if (decimal_info$decimal == ",") {
      # 使用 read.csv2 来处理逗号作为小数点的情况
      df <- read.csv2(file_path,
                      sep = delimiter,
                      header = file_header,
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8",
                      na.strings = c("", "NA", "NULL", "N/A"))
    } else {
      # 使用 read.csv 来处理点号作为小数点的情况
      df <- read.csv(file_path,
                     sep = delimiter,
                     dec = decimal_info$decimal,
                     header = file_header,
                     stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8",
                     na.strings = c("", "NA", "NULL", "N/A"))
    }

    # 添加数据框有效性检查
    if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
      stop("读取的CSV文件没有返回有效的数据框或数据为空")
    }

  } else {
    stop("请上传Excel文件(.xlsx, .xls)、SAS文件(.sas7bdat)、CSV文件(.csv, .txt)或R数据文件(.rda, .RData)")
  }

  # 如果用户选择不读取表头，设置默认列名
  if (!file_header && ncol(df) > 0) {
    names(df) <- paste0("V", 1:ncol(df))
  }

  return(df)
}

#' Get default data name based on analysis type
#'
#' @param analysis_type Type of analysis
#' @return Default data name
#' @noRd
get_default_data_name <- function(analysis_type) {
  data_names <- list(
    "q_describe" = "adsl",
    "c_describe" = "adsl",
    "c_srt" = "tyypspa",
    "q_param" = "cov_adur",
    "covancova" = "adts",
    "crosstable" = "adcrslb",
    "lifetest" = "adhj"
  )

  data_names[[analysis_type]] %||% "example_data"
}

#' Load example data for specific analysis type
#'
#' @param analysis_type Type of analysis
#' @return List containing data frame and metadata, or NULL if not found
#' @noRd
load_example_data <- function(analysis_type = NULL) {
  if (is.null(analysis_type)) {
    message("No analysis type specified")
    return(NULL)
  }

  # 获取默认数据名称
  data_name <- get_default_data_name(analysis_type)

  if (data_name == "example_data") {
    message("No default data defined for analysis type: ", analysis_type)
    return(NULL)
  }

  tryCatch({
    # 直接使用包名::数据名的方式加载数据
    df <- getExportedValue("BioStatsSuite", data_name)

    if (!is.data.frame(df)) {
      stop("Loaded object is not a data frame")
    }

    message("Example data loaded successfully: ", data_name)
    message("Dimensions: ", nrow(df), " x ", ncol(df))

    # 返回数据和元数据
    return(list(
      data = df,
      data_name = data_name,
      analysis_type = analysis_type,
      loaded_successfully = TRUE
    ))

  }, error = function(e) {
    message("Error loading example data: ", e$message)
    return(list(
      data = NULL,
      error_message = e$message,
      loaded_successfully = FALSE
    ))
  })
}



#' Get data file type
#'
#' @param file_name File name
#' @return File extension in lowercase
#' @noRd
get_file_type <- function(file_name) {
  tolower(tools::file_ext(file_name))
}

#' Extract data name from file name
#'
#' @param file_name File name
#' @return Base name without extension
#' @noRd
get_data_name <- function(file_name) {
  tools::file_path_sans_ext(file_name)
}

#' Helper function for NULL coalescing
#'
#' @param x Value to check
#' @param y Default value
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
