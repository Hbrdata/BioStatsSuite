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

#' Read data file based on file extension
#'
#' @param file_path Path to the file
#' @param file_name Name of the file
#' @param csv_separator CSV separator character
#' @param csv_decimal CSV decimal character
#' @param csv_header Whether CSV has header
#' @return Data frame containing the read data
#' @export
read_data_file <- function(file_path, file_name, csv_separator = ",", csv_decimal = ".", csv_header = TRUE) {
  file_ext <- tolower(tools::file_ext(file_name))

  if (file_ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file_path)
  } else if (file_ext %in% c("sas7bdat")) {
    df <- haven::read_sas(file_path)
    if (!is.data.frame(df)) {
      stop("读取的SAS文件没有返回有效的数据框")
    }
  } else if (file_ext %in% c("rda", "rdata")) {
    df <- extract_data_from_rda(file_path)
  } else if (file_ext %in% c("csv", "txt")) {
    # 读取CSV文件
    df <- read.csv(file_path,
                   sep = csv_separator,
                   dec = csv_decimal,
                   header = csv_header,
                   stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8")
    # 添加数据框有效性检查
    if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
      stop("读取的CSV文件没有返回有效的数据框或数据为空")
    }
  } else {
    stop("请上传Excel文件(.xlsx, .xls)、SAS文件(.sas7bdat)、CSV文件(.csv, .txt)或R数据文件(.rda, .RData)")
  }

  return(df)
}

#' Get example data file path based on analysis type
#'
#' @param analysis_type Type of analysis
#' @return Path to the example data file
#' @export
get_example_data_path <- function(analysis_type) {
  example_files <- list(
    "q_describe" = "data/adsl.Rda",
    "c_describe" = "data/adsl.Rda",
    "c_srt" = "data/tyypspa.Rda",
    "q_param" = "data/cov_adur.Rda",
    "covancova" = "data/adts.Rda",
    "crosstable" = "data/adcrslb.Rda",
    "lifetest" = "data/adhj.Rda"
  )

  file_name <- example_files[[analysis_type]]

  if (is.null(file_name)) {
    warning(paste("No example data defined for analysis type:", analysis_type))
    return(NULL)
  }

  # 检查文件是否存在
  if (file.exists(file_name)) {
    return(file_name)
  }

  # 如果相对路径不存在，尝试系统文件路径
  sys_file <- system.file("data", basename(file_name), package = "BioStatsSuite")
  if (file.exists(sys_file)) {
    return(sys_file)
  }

  warning(paste("Example data file not found:", file_name))
  return(NULL)
}

#' Get default data name based on analysis type
#'
#' @param analysis_type Type of analysis
#' @return Default data name
#' @export
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
#' @param custom_path Optional custom path to example data file
#' @return List containing data frame and metadata, or NULL if not found
#' @export
load_example_data <- function(analysis_type = NULL, custom_path = NULL) {
  # 确定要加载的文件路径
  if (!is.null(custom_path) && file.exists(custom_path)) {
    file_path <- custom_path
  } else if (!is.null(analysis_type)) {
    file_path <- get_example_data_path(analysis_type)
    if (is.null(file_path)) {
      message("No example data path found for analysis type: ", analysis_type)
      return(NULL)
    }
  } else {
    # 默认示例数据
    default_paths <- c("data/adsl.Rda", system.file("data", "adsl.Rda", package = "BioStatsSuite"))
    file_path <- NULL
    for (path in default_paths) {
      if (file.exists(path)) {
        file_path <- path
        break
      }
    }
    if (is.null(file_path)) {
      message("Default example data not found")
      return(NULL)
    }
  }

  message("Loading example data from: ", file_path)

  tryCatch({
    # 创建新环境加载数据
    env <- new.env()
    load(file_path, envir = env)

    # 获取环境中的数据框
    data_objects <- ls(env)
    data_frames <- data_objects[sapply(data_objects, function(x) is.data.frame(get(x, envir = env)))]

    if (length(data_frames) == 0) {
      stop("示例数据文件中没有找到数据框")
    }

    df <- get(data_frames[1], envir = env)

    # 获取默认数据名称
    data_name <- if (!is.null(analysis_type)) {
      get_default_data_name(analysis_type)
    } else {
      tools::file_path_sans_ext(basename(file_path))
    }

    # 返回数据和元数据
    return(list(
      data = df,
      data_name = data_name,
      file_type = "rda",
      file_path = file_path,
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
#' @export
get_file_type <- function(file_name) {
  tolower(tools::file_ext(file_name))
}

#' Extract data name from file name
#'
#' @param file_name File name
#' @return Base name without extension
#' @export
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
