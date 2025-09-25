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
#' @noRd
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
