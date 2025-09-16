#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  data_upload <- mod_dataUpload_server("dataUpload_1")
  analyze_result <- mod_analyze_server("analyze_1", data_upload)

}
