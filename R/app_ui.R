#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    fluidPage(
      titlePanel("BioStatsSuite - 统计分析工具套件"),

      sidebarLayout(
        sidebarPanel(
          mod_dataUpload_sidebar_ui("dataUpload_1"),
          mod_analyze_sidebar_ui("analyze_1"),
          width = 3
        ),

        mainPanel(
          tabsetPanel(
            mod_dataUpload_tabPanel_ui("dataUpload_1"),
            mod_analyze_tabPanel_ui("analyze_1")
          ),
          width = 9
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BioStatsSuite"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
