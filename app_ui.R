#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      title =
              div(id="logo-id","Drug Signal Detection",
                           img(src="www/app_ICON.png", style="float:left; padding-right:3px; height:50px; width:60px")),
               id = "navbarID",
               windowTitle = "AE Detection",
               tabPanel(
                 title = "Data"
                 ,mod_dataUpload_ui("dataUpload_ui_1")
               ),
               tabPanel(
                 title = "Analyze",
                 mod_tableGen_ui("tableGen_ui_1")
               ),
               tabPanel(
                 title = "MedDRA Translate Mapping"
                 ,mod_translate_ui("translate_ui_1")
               ),
               tabPanel(
                 title = "Plot"
                 , mod_Plot_ui("Plot_ui_1")
               )

    )
    ,tags$script(
      HTML("var header = $('.navbar > .container-fluid');
                                header.append('<img src=\"www/laboratory.png\" style=\"width:4%;height:4%;float:right;padding-top:5px;\"></a>')")
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
      app_title = "DrugAESD"
    ),
    # Add here other external resources
    tags$script(HTML(htmljs)),
    tags$script(src = "https://cdn.datatables.net/2.0.2/js/dataTables.min.js"),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyjs::inlineCSS(css),
    cicerone::use_cicerone()

  )
}
