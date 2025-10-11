#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    navbarPage(
      title = div(
        id = "logo-id",
        "小海 - 统计分析工具套件"
        # ,img(
        #   src = "www/app_ICON.png",
        #   style = "float:left; padding-right:3px; height:25px; width:30px"
        # )
      ),
      id = "navbarID",
      windowTitle = "BioStatsSuite",

      # 数据上传和管理标签页
      tabPanel(
        title = "数据管理",
        icon = icon("database"),
        fluidRow(
          column(
            width = 3,
            mod_dataUpload_sidebar_ui("dataUpload_1")
          ),
          column(
            width = 9,
            mod_dataUpload_tabPanel_ui("dataUpload_1")
          )
        )
      ),

      # 统计分析标签页
      tabPanel(
        title = "统计分析",
        icon = icon("chart-bar"),
        fluidRow(
          column(
            width = 3,
            mod_analyze_sidebar_ui("analyze_1")
          ),
          column(
            width = 9,
            mod_analyze_tabPanel_ui("analyze_1")
          )
        )
      )
    ),
    tags$script(
      HTML("var header = $('.navbar > .container-fluid');
                                header.append('<img src=\"www/HBRDATA.png\" style=\"width:4.5%;height:2.5%;float:right;padding-top:5px;\"></a>')")
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
    ),
    # 添加自定义 CSS
    tags$style(HTML("
      .gradient-bg-img {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 8px;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      .gradient-bg-img img {
        display: block;
        width: 100%;
        height: auto;
      }
    "))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
