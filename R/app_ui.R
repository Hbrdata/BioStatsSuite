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
      title = tags$div(
        style = "color: white !important; font-weight: 700 !important;
                 font-size: 22px !important;
                 text-shadow: 0 1px 3px rgba(0, 0, 0, 0.5) !important;",
        "小海 - 统计助手"
      ),
      # title = div(
      #   id = "logo-id",
      #   "小海 - 统计助手"
      #   # ,img(
      #   #   src = "www/app_ICON.png",
      #   #   style = "float:left; padding-right:3px; height:25px; width:30px"
      #   # )
      # ),
      id = "navbarID",
      windowTitle = "BioStatsSuite",

      # 导航栏样式设置
      header = tags$style(HTML("
        .navbar {
          background-color: #2573BA !important;
          border: none !important;
          min-height: 70px !important;
          padding-top: 8px !important;
          padding-bottom: 8px !important;
        }
      ")),

      # 数据上传和管理标签页
      tabPanel(
        title = tags$span(
          style = "font-size: 18px;",
          icon("database"),
          "数据管理"
        ),
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
        title = tags$span(
          style = "font-size: 18px;",
          icon("chart-bar"),
          "统计分析"
        ),
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
    )



    # tags$script(
    #   HTML("var header = $('.navbar > .container-fluid');
    #     header.prepend('<div style=\"float:left; margin:5px 15px 0 0;\"><img src=\"www/HBRDATA.png\" style=\"width:45px;height:25px;\"></div>')")
    # )
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


    "))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

  )
}
