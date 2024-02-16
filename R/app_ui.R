#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      # Application title
      ## Logo and headertext
      shiny::titlePanel( shiny::div(
        shiny::column(width = 10, shiny::h1("United4Surveillance Signal Detection Tool")),
        shiny::column(width = 2, shiny::tags$img(src = "www/U4S-BLUE-200x87.png"))),
        windowTitle="U4S Signal Detection"
      ),

      shiny::tabsetPanel(
        mod_tabpanel_help_ui("help"),
        mod_tabpanel_data_ui("data"),
        mod_tabpanel_input_ui("input"),
        mod_tabpanel_signals_ui("signals"),
        mod_tabpanel_report_ui("report"),
        selected = "Data"
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
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(
      ico = "https://united4surveillance.eu/wp-content/uploads/2023/03/FAV-150x107.png",
      ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SignalDetectionTool"
    ),
    tags$style(HTML("
      .value-box {
        border-radius: 3px;
        border: 1px solid #ddd;
        background-color: #f4f4f4;
        padding: 10px;
        text-align: center;
      }
      .value-box .title {
        font-size: 16px;
        color: white;
      }
      .value-box .value {
        font-size: 24px;
        font-weight: bold;
        color: white;
      }
      .value-box .icon {
        font-size: 32px;
        margin-bottom: 10px;
      }
      .value-box.blue {
        border-color: #2297E6;
        background-color: #2297E6;
      }
      .value-box.red {
        border-color: #DF536B;
        background-color: #DF536B;
      }"
      )
      )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
