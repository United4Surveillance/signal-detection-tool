#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Leave this function for adding external resources
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_fluid(
      shinyjs::useShinyjs(),
      # Application title
      shiny::div(
        id = "title-panel",
        class = "title-panel",
        shiny::div(
          class = "title-text",
          "Signal Detection Tool"
        ),
        shiny::div(
          class = "logo-container",
          shiny::tags$img(src = "www/U4S - BLUE - Transparent_cropped.png", width = "235px"),
          shiny::tags$img(src = "www/EN Co-Funded by the EU_NEG_resized.png", width = "235px")
        )
      ),

      # Content
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
  shiny::tags$head(
    golem::favicon(
      ico = "https://united4surveillance.eu/wp-content/uploads/2023/03/FAV-150x107.png",
      ext = "png"
    ),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "SignalDetectionTool"
    )
  )
}
