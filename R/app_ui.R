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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SignalDetectionTool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}
