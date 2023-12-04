#' tabpanel "help" UI Function
#'
#' @description A shiny Module for a tab panel that explains how to use the shiny app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_help_ui <- function(id) {
  ns <- shiny::NS(id)

    shiny::tabPanel(
      "Help",
      shiny::h1("HowTo Use This App"),
      shiny::p("Follow the data flow in the tabs - from Data input to the Report on signal detections."),
      shiny::br(),
      shiny::p("Descriptions of required inputs etc."),
      icon = shiny::icon("question")
      )

}

#' tabpanel "help" Server Functions
#'
#' @noRd
mod_tabpanel_help_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

