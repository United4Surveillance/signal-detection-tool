#' tabpanel "help" UI Function
#'
#' @description A shiny Module for a tab panel that explains how to use the shiny app.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_help_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Help",
    shiny::fluidPage(
      style = "padding-top: 30px;",
      shiny::uiOutput(ns("help_html"))
    ),
    icon = shiny::icon("question")
  )
}

#' tabpanel "help" Server Functions
#'
#' @noRd
mod_tabpanel_help_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    html_path <- system.file("rmd/help_tab.html", package = "SignalDetectionTool")

    output$help_html <- shiny::renderUI({
      shiny::includeHTML(html_path)
    })
  })
}
