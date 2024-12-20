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
    title = "Help",
    icon = shiny::icon("question"),
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        bslib::card(
          shiny::h1("Table of Contents"),
          shiny::uiOutput(ns("help_tab_html"))
        )
      ),
      footer_text
    )
  )
}

#' tabpanel "help" Server Functions
#'
#' @noRd
mod_tabpanel_help_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rmd_path <- system.file("rmd/help_tab.Rmd", package = "SignalDetectionTool")
    html_path_temp <- tempfile(fileext = ".html")
    # somehow when including the finished rendered html instead of rerendering each time the app starts does not work and then the data tab does not show the output
    knitr::knit(
      input = rmd_path,
      output = html_path_temp,
      quiet = TRUE,
      encoding = "UTF-8"
    )

    html_content <- markdown::mark_html(
      html_path_temp,
      output = NULL,
      options = list(
        toc = TRUE,
        number_sections = FALSE
      ),
      template = FALSE
    )

    output$help_tab_html <- shiny::renderUI({
      shiny::HTML(html_content)
    })
  })
}
