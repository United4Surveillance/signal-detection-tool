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
      column(
        12,
        shiny::br(),
        shiny::h3("Table of content"),
        shiny::uiOutput(ns("help_markdown"))
      )
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

    rmd_path <- system.file("rmd/help_tab.Rmd", package = "SignalDetectionTool")
    # Knit the R Markdown file to a temporary Markdown file
    md_path_temp <- tempfile(fileext = ".md")
    knitr::knit(input = rmd_path, output = md_path_temp, quiet = TRUE, encoding = "UTF-8")

    # Convert the Markdown file to HTML
    html_content <- markdown::mark_html(md_path_temp, output = NULL, options = list(toc = TRUE, number_sections = FALSE), template = FALSE)

    output$help_markdown <- shiny::renderUI({
      shiny::HTML(html_content)
    })
  })
}
