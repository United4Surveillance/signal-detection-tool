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
        column(12,
          shiny::uiOutput(ns("help_markdown")))
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

    output$help_markdown <- shiny::renderUI({
      shiny::HTML(
        markdown::mark_html(
          knitr::knit("inst/rmd/help_tab.Rmd", quiet = TRUE,
                      encoding = "UTF-8", output = "inst/rmd/help_tab.md"),
          output = NULL, options = list(number_sections = FALSE, toc = TRUE,
                                        toc_float = list(collapsed = TRUE)),
          template = FALSE
        ))
    })

  })
}

