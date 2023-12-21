#' tabpanel "report" UI Function
#'
#' @description A shiny Module for a tab to generate and download report of results.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Report",
    shiny::titlePanel("Download Report"),

    # Sidebar layout with input and output definitions ----
    shiny::sidebarLayout(

     # Sidebar panel for inputs ----
     shiny::sidebarPanel(

       # Input: Choose dataset ----
       shiny::selectInput(NS(id,"dataset"), "Choose a format:",
                   choices = c("PDF", "HTML", "DOCX")),

       # Button
       shiny::downloadButton(NS(id,"downloadReport"), "Download")

     ),

     # Main panel for displaying outputs ----
     shiny::mainPanel(
       shiny::h1("This is the report"),
       shiny::textOutput(NS(id,"report_text"))
     )

    ),
    icon = shiny::icon("download")
    )

}

#' tabpanel "report" Server Functions
#'
#' @noRd
mod_tabpanel_report_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Download generated report
    output$report_text <- renderText({ paste(
      "Generated outputs for", path_vars[1], " ...")
    })
    output$downloadReport <- downloadHandler(filename = "report.pdf",
                                             content = NULL)
  })
}

