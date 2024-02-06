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

    shiny::uiOutput(ns("report_tab_ui")),

    icon = shiny::icon("download")
  )

}


#' tabpanel "report" Server Functions
#'
#' @noRd
mod_tabpanel_report_server <- function(id,
                                       indata,
                                       strat_vars,
                                       pathogen_vars,
                                       method,
                                       errors_detected,
                                       no_algorithm_possible) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## UI-portion of the tab below!
    # ensuring that content is onlyu shown if data check returns no errors
    output$report_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(shiny::tagList(
          shiny::br(),
          shiny::h2("Data Format Check Failed"),
          shiny::p("Unfortunately, the selected data does not meet the required format."),
          shiny::p("Please make sure the data follows the correct structure and try again."),
          shiny::br(),
          shiny::hr(),
          shiny::p("You can check the data in the 'Data' tab for more details on the issue.")
        ))
      } else if (no_algorithm_possible() == TRUE){
        return(shiny::tagList(
          shiny::br(),
          shiny::h3("There is no algorithm which can be applied to your current settings, please change your selected settings in the input tab and try again."),
          shiny::br()
        ))
      } else {
        return(shiny::tagList(
          shiny::titlePanel("Download Report"),

          # Sidebar layout with input and output definitions ----
          shiny::sidebarLayout(

            # Sidebar panel for inputs ----
            shiny::sidebarPanel(

              # Input: Choose dataset ----
              shiny::selectInput(NS(id, "format"), "Choose a format:",
                                 choices = c("HTML", "DOCX", "PDF")),

              shiny::checkboxInput(NS(id, "interactive"),
                                   "Interactive HTML",
                                   value = TRUE),

              shiny::checkboxInput(NS(id, "tables"),
                                   "Include tables (stratifications)",
                                   value = TRUE),

              # Button
              shiny::downloadButton(NS(id, "downloadReport"), "Create Report")

            ),

            # Main panel for displaying outputs ----
            shiny::mainPanel(
              shiny::h1("Signal detection report"),
              shiny::p(paste("Be aware that the generation of the report can take",
                             "several minutes.")),
              shiny::textOutput(NS(id, "report_text"))
            )
          )
        ))
      }
    })

    # Download generated report
    output$report_text <- renderText({
      paste("Generated outputs for", pathogen_vars(), " stratified ",
            "by ", paste0(strat_vars(), collapse = ", "), "using ", names(available_algorithms())[available_algorithms() == method()], " as outbreak detection algorithm.")
    })


    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("SignalDetectionReport.",
               switch(input$format, HTML = "html", DOCX = "docx", PDF = "pdf"))
      },
      content = function(con) {
        run_report(report_format = input$format,
                   data = indata(),
                   strata = strat_vars(),
                   algo = method(),
                   interactive = input$interactive,
                   tables = input$tables,
                   # training_range, #TODO
                   # analysis_range, #TODO
                   output_file = con)
      })
  })
}

