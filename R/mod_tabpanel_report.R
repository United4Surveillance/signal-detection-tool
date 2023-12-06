

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
        shiny::selectInput(NS(id, "format"), "Choose a format:",
                           choices = c("HTML", "DOCX", "PDF")),

        shiny::checkboxInput(NS(id, "interactive"),
                             "Interactive HTML",
                             value = TRUE),

        shiny::checkboxInput(NS(id, "tables"),
                             "Include tables (stratifications)",
                             value = TRUE),

        # Button
        shiny::downloadButton(NS(id, "downloadReport"), "Download")

      ),

      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::h1("This is the report"),
        shiny::textOutput(NS(id, "report_text"))
      )

    ),
    icon = shiny::icon("download")
  )

}


mod_tabpanel_report_server <- function(id,
                                       indata,
                                       strat_vars,
                                       pathogen_vars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Download generated report
    output$report_text <- renderText({
      paste("Generated outputs for", pathogen_vars(), " stratified ",
            "by ", paste0(strat_vars(), collapse = ", "))
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
                   interactive = input$interactive,
                   tables = input$tables,
                   # training_range, #TODO
                   # analysis_range, #TODO
                   output_file = con)
      })
  })
}

