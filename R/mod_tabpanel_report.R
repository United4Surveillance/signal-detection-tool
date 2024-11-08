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
    title = "Report",
    icon = shiny::icon("download"),
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::uiOutput(ns("report_tab_ui"))
  )
}


#' tabpanel "report" Server Functions
#'
#' @noRd
mod_tabpanel_report_server <- function(id,
                                       filtered_data,
                                       strat_vars,
                                       pathogen_vars,
                                       errors_detected,
                                       no_algorithm_possible,
                                       number_of_weeks_input_valid,
                                       signals_padded,
                                       signals_agg,
                                       intervention_date) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## UI-portion of the tab below!
    # ensuring that content is onlyu shown if data check returns no errors
    output$report_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else if (!number_of_weeks_input_valid()) {
        return(nweeks_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          shiny::titlePanel("Download Report"),

          # Sidebar layout with input and output definitions ----
          shiny::sidebarLayout(

            # Sidebar panel for inputs ----
            shiny::sidebarPanel(

              # Input: Choose dataset ----
              shiny::selectInput(NS(id, "format"), "Choose a format:",
                choices = c("HTML", "DOCX")
              ),
              shiny::checkboxInput(NS(id, "tables"),
                "Include tables (stratifications)",
                value = TRUE
              ),
              shiny::checkboxInput(NS(id, "interactive"),
                "Interactive HTML",
                value = TRUE
              ),

              # Button
              shiny::downloadButton(NS(id, "downloadReport"), "Create Report")
            ),

            # Main panel for displaying outputs ----
            shiny::mainPanel(
              shiny::textOutput(NS(id, "report_text"))
            )
          )
        ))
      }
    })

    shiny::observeEvent(input$format, {
      if (input$format == "HTML") {
        shinyjs::show(id = "interactive")
      } else {
        shinyjs::hide(id = "interactive")
      }
    })

    method <- reactive({
      unique(signals_padded()$method)
    })

    number_of_weeks <- reactive({
      unique(signals_padded()$number_of_weeks)
    })

    # Download generated report
    output$report_text <- renderText({
      paste(
        "Generates report for", pathogen_vars(), " stratified ",
        "by ", paste0(strat_vars(), collapse = ", "), "for the last",
        number_of_weeks(), " weeks using ",
        names(available_algorithms())[available_algorithms() == method()],
        " as outbreak detection algorithm."
      )
    })


    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0(
          "SignalDetectionReport.",
          switch(input$format,
            HTML = "html",
            DOCX = "docx",
            PDF = "pdf"
          )
        )
      },
      content = function(con) {
        run_report(
          report_format = input$format,
          data = filtered_data(),
          method = names(available_algorithms()[which(available_algorithms() == method())]),
          number_of_weeks = number_of_weeks(),
          strata = strat_vars(),
          interactive = input$interactive,
          tables = input$tables,
          output_file = con,
          signals_padded = signals_padded(),
          signals_agg = signals_agg(),
          intervention_date = intervention_date()
        )
      }
    )
  })
}
