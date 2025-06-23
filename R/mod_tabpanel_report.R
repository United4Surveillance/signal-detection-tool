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
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        shiny::uiOutput(ns("report_tab_ui"))
      ),
      footer_text
    )
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
        datacheck_error_message
      } else if (!number_of_weeks_input_valid()) {
        nweeks_error_message
      } else if (no_algorithm_possible() == TRUE) {
        algorithm_error_message
      } else {
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_title("Download Report", container = shiny::h1),
              shiny::selectInput(NS(id, "format"), "Choose a format:",
                choices = c("HTML", "DOCX")
              ),
              shiny::checkboxInput(NS(id, "tables"),
                "Include tables (stratifications)",
                value = TRUE
              ),
              shiny::downloadButton(NS(id, "downloadReport"), "Create Report")
            ),
            bslib::card(
              shiny::textOutput(NS(id, "report_text"))
            )
          )
        )
      }
    })

    method <- shiny::reactive({
      unique(signals_padded()$method)
    })

    number_of_weeks <- shiny::reactive({
      unique(signals_padded()$number_of_weeks)
    })

    # Download generated report
    output$report_text <- shiny::renderText({
      paste(
        "Generates report for", pathogen_vars(), " stratified ",
        "by ", paste0(strat_vars(), collapse = ", "), "for the last",
        number_of_weeks(), " weeks using ",
        names(available_algorithms())[available_algorithms() == method()],
        " as outbreak detection algorithm."
      )
    })


    output$downloadReport <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "SignalDetectionReport.",
          switch(input$format,
            HTML = "html",
            DOCX = "docx"
          )
        )
      },
      content = function(con) {
        run_report(
          report_format = input$format,
          data = filtered_data(),
          method = names(available_algorithms()[which(available_algorithms() == method())]),
          number_of_weeks = number_of_weeks(),
          pathogens = pathogen_vars(),
          strata = strat_vars(),
          tables = input$tables,
          output_file = con,
          output_dir = NULL,
          signals_padded = signals_padded() %>% dplyr::mutate(pathogen = pathogen_vars()),
          signals_agg = signals_agg() %>% dplyr::mutate(pathogen = pathogen_vars()),
          intervention_date = intervention_date()
        )
      }
    )
  })
}
