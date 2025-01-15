#' tabpanel "Signal Line List" UI Function
#'
#' @description A shiny Module for a tab to display line list cases related to a
#' detected signal based on parameters inputs chosen. Ability to download data
#' to file.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_linelist_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Signal Line List",
    icon = shiny::icon("magnifying-glass"),
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::uiOutput(ns("signal_linelist_tab_ui"))
  )
}


#' tabpanel "Signal Line List" Server Functions
#'
#' @noRd
mod_tabpanel_linelist_server <- function(
    id,
    filtered_data,
    errors_detected,
    number_of_weeks_input_valid,
    method,
    no_algorithm_possible,
    intervention_date,
    signals_padded) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is only shown if data check returns no errors
    output$signal_linelist_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else if (!number_of_weeks_input_valid()) {
        return(nweeks_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::wellPanel(
                shiny::h3("Investigate signals", style = "color:#304794"),
                shiny::span("Click into the table to select the signals you want to investigate."),
                shiny::hr(),
                shiny::span(
                  paste0(
                    "Detected signals using method '",
                    get_name_by_value(method(), available_algorithms()), "':"
                  ),
                  style = "font-size:120%;font-weight: bold"
                ),
                shiny::br(),
                DT::DTOutput(ns("show_signals_padded"))
              )
            )
          ),
          shiny::br(),
          shiny::h3("Line list of selected signals", style = "color:#304794"),
          shiny::span("Click any of the buttons below to export the line list in your desired format."),
          DT::DTOutput(ns("linelist"))
        ))
      }
    })


    true_signals <- shiny::reactive({
      shiny::req(signals_padded)

      signals <- signals_padded() %>% dplyr::filter(alarms == TRUE)
      if (nrow(signals) > 0){
        signals <- signals %>% dplyr::mutate(signal_id = 1:dplyr::n(), .before = 1)
      }
      return(signals)
    })

    # output padded signal data in table
    output$show_signals_padded <- DT::renderDT({
      req(!errors_detected())
      req(true_signals)

      signals <- true_signals()

      if (nrow(signals) == 0) {
        # Return a placeholder message
        build_empty_datatable("No signals found.")
      } else {
        # Render the actual data table
        build_signals_table(
          signals,
          format = "DataTable",
          dt_selection_type = "multiple"
        )
      }
    })

    # display line lists of selected signals
    output$linelist <- DT::renderDataTable({
      req(filtered_data)
      req(true_signals)
      # check if any signals are selected for investigation
      req(!is.na(input$show_signals_padded_rows_selected))
      # selected rows by user in UI
      selected_signal_ids <- sort(input$show_signals_padded_rows_selected)

      filter_rows <- function(df1_row, df2) {
        # extract year and week out of df1
        year <- df1_row$year
        week <- df1_row$week
        category <- df1_row$category
        stratum <- df1_row$stratum

        # determine start and end time of detection period
        start_date <- ISOweek::ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))
        end_date <- start_date + lubridate::days(6)

        # Dynamic filtering
        filtered_df <- df2 %>%
          dplyr::filter(
            date_report >= start_date & date_report <= end_date # Filter für das Datum
          )

        # further filtering if stratification was applied
        if (!is.na(category) && !is.na(stratum)) {
          filtered_df <- filtered_df %>%
            dplyr::filter(!!sym(category) == stratum)
        }

        return(filtered_df)
      }

      # Filter rows for each signal ID
      cases <- purrr::map_dfr(selected_signal_ids, function(ssid) {
        # Extract the corresponding row from true_signals()
        signal_row <- true_signals() %>% dplyr::slice(ssid)

        # Apply the filtering function
        filtered_cases <- filter_rows(signal_row, filtered_data())

        # Add signal_id column
        filtered_cases <- filtered_cases %>%
          dplyr::mutate(signal_id = ssid, .before = 1)

        return(filtered_cases)
      })

      filename_download <- "signals_line_list"

      DT::datatable(
        cases,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = list("copy",
                     list(extend = 'csv',   filename =  filename_download),
                     list(extend = 'excel', filename =  filename_download),
                     list(extend = 'pdf', filename =  filename_download),
                      "print"),
          scrollX = TRUE
        )
      )
    })
  })
}
