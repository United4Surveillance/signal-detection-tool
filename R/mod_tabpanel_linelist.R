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
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        bslib::layout_column_wrap(
          width = 1, height = "100%",
          heights_equal = "row",
          shiny::uiOutput(ns("signal_linelist_tab_ui"))
        )
      ),
      footer_text
    )
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
  signals_padded
) {
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
          bslib::card(
            min_height = "700px",
            shiny::h1("Investigate signals"),
            shiny::span("Click into the table to select the signals you want to investigate."),
            shiny::span(
              paste0(
                "Detected signals using method '",
                get_name_by_value(method(), available_algorithms()), "':"
              ),
              style = "font-size:120%;font-weight: bold"
            ),
            DT::DTOutput(ns("show_signals_padded"))
          ),
          bslib::card(
            min_height = "500px",
            shiny::h1("Signal Investigation"),
            plotly::plotlyOutput(ns("age_comparison")),
            shiny::span("Click any of the buttons below to export the line list in your desired format."),
            DT::DTOutput(ns("linelist"))
          )
        ))
      }
    })


    true_signals <- shiny::reactive({
      shiny::req(signals_padded)

      signals <- signals_padded() %>% dplyr::filter(alarms == TRUE)
      if (nrow(signals) > 0) {
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

    # display age distribution graphic
    output$age_comparison <- plotly::renderPlotly({
      req(cases_linelist)
      req(true_signals)

      linelist_cases <- dplyr::bind_rows(cases_linelist()$cases |> dplyr::mutate(signal = T),
                                        cases_linelist()$cases_comparison |> dplyr::mutate(signal = F))
      cases_agg <- linelist_cases |>
        dplyr::count(signal,age_group)

      plot_agegroup_comparison(cases_agg,unique(true_signals()$number_of_weeks))

    })

    cases_linelist <- shiny::reactive({
      req(filtered_data)
      req(true_signals)
      # check if any signals are selected for investigation
      req(!is.na(input$show_signals_padded_rows_selected))

      # selected rows by user in UI
      selected_signal_ids <- sort(input$show_signals_padded_rows_selected)

      filter_rows_by_period <- function(linelist, start_date, end_date, df1_row = NULL) {

        filtered_df <- linelist %>%
          dplyr::filter(date_report >= start_date & date_report <= end_date)

        # filtering for the specific stratum in the linelist
        if (!is.null(df1_row)){
          category <- df1_row$category
          stratum <- df1_row$stratum
          if (!is.na(category) && !is.na(stratum)) {
            filtered_df <- filtered_df %>%
              dplyr::filter(!!rlang::sym(category) == stratum)
          }
        }

        filtered_df
      }

      filter_rows_past_weeks <- function(signals_padded, linelist) {

        number_of_weeks <- unique(signals_padded$number_of_weeks)
        signals_padded_n_weeks <- signals_padded |>
          dplyr::filter(!is.na(alarms)) |>
          dplyr::mutate(
            week_start = ISOweek::ISOweek2date(
              paste0(year, "-W", sprintf("%02d", week), "-1")
            ))

        start_date <- min(signals_padded_n_weeks$week_start)
        end_date <- max(signals_padded_n_weeks$week_start) + lubridate::days(6)

        filter_rows_by_period(linelist, start_date, end_date)
      }

      filter_rows_signal_week <- function(df1_row, df2) {
        start_date <- ISOweek::ISOweek2date(
          paste0(df1_row$year, "-W", sprintf("%02d", df1_row$week), "-1")
        )
        end_date <- start_date + lubridate::days(6)

        filter_rows_by_period(df2, start_date, end_date, df1_row)
      }

      # Filter rows for each signal ID
      cases <- purrr::map_dfr(selected_signal_ids, function(ssid) {
        # Extract the corresponding row from true_signals()
        signal_row <- true_signals() %>% dplyr::slice(ssid)

        # Apply the filtering function
        filtered_cases <- filter_rows_signal_week(signal_row, filtered_data())

        # Add signal_id column
        filtered_cases <- filtered_cases %>%
          dplyr::mutate(signal_id = ssid, .before = 1)

        return(filtered_cases)
      })

      # remove duplicated cases in signals if there are some

      # Apply the filtering function
      cases_comparison <- filter_rows_past_weeks(signals_padded(), filtered_data())
      # removing signal cases
      cases_comparison <- cases_comparison |> dplyr::anti_join(cases, by = "case_id")

      list(
        cases = cases,
        cases_comparison = cases_comparison
      )
    })

    # display line lists of selected signals
    output$linelist <- DT::renderDataTable({


      filename_download <- "signals_line_list"

      DT::datatable(
        cases_linelist()$cases,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = list(
            "copy",
            list(extend = "csv", filename = filename_download),
            list(extend = "excel", filename = filename_download),
            list(extend = "pdf", filename = filename_download),
            "print"
          ),
          scrollX = TRUE
        )
      )
    })
  })
}
