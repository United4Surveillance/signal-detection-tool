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
  number_of_time_units_input_valid,
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
      } else if (!number_of_time_units_input_valid()) {
        return(ntime_units_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          bslib::card(
            min_height = "700px",
            shiny::h1("Signals Overview"),
            shiny::span("Click into the table to select the signals you want to further investigate."),
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
            plotly::plotlyOutput(ns("age_comparison"))
          ),
          bslib::layout_columns(
            col_widths = c(9, 3),
            bslib::card(
              min_height = "500px",
              shiny::h1("Epicurve"),
              plotly::plotlyOutput(ns("epicurve"))
            ),
            bslib::card(
              min_height = "500px",
              shiny::h1("Select stratum epicurve"),
              shiny::uiOutput(ns("epicurve_stratum_ui"))
            )
          ),
          bslib::card(
            min_height = "500px",
            shiny::h1("Case Linelist for Selected Signals"),
            shiny::span("Export or review cases linked to the selected signals."),
            DT::DTOutput(ns("linelist"))
          )
        ))
      }
    })


    true_signals <- shiny::reactive({
      shiny::req(signals_padded())

      signals <- signals_padded() %>% dplyr::filter(alarms == TRUE)
      if (nrow(signals) > 0) {
        signals <- signals %>% dplyr::mutate(signal_id = 1:dplyr::n(), .before = 1)
      }
      return(signals)
    })

    # output padded signal data in table
    output$show_signals_padded <- DT::renderDT({
      shiny::req(!errors_detected())
      shiny::req(true_signals)

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
      shiny::req(cases_linelist())
      shiny::req(true_signals())

      linelist_cases <- dplyr::bind_rows(
        cases_linelist()$cases %>% dplyr::mutate(signal = T),
        cases_linelist()$cases_comparison %>% dplyr::mutate(signal = F)
      )
      cases_agg <- linelist_cases %>%
        dplyr::count(signal, age_group) %>%
        dplyr::group_by(signal) %>%
        dplyr::mutate(
          total_n = sum(n),
          perc = round(n / total_n * 100)
        ) %>%
        dplyr::ungroup()

      plot_agegroup_comparison(cases_agg, unique(true_signals()$number_of_time_units), unique(true_signals()$time_unit))
    })

    # Generate epicurve plot
    epicurve_plot <- shiny::reactive({
      linelist <- shiny::req(cases_linelist())
      epicurve_stratum <- shiny::req(input$epicurve_stratum)

      cases_agg <- dplyr::bind_rows(
        linelist$cases %>%
          dplyr::mutate(signal = TRUE),
        linelist$cases_comparison %>%
          dplyr::mutate(signal = FALSE)
      )

      selected_stratum <- if (
        identical(epicurve_stratum, "None")
      ) {
        NULL
      } else {
        epicurve_stratum
      }

      plot_epicurve(
        cases_agg,
        stratum = selected_stratum
      )
    })

    # Display epicurve graphic
    output$epicurve <- plotly::renderPlotly({
      epicurve_plot()
    })

    # make selection of stratum possible
    output$epicurve_stratum_ui <- shiny::renderUI({
      shiny::req(cases_linelist())

      dat <- dplyr::bind_rows(
        cases_linelist()$cases,
        cases_linelist()$cases_comparison
      )

      stratum_choices <- dat %>%
        dplyr::select(where(is.character) | where(is.factor)) %>%
        dplyr::select(
          -dplyr::any_of(c("pathogen")),
          -dplyr::ends_with("_id")
        ) %>%
        names() %>%
        sort()

      shiny::selectInput(
        inputId = ns("epicurve_stratum"),
        label = "Select a stratum to display in the epicurve",
        choices = c("None", stratum_choices),
        selected = "None",
        multiple = FALSE,
        width = "100%"
      )
    })

    cases_linelist <- shiny::reactive({
      shiny::req(filtered_data())
      shiny::req(true_signals())
      shiny::req(signals_padded())
      shiny::req(!is.na(input$show_signals_padded_rows_selected))

      selected_signal_ids <- sort(input$show_signals_padded_rows_selected)

      # fix problem with displaying all values when signals with stratum category "unknown" are selected
      replace_unknown <- function(data) {
        data %>%
          dplyr::mutate(
            dplyr::across(
              where(is.character),
              function(x) tidyr::replace_na(x, "unknown")
            ),
            dplyr::across(
              where(is.factor),
              function(x) {
                forcats::fct_na_value_to_level(
                  x,
                  level = "unknown"
                )
              }
            )
          )
      }

      build_signal_and_comparison_linelist(
        selected_signal_ids = selected_signal_ids,
        true_signals = replace_unknown(true_signals()),
        signals_padded = replace_unknown(signals_padded()),
        filtered_data = replace_unknown(filtered_data())
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
