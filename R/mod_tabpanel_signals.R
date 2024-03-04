#' tabpanel "signals" UI Function
#'
#' @description A shiny Module for a tab to generate and display results from
#' signal detection methods based on parameters inputs chosen.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_signals_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Signals",
    icon = shiny::icon("wave-square"),
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::uiOutput(ns("signals_tab_ui")),
  )
}


#' tabpanel "signals" Server Functions
#'
#' @noRd
mod_tabpanel_signals_server <- function(
    id,
    data,
    errors_detected,
    number_of_weeks,
    strat_vars,
    method,
    no_algorithm_possible) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is onlyu shown if data check returns no errors
    output$signals_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          fluidRow(
            # Creation of boxes using div
            column(width = 3,
                   shiny::div(class = "value-box blue",
                              shiny::div(class = "title", "Outbreak detection algorithm"),
                              shiny::div(class = "value", method())
                   )
            ),
            column(width = 2,
                   shiny::div(class = "value-box blue",
                              shiny::div(class = "title", "Number of weeks"),
                              shiny::div(class = "value", number_of_weeks())
                   )
            ),
            # Red box if alarms were found
            if(sum(signals_agg()$n_alarms) > 0) {
              column(width = 2,
                   shiny::div(class = "value-box red",
                              shiny::div(class = "title", "Number of alarms"),
                              shiny::div(class = "value", shiny::textOutput(ns("n_alarms")))
                   )
              # Green box if no alarms were found
              )} else{
                column(width = 2,
                       shiny::div(class = "value-box green",
                                  shiny::div(class = "title", "Number of alarms"),
                                  shiny::div(class = "value", shiny::textOutput(ns("n_alarms")))
                       )
                )

                }
            ,
            # Box of alarms by stratum
            if(!"None" %in% strat_vars()){
              # Red box if alarms were found
              if(sum(signals_agg()$n_alarms) > 0){
                column(width = 4,
                     shiny::div(class = "value-box red",
                                shiny::div(class = "title", "Number of alarms by stratum"),
                                shiny::div(class = "value", shiny::htmlOutput(ns("signals_stratum")))
                     )
              )
              # Green box if no alarms were found
              } else {
                column(width = 4,
                       shiny::div(class = "value-box green",
                                  shiny::div(class = "title", "Number of alarms by stratum"),
                                  shiny::div(class = "value", shiny::htmlOutput(ns("signals_stratum")))
                       )
                )
                }
            }
          ),
          uiOutput(ns("plot_table_stratas")),
          shiny::br(),
          shiny::h3(paste0("Timeseries of weekly cases on country level with signal detection applied to the last ",
                           number_of_weeks(), " weeks.")),
          plotly::plotlyOutput(ns("time_series_plot")),
          shiny::h3("Signal detection table"),
          DT::DTOutput(ns("signals"))
        ))
      }
    })

    # fix stratification vars
    strat_vars_tidy <- reactive({
      req(strat_vars)
      strat_vars_chr <- strat_vars()
      # Tidy up stratification vector
      if ("None" %in% strat_vars_chr) {
        strat_vars_chr <- NULL
      }
      # 'None' takes precedence over 'All'
      else if ("All" %in% strat_vars_chr) {
        strat_vars_chr <- names(data())
      }

      return(strat_vars_chr)
    })

    # generate signals once
    signal_results <- shiny::reactive({
      shiny::req(!errors_detected())
      shiny::req(!no_algorithm_possible())
      results <- SignalDetectionTool::get_signals(
        data = data(),
        method = method(),
        stratification = strat_vars_tidy(),
        date_var = "date_report",
        number_of_weeks = number_of_weeks()
      )
      results
    })

    signals_agg <- shiny::reactive({
      shiny::req(signal_results)
      aggregate_signals(signal_results(), number_of_weeks = number_of_weeks())
    })

    signal_data <- shiny::reactive({
      shiny::req(!errors_detected())
      shiny::req(signal_results())
      weeks <- paste0(signal_results()$year, "-", signal_results()$week, "-1") %>%
        as.Date("%Y-%W-%u") %>%
        unique() %>%
        sort()

      dates <- seq(weeks[1], weeks[length(weeks)], by = "day")
      data_n_weeks <- data() %>%
        dplyr::filter(date_report %in% dates) # this has to use the same variable as in get_signals()
      data_n_weeks
    })

    output$plot_table_stratas <- shiny::renderUI({
      shiny::req(signal_results)
      plot_table_list <- list()

      # using the categories of the signal_results instead of strat_vars_tidy
      # because it could be that not for all selected strat_vars signals could have been generated
      # we would still want to give this feedback to the user then, this is not implemented yet
      signal_categories <- unique(signal_results()$category)
      # remove the NA category which is generate when signals were generated unstratified
      signal_categories <- signal_categories[!is.na(signal_categories)]
      n_plots_tables <- length(signal_categories)

      # generating barcharts, maps or tables and header for this ui section
      # The number of plots/tables and the header generated depends on the number of signal_categories
      # if strata were selected by the user
      if (n_plots_tables != 0) {
        # populate the plot_table_list with plots/tables of each category
        plot_table_list <- lapply(signal_categories, function(category) {
          decider_barplot_map_table(signals_agg(), data(), category)
        })
        if (n_plots_tables == 1) {
          header <- h3(paste0("Visualisation and/or table showing the number of cases in the last ", number_of_weeks(),
                              " weeks with alarms from Signal Detection", " for the selected stratum ",
                              paste(signal_categories, collapse = ", "), "."))
        } else {
          header <- h3(paste0("Visualisations and/or tables showing the number of cases in the last ", number_of_weeks(),
                              " weeks with alarms from Signal Detection", " for the selected strata ",
                              paste(signal_categories, collapse = ", "), "."))
        }
        column_plots <- shiny::fluidRow(
          lapply(1:n_plots_tables, function(x) shiny::column(12 / n_plots_tables, plot_table_list[x]))
        )
        columns_with_header <- list(header, column_plots)

        # Return the combined UI elements
        column_plots_with_headers <- do.call(shiny::tagList, columns_with_header)

        return(column_plots_with_headers)

        # in case no strata were selected (n_plots_tables == 0) we show the country timeseries without signals
      } else {
        return(NULL)
      }

    })

    # timeseries plot with non-stratisfied signals
    output$time_series_plot <- plotly::renderPlotly({
      shiny::req(!errors_detected())
      shiny::req(!no_algorithm_possible())

      # preparing dataset with padding
      result_padding <- SignalDetectionTool::get_signals(
        data = data(),
        method = method(),
        date_var = "date_report",
        stratification = NULL,
        number_of_weeks = 52
      ) %>%
        dplyr::ungroup() %>%
        dplyr::select(year, week, upperbound_pad = upperbound, expected_pad = expected)

      # preparing dataset within actual signal detection period
      results <- SignalDetectionTool::get_signals(
        data = data(),
        method = method(),
        date_var = "date_report",
        stratification = NULL,
        number_of_weeks = number_of_weeks()
      ) %>%
        dplyr::arrange(year, week) %>%
        dplyr::left_join(x = ., y  = result_padding, by = c("year", "week"))

      plot_timeseries <- plot_time_series(results, interactive = TRUE)
    })

    # signals table
    output$signals <- DT::renderDT({
      req(!errors_detected())
      create_results_table(signal_results(),
        interactive = TRUE
      )
      # FIXME: interactive mode not working here?
    })
    output$n_alarms <- shiny::renderText({
      sum(signals_agg()$n_alarms)
    })

    output$signals_stratum <- shiny::renderUI({

      signals_strat <- signals_agg() %>%
        dplyr::group_by(category) %>%
        dplyr::summarise(n_alarms = sum(n_alarms))

      text_output <- c()

      for(i in signals_strat$category){

        n_alarms <- signals_strat %>%
          dplyr::filter(category == i) %>%
          dplyr::pull(n_alarms)

        text_output <- paste0(text_output,
                       paste0(i, ": ", n_alarms, "<br/>"))

        }
      shiny::HTML(text_output)

    })
  })
}
