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
    "Signals",
    shiny::uiOutput(ns("signals_tab_ui")),
    icon = shiny::icon("wave-square")
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
        return(shiny::tagList(
          shiny::br(),
          shiny::h2("Data Format Check Failed"),
          shiny::p("Unfortunately, the selected data does not meet the required format."),
          shiny::p("Please make sure the data follows the correct structure and try again."),
          shiny::br(),
          shiny::hr(),
          shiny::p("You can check the data in the 'Data' tab for more details on the issue.")
        ))
      } else if (no_algorithm_possible() == TRUE) {
        return(shiny::tagList(
          shiny::br(),
          shiny::h3("There is no outbreak detection algorithm which can be applied to your current settings, please change your selected settings in the input tab and try again."),
          shiny::br()
        ))
      } else {
        return(shiny::tagList(
          uiOutput(ns("plot_table_stratas")),
          shiny::br(),
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

    output$plot_table_stratas <- renderUI({
      req(signal_results)
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
          header <- h3(paste0("Visualisation and/or table showing the number of cases in the last ", number_of_weeks(), " weeks with alarms from Signal Detection", " for the selected stratum ", paste(signal_categories, collapse = ", "), "."))
        } else {
          header <- h3(paste0("Visualisations and/or tables showing the number of cases in the last ", number_of_weeks(), " weeks with alarms from Signal Detection", " for the selected strata ", paste(signal_categories, collapse = ", "), "."))
        }

        # in case no strata were selected (n_plots_tables == 0) we show the country timeseries
      } else {
        plot_timeseries <- plot_time_series(signal_results(), interactive = TRUE)
        plot_table_list[[1]] <- plot_timeseries
        # update the n_plots_tables such that creating the column_plots below works
        n_plots_tables <- 1
        header <- h3(paste0("Timeseries of weekly cases on country level with signal detection applied to the last ", number_of_weeks(), " weeks."))
      }

      column_plots <- fluidRow(
        lapply(1:n_plots_tables, function(x) column(12 / n_plots_tables, plot_table_list[x]))
      )
      columns_with_header <- list(header, column_plots)


      # Return the combined UI elements
      column_plots_with_headers <- do.call(tagList, columns_with_header)

      return(column_plots_with_headers)
})

    # signals table
    output$signals <- DT::renderDT({
      req(!errors_detected())
      create_results_table(signal_results(),
        interactive = TRUE
      )
      # FIXME: interactive mode not working here?
    })
  })
}
