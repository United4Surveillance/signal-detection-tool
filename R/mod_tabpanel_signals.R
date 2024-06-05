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
    shiny::uiOutput(ns("signals_tab_ui"))
  )
}


#' tabpanel "signals" Server Functions
#'
#' @noRd
mod_tabpanel_signals_server <- function(
    id,
    filtered_data,
    errors_detected,
    number_of_weeks,
    number_of_weeks_input_valid,
    strat_vars,
    method,
    no_algorithm_possible) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is only shown if data check returns no errors
    output$signals_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else if (!number_of_weeks_input_valid()) {
        return(nweeks_error_message)
      } else if (no_algorithm_possible() == TRUE) {
        return(algorithm_error_message)
      } else {
        return(shiny::tagList(
          fluidRow(
            # Creation of boxes using div
            column(
              width = 3,
              shiny::div(
                class = "value-box blue",
                shiny::div(class = "title", "Outbreak detection algorithm"),
                shiny::div(class = "value", get_name_by_value(method(), available_algorithms()))
              )
            ),
            column(
              width = 2,
              shiny::div(
                class = "value-box blue",
                shiny::div(class = "title", "Number of weeks"),
                shiny::div(class = "value", number_of_weeks())
              )
            ),
           # Box of signals in total
            # Red box if signals were found, green if no signals
              column(
                width = 2,
                shiny::div(
                  class = ifelse(sum(signals_agg()$n_alarms) > 0,
                                 "value-box red", "value-box green"),
                  shiny::div(class = "title", "Number of signals"),
                  shiny::div(class = "value", shiny::textOutput(ns("n_alarms")))
                )
              ),
            # Box of signals by stratum
            if (!"None" %in% strat_vars()) {
              # Red box if signals were found, green if no signals
              column(
                width = 4,
                shiny::div(
                 class = ifelse(sum(signals_agg()$n_alarms) > 0,
                                 "value-box red", "value-box green"),
                  shiny::div(class = "title", "Number of signals by stratum"),
                  shiny::div(class = "value", shiny::htmlOutput(ns("signals_stratum")))
                )
              )
            }
          ),
          shiny::uiOutput(ns("alarm_button")),
          shiny::uiOutput(ns("plot_table_stratas")),
          shiny::br(),
          shiny::h3(paste0(
            "Timeseries of weekly cases with signal detection applied to the last ",
            number_of_weeks(), " weeks."
          )),
          shiny::uiOutput(ns("ts_filter_var")),
          shiny::uiOutput(ns("ts_filter_val")),
          tags$style(shiny::HTML(paste0("#", id, "-ts_filter_var{display:inline-block}"))),
          tags$style(shiny::HTML(paste0("#", id, "-ts_filter_val{display:inline-block; vertical-align: top;}"))),
          plotly::plotlyOutput(ns("time_series_plot")),
          shiny::h3("Signal detection table"),
          DT::DTOutput(ns("signals"))
        ))
      }
    })

    ## button to select which timeseries to visualise in signals tab
    output$ts_filter_var <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::req(!(length(unique(strat_vars())) == 1 & strat_vars() == "None")) # only show the filter button when strata were selected
      shiny::selectInput(
        inputId = ns("ts_filter_var"),
        label = "Choose stratum",
        choices = c(strat_vars(), "None"),
        selected = "None"
      )
    })

    ts_filter_var_tidy <- shiny::reactive({
      if (!is.null(input$ts_filter_var)) {
        if (input$ts_filter_var == "None") {
          NULL
        } else {
          input$ts_filter_var
        }
      } else {
        input$ts_filter_var
      }
    })

    output$ts_filter_val <- shiny::renderUI({
      # only when not NULL the dropdown to select values to filter for appears
      shiny::req(ts_filter_var_tidy())
      shiny::req(signal_results)

      filter_choices <- signal_results() %>%
        dplyr::filter(category == input$ts_filter_var) %>%
        dplyr::pull(stratum) %>%
        as.character() %>%
        tidyr::replace_na("unknown") %>%
        unique()

      shiny::selectInput(
        inputId = ns("ts_filter_val"),
        multiple = FALSE,
        label = "Choose value",
        choices = filter_choices
      )
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
        strat_vars_chr <- names(filtered_data())
      }

      return(strat_vars_chr)
    })

    # generate signals for number of weeks and stratification specified and always also the non stratified signals
    signal_results <- shiny::reactive({
      shiny::req(!errors_detected())
      shiny::req(!no_algorithm_possible())
      results <- SignalDetectionTool::get_signals(
        data = filtered_data(),
        method = method(),
        stratification = strat_vars_tidy(),
        date_var = "date_report",
        number_of_weeks = number_of_weeks()
      )
      # when stratified signals were computed also add unstratified signals to the dataframe so that all can be visualised
      if (!is.null(strat_vars_tidy())) {
        results_unstratified <- SignalDetectionTool::get_signals(
          data = filtered_data(),
          method = method(),
          stratification = NULL,
          date_var = "date_report",
          number_of_weeks = number_of_weeks()
        )
        results <- dplyr::bind_rows(results, results_unstratified)
      }
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
      data_n_weeks <- filtered_data() %>%
        dplyr::filter(date_report %in% dates) # this has to use the same variable as in get_signals()
      data_n_weeks
    })

    show_alarms <- shiny::reactive({
      shiny::req(input$alarms_trig)
      if (input$alarms_trig == "Show number of signals") {
        return(TRUE)
      } else {
        return(FALSE)
      }
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
          decider_barplot_map_table(signals_agg(), filtered_data(), category, toggle_alarms = show_alarms())
        })
        if (n_plots_tables == 1) {
          header <- h3(paste0(
            "Visualisation and/or table showing the number of cases in the last ", number_of_weeks(),
            " weeks with signals from Signal Detection", " for the selected stratum ",
            paste(signal_categories, collapse = ", "), "."
          ))
        } else {
          header <- h3(paste0(
            "Visualisations and/or tables showing the number of cases in the last ", number_of_weeks(),
            " weeks with signals from Signal Detection", " for the selected strata ",
            paste(signal_categories, collapse = ", "), "."
          ))
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
    # I dont want to recompute all of this again and again, it should be computed once
    # on all the stratification vars and then when the user selects everything has already been computed in the background! thus the computation part should be independent of the visualisation part and always run if strat_vars were selected
    # then in the visualisation part this is reactive to what was selected as variables
    signals_padded <- shiny::reactive({
      shiny::req(!errors_detected())
      shiny::req(!no_algorithm_possible())

      pad_signals(filtered_data(),signal_results())
    })

    # based on the user input which timeseries should be visualised filter the signals_padded
    signals_padded_filtered <- shiny::reactive({
      shiny::req(signals_padded())
      if (is.null(ts_filter_var_tidy())) {
        results <- signals_padded() %>%
          dplyr::filter(is.na(category))
      } else{
        if (!is.null(input$ts_filter_val)) {
          results <- signals_padded() %>%
            dplyr::mutate(stratum = dplyr::if_else(!is.na(category) & is.na(stratum),tidyr::replace_na("unknown"),stratum)) %>%
            dplyr::filter(category == input$ts_filter_var & stratum == input$ts_filter_val)}
        else{
          # we should not get inside here but for completeness
          results <- signals_padded() %>%
            dplyr::filter(is.na(category))
        }
      }
      return(results)
    })

    # visualisation of the timeseries
    output$time_series_plot <- plotly::renderPlotly({
      shiny::req(signals_padded_filtered())
      plot_time_series(signals_padded_filtered(), interactive = TRUE)
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
        dplyr::filter(!is.na(category)) %>% # for stratified results remove the unstratified signals
        dplyr::group_by(category) %>%
        dplyr::summarise(n_alarms = sum(n_alarms)) %>%
        dplyr::ungroup()

      text_output <- c()

      for (i in signals_strat$category) {
        n_alarms <- signals_strat %>%
          dplyr::filter(category == i) %>%
          dplyr::pull(n_alarms)

        text_output <- paste0(
          text_output,
          paste0(i, ": ", n_alarms, "<br/>")
        )
      }
      shiny::HTML(text_output)
    })

    output$alarm_button <- shiny::renderUI({
      req(!errors_detected())

      if (length(strat_vars_tidy()) > 0) {
        shiny::tagList(
          shiny::br(),
          shiny::selectInput(ns("alarms_trig"),
                             label = "Toggle number of signals on / off on stratification graphs",
                             choices = c("Don't show number of signals", "Show number of signals"),
                             selected = "Show number of signals")
        )
      }
    })


    # Return list of subsetted data and parameters
    return(list(
      signals_padded = shiny::reactive(signals_padded()),
      signals_agg = shiny::reactive(signals_agg()),
      signal_data = shiny::reactive(signal_data())
    ))

  })
}
