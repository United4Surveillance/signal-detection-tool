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
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        shiny::uiOutput(ns("signals_tab_ui"))
      ),
      footer_text
    )
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
  no_algorithm_possible,
  intervention_date,
  min_cases_signals
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is only shown if data check returns no errors
    output$signals_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        datacheck_error_message
      } else if (!number_of_weeks_input_valid()) {
        nweeks_error_message
      } else if (no_algorithm_possible() == TRUE) {
        algorithm_error_message
      } else {
        shiny::tagList(
          bslib::layout_column_wrap(
            width = ifelse("None" %in% strat_vars(), 1 / 2, 1 / 3),
            !!!(list(
              bslib::value_box(
                height = 135,
                theme = bslib::value_box_theme(bg = "#304794", fg = "#FFFFFF"),
                title = "Method, disease and period",
                value = shiny::div(
                  "Algorithm: ", get_name_by_value(method(), available_algorithms()),
                  shiny::tags$br(),
                  "Disease: ", unique(filtered_data()$pathogen),
                  shiny::tags$br(),
                  "Time period: ", shiny::span(shiny::textOutput(ns("signal_period_text"), inline = TRUE))
                )
              ),
              bslib::value_box(
                height = 135,
                theme = bslib::value_box_theme(bg = dplyr::if_else(sum(signal_results_unstratified()$alarms) > 0, "#DF536B", "#23FF00"), fg = "#FFFFFF"),
                title = "Number of unstratified cases and alarms",
                value = shiny::div(
                  "Cases: ", shiny::span(paste0(sum(signal_results_unstratified()$cases)), inline = TRUE),
                  shiny::tags$br(),
                  "Alarms: ", shiny::span(paste0(sum(signal_results_unstratified()$alarms)), inline = TRUE)
                )
              ),
              if (!"None" %in% strat_vars()) {
                # Box of alarms by stratum
                bslib::value_box(
                  height = 135,
                  theme = bslib::value_box_theme(bg = dplyr::if_else(sum(signals_agg()$n_alarms) > 0, "#DF536B", "#23FF00"), fg = "#FFFFFF"),
                  title = "Number of stratified alarms",
                  value = shiny::div(
                    shiny::tagList(
                      shiny::htmlOutput(ns("signals_stratum"))
                    )
                  )
                )
              } else {
                NULL
              }
            ) %>%
              purrr::compact()
            )
          ),
          if (!"None" %in% strat_vars()) {
            shiny::tagList(
              bslib::card(
                shiny::h1("Signals by stratum"),
                shiny::uiOutput(ns("plot_table_stratas")),
                shiny::uiOutput(ns("alarm_button"))
              )
            )
          },
          bslib::card(
            full_screen = TRUE,
            shiny::h1(paste0(
              "Timeseries of weekly cases with signal detection applied to the last ",
              number_of_weeks(), " weeks."
            )),
            shiny::uiOutput(ns("ts_filter_var")),
            shiny::uiOutput(ns("ts_filter_val")),
            tags$style(shiny::HTML(paste0("#", id, "-ts_filter_var{display:inline-block}"))),
            tags$style(shiny::HTML(paste0("#", id, "-ts_filter_val{display:inline-block; vertical-align: top;}"))),
            plotly::plotlyOutput(ns("time_series_plot"))
          ),
          bslib::card(
            min_height = "775px",
            shiny::h1("Signal detection table"),
            DT::DTOutput(ns("signals"))
          )
        )
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

      results <- get_signals_all(
        preprocessed_data = filtered_data(),
        method = method(),
        intervention_date = intervention_date(),
        stratification = strat_vars_tidy(),
        date_var = "date_report",
        number_of_weeks = number_of_weeks()
      )
      results %>% dplyr::mutate(
        alarms = dplyr::if_else(alarms & cases < min_cases_signals(),
          FALSE, alarms, missing = alarms
        )
      )
    })

    signals_agg <- shiny::reactive({
      shiny::req(signal_results)
      aggregate_signals(signal_results(), number_of_weeks = number_of_weeks())
    })

    signal_weeks <- shiny::reactive({
      shiny::req(signal_results())
      signal_results() %>%
        dplyr::mutate(date_week = as.Date(paste0(year, "-", week, "-1"), "%Y-%W-%u")) %>%
        dplyr::distinct(date_week, .keep_all = T) %>%
        dplyr::arrange(desc(date_week)) %>%
        head(number_of_weeks())
    })

    signal_results_unstratified <- shiny::reactive({
      shiny::req(signal_results())
      signal_results() %>%
        dplyr::filter(is.na(category)) %>%
        dplyr::arrange(year, week) %>%
        dplyr::slice_tail(n = number_of_weeks())
    })

    alarms_trig <- shiny::reactive({
      toggle_numbers <- input$alarms_trig
      # Default to FALSE if toggle_numbers is NULL
      if (is.null(toggle_numbers)) {
        toggle_numbers <- FALSE
      }

      toggle_numbers
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
          plot <- decider_barplot_map_table(signals_agg(), filtered_data(), category, toggle_alarms = alarms_trig())

          if (category %in% names(pretty_variable_names())) {
            category_label <- pretty_variable_names()[category][[1]]
          } else {
            category_label <- category
          }

          bslib::card(
            full_screen = TRUE,
            width = "auto",
            bslib::card_header(shiny::div("Distribution by ", category_label, ",", signal_period())),
            plot
          )
        })

        column_plots <- shiny::fluidRow(
          lapply(1:n_plots_tables, function(x) shiny::column(12 / n_plots_tables, plot_table_list[x]))
        )


        columns_with_header <- list(column_plots)

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
      if (grepl("glm", method())) {
        # for those the results are already padded
        signal_results()
      } else {
        pad_signals(filtered_data(), signal_results())
      }
    })

    # based on the user input which timeseries should be visualised filter the signals_padded
    signals_padded_filtered <- shiny::reactive({
      shiny::req(signals_padded())
      if (is.null(ts_filter_var_tidy())) {
        results <- signals_padded() %>%
          dplyr::filter(is.na(category))
      } else {
        if (!is.null(input$ts_filter_val)) {
          results <- signals_padded() %>%
            dplyr::mutate(stratum = dplyr::if_else(!is.na(category) & is.na(stratum), tidyr::replace_na("unknown"), stratum)) %>%
            dplyr::filter(category == input$ts_filter_var & stratum == input$ts_filter_val)
        } else {
          # we should not get inside here but for completeness
          results <- signals_padded() %>%
            dplyr::filter(is.na(category))
        }
      }
      return(results)
    })

    signal_period <- shiny::reactive({
      shiny::req(signal_weeks())
      if (signal_weeks()$year[1] != signal_weeks()$year[number_of_weeks()]) {
        signal_period <- paste0(format(signal_weeks()$date_week[number_of_weeks()], "W%W-%Y"), " - ", format(signal_weeks()$date_week[1], "W%W-%Y"))
      } else {
        signal_period <- paste0("W", signal_weeks()$week[number_of_weeks()], "-", signal_weeks()$week[1], " ", signal_weeks()$year[1])
      }

      as.character(signal_period)
    })

    # visualisation of the timeseries
    # visualisation of the timeseries
    output$time_series_plot <- plotly::renderPlotly({
      shiny::req(signals_padded_filtered())
      if (nrow(signals_padded_filtered()) > 0) {
        plot_time_series(signals_padded_filtered(), interactive = TRUE, intervention_date = intervention_date())
      }
    })

    # signals table
    output$signals <- DT::renderDT({
      req(!errors_detected())
      build_signals_table(signal_results(),
        format = "DataTable"
      )
      # FIXME: interactive mode not working here?
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

        if (i %in% names(pretty_variable_names())) {
          category_label <- pretty_variable_names()[i][[1]]
        } else {
          category_label <- i
        }

        text_output <- paste0(
          text_output,
          paste0(category_label, ": ", n_alarms, "<br/>")
        )
      }
      shiny::HTML(text_output)
    })

    output$signal_period_text <- shiny::renderText({
      signal_period()
    })

    output$alarm_button <- shiny::renderUI({
      req(!errors_detected())

      if (length(strat_vars_tidy()) > 0) {
        shiny::tagList(
          shiny::br(),
          shinyWidgets::materialSwitch(
            inputId = ns("alarms_trig"),
            label = "Show number of alarms",
            value = TRUE,
            right = TRUE,
            status = "primary"
          )
        )
      }
    })


    # Return list of subsetted data and parameters
    return(list(
      signals_padded = shiny::reactive(signals_padded()),
      signals_agg = shiny::reactive(signals_agg())
    ))
  })
}
