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
    mod_plot_time_series_ui(id = ns("timeseries")),
    shiny::br(),
    shiny::h3("Plot of age group"),
    plotly:::plotlyOutput(ns("age_group")),
    shiny::br(),
    shiny::h3("Signal detection table"),
    # shiny::tableOutput(ns("signals")),
    DT::DTOutput(ns("signals")),
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
    strat_vars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # fix stratification vars
    strat_vars_tidy <- reactive({
      req(strat_vars)
      strat_vars_chr <- strat_vars()
      # Tidy up stratification vector
      if ("None" %in% strat_vars_chr) {strat_vars_chr <- NULL}
      # 'None' takes precedence over 'All'
      else if ("All" %in% strat_vars_chr) {strat_vars_chr <- names(data())}

      return(strat_vars_chr)
    })

    # generate signals once
    signal_results <- shiny::reactive({
      shiny::req(!errors_detected())
      results <- SignalDetectionTool::get_signals(
        data = data(),
        stratification = strat_vars_tidy(),
        date_var = "date_report",
        number_of_weeks = number_of_weeks()
      ) %>%
        dplyr::filter(!is.na(alarms))

      results
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


    ## TODO: interactive 'yes/no'-button and weeks slider?
    ## TODO: apply over selected pathogens?
    mod_plot_time_series_server(id = "timeseries",
                                signals = signal_results)

    # agegroup plot
    output$age_group <- plotly::renderPlotly({
      req(!errors_detected())
      SignalDetectionTool::plot_agegroup_by(signal_data(),
                                            by_col = strat_vars_tidy()[1],
                                            interactive = TRUE)
    })

    # signals table
    output$signals <- DT::renderDT({
      req(!errors_detected())
      create_results_table(signal_results(),
                           interactive = TRUE)
      # FIXME: interactive mode not working here?
    })

  })
}
