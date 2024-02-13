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

    ## UI-portion of the tab below!
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
          mod_plot_time_series_ui(id = ns("timeseries")),
          shiny::br(),
          shiny::h3("Plot of age group"),
          plotly:::plotlyOutput(ns("age_group")),
          shiny::br(),
          shiny::h3("Signal detection table"),
          # shiny::tableOutput(ns("signals")),
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
      ) %>%
        filter_data_last_n_weeks(number_of_weeks = number_of_weeks())

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
    mod_plot_time_series_server(
      id = "timeseries",
      signals = signal_results
    )

    # agegroup plot
    output$age_group <- plotly::renderPlotly({
      req(!errors_detected())
      SignalDetectionTool::plot_agegroup_by(signal_data(),
        by_col = strat_vars_tidy()[1],
        interactive = TRUE
      )
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
