mod_tabpanel_signals_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Signals"
    ,shiny::h3("Plot of timeseries")
    # ,shiny::plotOutput(ns("timeseries"))
    ,plotly::plotlyOutput(ns("timeseries"))

    ,shiny::br()
    ,shiny::h3("Plot of age group")
    ,shiny::plotOutput(ns("age_group"))

    ,shiny::br()
    ,shiny::h3("Signal detection table")
    # ,shiny::tableOutput(ns("signals"))
    ,DT::DTOutput(ns("signals"))
    ,icon = shiny::icon("wave-square")
  )
  }


mod_tabpanel_signals_server <- function(id, data, strat_vars, errors_detected) {
  observe({
    req(data, strat_vars)
    })

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    strat_vars_tidy <- reactive({
      req(strat_vars)
      strat_vars_chr <- strat_vars()
      # Tidy up stratification vector
      if ("None" %in% strat_vars_chr) {strat_vars_chr <- NULL}
      # 'None' takes precedence over 'All'
      else if ("All" %in% strat_vars_chr) {strat_vars_chr <- names(data())}

      return(strat_vars_chr)
    })


    ## TODO: interactive 'yes/no'-button and weeks slider?
    ## TODO: apply over selected pathogens?
    output$timeseries <- plotly::renderPlotly({
      # shiny::renderPlot({
      req(data, strat_vars_tidy)
      req(!errors_detected())
      results <- get_signals(data = data(),
                             method = "farrington",
                             stratification = strat_vars_tidy())
      return(SignalDetectionTool::plot_time_series(results,
                                                   interactive = TRUE))
    })

    output$age_group <- shiny::renderPlot({
      req(!errors_detected())
      return(SignalDetectionTool::plot_agegroup_by(data()))
    })

    output$signals <- DT::renderDT({
      req(!errors_detected())
      # shiny::renderTable({
      results <- SignalDetectionTool::get_signals(
        data(), stratification = strat_vars_tidy())
      return(create_results_table(results, interactive = TRUE))
      # FIXME: interactive mode not working here?
    })

  })
}
