mod_tabpanel_signals_ui <- function(id) {
  ns <- shiny::NS(id)

  # shiny::tabPanel(
  #   shiny::sidebarPanel(
  #     shiny::br()
  #     ,shiny::h2("Choose signals detection methods:")
  #     ,shiny::checkboxGroupInput(NS(id,"method_vars"),"Choose",c("Farringthon", "EARS"), selected = "Farringthon")
  #     ,shiny::br()
  #     ,shiny::actionButton(NS(id,"run_get_signals"), "Examine for signals")
  #   )
  #
  #   ,shiny::mainPanel(
  #     shiny::br()
  #     ,
  #   )
  #
  # )
  shiny::tabPanel(
    "Signals",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        br()
        ,shiny::actionButton(ns("run_get_signals"), "Run program")
      ),
      shiny::mainPanel(
        shiny::br()
        ,shiny::h3("Plot of timeseries")
        ,shiny::plotOutput(ns("timeseries"))

        ,shiny::br()
        ,shiny::h3("Plot of age group")
        ,shiny::plotOutput(ns("age_group"))

        ,shiny::br()
        ,shiny::h3("Signal detection plot")
        ,shiny::tableOutput(ns("signals"))
      )
    )
  )
  }


mod_tabpanel_signals_server <- function(id, indata, strat_vars) {
  observe({
    req(indata, strat_vars)
    print("signals-tab");
    print(head(indata()))
    print(paste(c("signals-tab strat_vars:", strat_vars())))
    })

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    strat_vars_tidy <- reactive({
      req(strat_vars)
      strat_vars_chr <- strat_vars()
      # Tidy up stratification vector
      if ("None" %in% strat_vars_chr) {strat_vars_chr <- NULL}
      # 'None' takes precedence over 'All'
      else if ("All" %in% strat_vars_chr) {strat_vars_chr <- names(indata())}

      print(strat_vars_chr)
      return(strat_vars_chr)
    })


    ## TODO: interactive 'yes/no'-button and weeks slider?
    ## TODO: apply over selected pathogens
    output$timeseries <- shiny::renderPlot({
      req(indata, strat_vars_tidy)
      results <- get_signals(data = indata(),
                             method = "farrington",
                             stratification = strat_vars_tidy())
      print(results)
      return(SignalDetectionTool::plot_time_series(results,
                                                   interactive = FALSE))
    })

    output$age_group <- shiny::renderPlot({
      return(SignalDetectionTool::plot_agegroup_by(indata()))
    })

    output$signals <- shiny::renderTable({
      print(c("strat_vars_tidy: ", strat_vars_tidy()))
      results <- SignalDetectionTool::get_signals(
        indata(), stratification = strat_vars_tidy())
      return(create_results_table(results, interactive = FALSE))
      # interactive mode not working here?
    })

  })
}
