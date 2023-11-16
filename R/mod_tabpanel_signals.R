mod_tabpanel_results_ui <- function(id) {
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
        ,shiny::actionButton(NS(id,"run_get_signals"), "Run program")
      ),
      shiny::mainPanel(
        shiny::br()
        ,shiny::h3("Plot of timeseries")
        ,shiny::plotOutput(NS(id,"timeseries"))

        ,shiny::br()
        ,shiny::h3("Plot of age group")
        ,shiny::plotOutput(NS(id,"age_group"))

        ,shiny::br()
        ,shiny::h3("Signal detection plot")
        ,shiny::tableOutput(NS(id,"signals"))
      )
    )
  )
  }


mod_tabpanel_results_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## interactive 'yes/no'-button and weeks slider?
    output$timeseries <- shiny::renderPlot({
      results <- get_signals_farringtonflexible(indata)
      return(SignalDetectionTool::plot_time_series(results, interactive = F, number_of_weeks = 10))
    })

    output$age_group <- shiny::renderPlot({
      return(SignalDetectionTool::plot_agegroup_by(indata))
    })

    ## make ReactiveEvent?
    output$signals <- shiny::renderTable({
      # 'None' takes precendence over 'All'
      if ("None" %in% input$strat_vars) {
        results <- SignalDetectionTool::get_signals(indata)
      } else if ("All" %in% input$strat_vars) {
        results <- SignalDetectionTool::get_signals(indata, stratification = names(indata))
      } else {
        results <- SignalDetectionTool::get_signals(indata, stratification = input$strat_vars)
      }

      return(create_results_table(results, interactive = FALSE))

    })

  })
}
