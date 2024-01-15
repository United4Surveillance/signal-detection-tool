#' time_series_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_time_series_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Plot of timeseries"),
    plotly::plotlyOutput(ns("timeseries_plot"))
  )
}

#' time_series_plot Server Functions
#'
#' @noRd
mod_plot_time_series_server <- function(id, indata, strat_vars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$timeseries_plot <- plotly::renderPlotly({
      shiny::req(indata, strat_vars)
      results <- get_signals(data = indata(),
                             method = "farrington",
                             stratification = strat_vars())
      SignalDetectionTool::plot_time_series(results, interactive = TRUE)
    })
  })
}

## To be copied in the UI
# mod_plot_time_series_ui("time_series_plot_1")

## To be copied in the server
# mod_plot_time_series_server("time_series_plot_1")
