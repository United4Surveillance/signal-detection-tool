#' time_series_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_time_series_plot_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    # shiny::plotOutput(shiny::NS(id, "time_series_plot"))
    plotly::plotlyOutput(shiny::NS(id, "time_series_plot"))
  )
}

#' time_series_plot Server Functions
#'
#' @noRd
mod_time_series_plot_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$time_series_plot <- shiny::renderPlot({
    #   input_example %>%
    #     get_signals_farringtonflexible() %>%
    #     plot_time_series(results = ., interactive = FALSE)
    # })
    output$time_series_plot <- plotly::renderPlotly({
      input_example %>%
        get_signals_farringtonflexible() %>%
        plot_time_series(results = ., interactive = TRUE)
    })

  })
}

## To be copied in the UI
# mod_time_series_plot_ui("time_series_plot_1")

## To be copied in the server
# mod_time_series_plot_server("time_series_plot_1")
