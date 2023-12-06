#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  mod_tabpanel_help_server("help")
  datload <- mod_tabpanel_data_server("data")
  datinput <- mod_tabpanel_input_server("input", indata = datload)
  mod_tabpanel_signals_server("signals",
                              indata = datinput$data,
                              strat_vars = datinput$strat_vars)
  mod_tabpanel_report_server("report",
                             indata = datinput$data,
                             strat_vars = datinput$strat_vars,
                             pathogen_vars = datinput$pathogen_vars)
}
