#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  mod_tabpanel_help_server("help")
  dat <- mod_tabpanel_data_server("data")
  subdat <- mod_tabpanel_input_server("input", indata = dat)
  mod_tabpanel_signals_server("signals", indata = subdat)
  mod_tabpanel_report_server("report")
}
