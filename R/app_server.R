#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  mod_tabpanel_help_server("help")

  data_load_check_result <- mod_tabpanel_data_server("data")

  # question: is it correct to have data_load_check_result$errors_detected() here instead of
  # data_load_check_result$errors_detected without brackets. Without brackets it does not work as expected, i.e. already goes inside even if data was not loaded yet
  # but I dont understand
  shiny::observeEvent(data_load_check_result$errors_detected(),{
  # if there were no errors for the mandatory and optional variables proceed with signals and report tab
    if (!data_load_check_result$errors_detected()) {
      datinput <- mod_tabpanel_input_server("input",
                                            data = data_load_check_result$data,
                                            errors_detected = data_load_check_result$errors_detected)
      mod_tabpanel_signals_server("signals",
                                   data = datinput$data,
                                   strat_vars = datinput$strat_vars)
      mod_tabpanel_report_server("report")
    }
  }, ignoreNULL = TRUE)
}
