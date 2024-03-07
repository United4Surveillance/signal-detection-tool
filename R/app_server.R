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

  datinput <- mod_tabpanel_input_server("input",
    data = data_load_check_result$data,
    errors_detected = data_load_check_result$errors_detected
  )

  signals_output <- mod_tabpanel_signals_server("signals",
    data = datinput$data,
    number_of_weeks = datinput$n_weeks,
    strat_vars = datinput$strat_vars,
    errors_detected = data_load_check_result$errors_detected,
    method = datinput$method,
    no_algorithm_possible = datinput$no_algorithm_possible
  )

  mod_tabpanel_report_server("report",
                             indata = datinput$data,
                             strat_vars = datinput$strat_vars,
                             pathogen_vars = datinput$pathogen_vars,
                             errors_detected = data_load_check_result$errors_detected,
                             method = datinput$method,
                             datinput$no_algorithm_possible,
                             number_of_weeks = datinput$n_weeks,
                             signal_results = signals_output$signal_results,
                             signals_agg = signals_output$signals_agg,
                             signal_data = signals_output$signal_data)

}
