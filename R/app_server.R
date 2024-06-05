#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.sanitize.errors = TRUE)

  mod_tabpanel_help_server("help")

  data_load_check_result <- mod_tabpanel_data_server("data")

  datinput <- mod_tabpanel_input_server("input",
    data = data_load_check_result$data,
    errors_detected = data_load_check_result$errors_detected
  )

  signals_output <- mod_tabpanel_signals_server("signals",
    filtered_data = datinput$filtered_data,
    number_of_weeks = datinput$n_weeks,
    number_of_weeks_input_valid = datinput$weeks_input_valid,
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
    datinput$no_algorithm_possible,
    number_of_weeks_input_valid = datinput$weeks_input_valid,
    signals_padded = signals_output$signals_padded,
    signals_agg = signals_output$signals_agg,
    signal_data = signals_output$signal_data
  )
}
