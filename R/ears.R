#' Get signals of surveillance's EARS algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_time_units integer, specifying number of time units to generate signals for. The default is weeks
#' @param method string indicating which method to use: one of "C1", "C2", "C3"
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly".
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data()
#' results <- get_signals_ears(data_aggregated)
#' results
#' }
get_signals_ears <- function(data_aggregated,
                             number_of_time_units = 52,
                             method = "C1",
                             time_unit = "weekly") {
  checkmate::assert(
    checkmate::check_integerish(number_of_time_units)
  )
  checkmate::assert(
    checkmate::check_choice(method, choices = c("C1", "C2", "C3"))
  )
  checkmate::assert_choice(
    time_unit,
    choices = c("weekly", "biweekly", "monthly"),
    null.ok = FALSE
  )

  # using default value for baseline
  baseline <- 7

  sts_cases <- convert_to_sts(data_aggregated, time_unit = time_unit)

  num_time_units_total <- length(sts_cases@observed)
  num_time_units_for_calibration <- num_time_units_total - number_of_time_units

  if (num_time_units_for_calibration < 0) {
    warning(paste0(
      "The number of time units you want to generate alarms for (n = ", number_of_time_units, ")",
      " is higher than the number of time units you have in your data (n = ", num_time_units_total, ")."
    ))
    return(NULL)
  } else if (num_time_units_for_calibration < baseline) {
    warning(paste0(
      "Your data/stratification covers ",
      num_time_units_total,
      " number of time units in total and you want to generate alarms for ", number_of_time_units, ". ",
      "EARS uses ", baseline, " time units of data to calibrate an epidemiological basline. ",
      "You have ", num_time_units_for_calibration, " time units for calibration left in your data/stratification."
    ))
    return(NULL)
  }


  control <- list(
    range = ((num_time_units_total - number_of_time_units + 1):num_time_units_total),
    method = method,
    baseline = baseline,
    minSigma = 0,
    alpha = 0.001
  )

  # run EARS on data
  results <- surveillance::earsC(sts_cases, control)

  pad <- rep(NA, num_time_units_total - number_of_time_units)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  # ears does not return an expected value
  data_aggregated$expected <- NA

  return(data_aggregated)
}
