#' Get signals of surveillance's EARS algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for
#' @param method string indicating which method to use: one of "C1", "C2", "C3"
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' results <- get_signals_ears(data_aggregated)
#' }
get_signals_ears <- function(data_aggregated,
                             number_of_weeks = 52,
                             method = "C1") {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )
  checkmate::assert(
    checkmate::check_choice(method, choices = c("C1", "C2", "C3"))
  )
  # using default value for baseline
  baseline <- 7

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_weeks_for_calibration <- num_weeks_total - number_of_weeks

  if (num_weeks_for_calibration < 0) {
    warning(paste0(
      "The number of weeks you want to generate alarms for (n = ", number_of_weeks, ")",
      " is higher than the number of weeks you have in your data (n = ", num_weeks_total, ")."
    ))
    return(NULL)
  } else if (num_weeks_for_calibration < baseline) {
    warning(paste0(
      "Your data/stratification covers ",
      num_weeks_total,
      " number of weeks in total and you want to generate alarms for ", number_of_weeks, ". ",
      "EARS uses ", baseline, " weeks of data to calibrate an epidemiological basline. ",
      "You have ", num_weeks_for_calibration, " weeks for calibration left in your data/stratification."
    ))
    return(NULL)
  }


  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    method = method,
    baseline = baseline,
    minSigma = 0,
    alpha = 0.001
  )

  # run EARS on data
  results <- surveillance::earsC(sts_cases, control)

  pad <- rep(NA, num_weeks_total - number_of_weeks)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  # ears does not return an expected value
  data_aggregated$expected <- NA

  return(data_aggregated)
}
