#' Get signals of surveillance's farringtonFlexible algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate signals for
#' @param alpha_upper numeric between 0.001 and 0.2 (default: 0.05).
#'   Specifies the p-value cutoff used to define the threshold; for example, a value of 0.05 corresponds to using the 0.95 quantile.
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data()
#' results <- get_signals_farringtonflexible(data_aggregated)
#' results
#' }
get_signals_farringtonflexible <- function(data_aggregated,
                                           number_of_weeks = 52,
                                           alpha_upper = 0.05) {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  checkmate::assert(
    checkmate::check_number(alpha_upper, lower = 0.001, upper = 0.2)
  )

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_weeks_for_calibration <- num_weeks_total - number_of_weeks
  num_years_total <- floor((num_weeks_for_calibration - 26) / 52)

  if (num_weeks_for_calibration < 0) {
    warning(paste0(
      "The number of weeks you want to generate alarms for (n = ", number_of_weeks, ")",
      " is higher than the number of weeks you have in your data (n = ", num_weeks_total, ")."
    ))
    return(NULL)
  } else if (num_weeks_for_calibration < 52 + 26) {
    warning(paste0(
      "Your data/stratification covers ",
      num_weeks_total,
      " number of weeks in total and you want to generate alarms for ", number_of_weeks, ". ",
      "FarringtonFlexible needs at least 1.5 years (78 weeks) of data to calibrate an epidemiological basline. ",
      "You have ", num_weeks_for_calibration, " weeks for calibration left in your data/stratification."
    ))
    return(NULL)
  }

  alpha <- alpha_upper

  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    noPeriods = 10, populationOffset = FALSE,
    fitFun = "algo.farrington.fitGLM.flexible",
    b = num_years_total, w = 3, weightsThreshold = 2.58,
    pastWeeksNotIncluded = 26,
    pThresholdTrend = 1, trend = TRUE,
    thresholdMethod = "delta", alpha = alpha
  )

  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases, control)

  pad <- rep(NA, num_weeks_total - number_of_weeks)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)
  expected <- c(pad, results@control$expected)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  data_aggregated$expected <- expected

  return(data_aggregated)
}
