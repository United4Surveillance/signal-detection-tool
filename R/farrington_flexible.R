#' Get signals of surveillance's farringtonFlexible algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_time_units integer, specifying number of time units to generate signals for. The default is weeks and should not be changed.
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
                                           number_of_time_units = 52,
                                           alpha_upper = 0.05) {
  checkmate::assert(
    checkmate::check_integerish(number_of_time_units)
  )
  checkmate::assert(
    checkmate::check_number(alpha_upper, lower = 0.001, upper = 0.2)
  )

  if (!"week" %in% names(data_aggregated)) {
    stop(
      "The algorithm selected requires that the case aggregation is performed on consecutive weeks.",
      call. = FALSE
    )
  }

  invalid <- with(
    data_aggregated,
    year == dplyr::lag(year) & week != dplyr::lag(week) + 1L
  )

  if (any(invalid, na.rm = TRUE)) {
    stop(
      "The algorithm selected requires that the case aggregation is performed on consecutive weeks.",
      call. = FALSE
    )
  }

  sts_cases <- convert_to_sts(data_aggregated, time_unit = "weekly")

  num_time_units_total <- length(sts_cases@observed)
  num_time_units_for_calibration <- num_time_units_total - number_of_time_units
  num_years_total <- floor((num_time_units_for_calibration - 26) / 52)

  if (num_time_units_for_calibration < 0) {
    warning(paste0(
      "The number of time units you want to generate alarms for (n = ", number_of_time_units, ")",
      " is higher than the number of time units you have in your data (n = ", num_time_units_total, ")."
    ))
    return(NULL)
  } else if (num_time_units_for_calibration < 52 + 26) {
    warning(paste0(
      "Your data/stratification covers ",
      num_time_units_total,
      " number of weeks in total and you want to generate alarms for ", number_of_time_units, ". ",
      "FarringtonFlexible needs at least 78 weeks (1.5 years) of data to calibrate an epidemiological baseline. ",
      "You have ", num_time_units_for_calibration, " weeks for calibration left in your data/stratification."
    ))
    return(NULL)
  }

  alpha <- alpha_upper

  control <- list(
    range = ((num_time_units_total - number_of_time_units + 1):num_time_units_total),
    noPeriods = 10, populationOffset = FALSE,
    fitFun = "algo.farrington.fitGLM.flexible",
    b = num_years_total, w = 3, weightsThreshold = 2.58,
    pastWeeksNotIncluded = 26,
    pThresholdTrend = 1, trend = TRUE,
    thresholdMethod = "delta", alpha = alpha
  )

  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases, control)

  pad <- rep(NA, num_time_units_total - number_of_time_units)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)
  expected <- c(pad, results@control$expected)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  data_aggregated$expected <- expected

  return(data_aggregated)
}
