#' Get signals of surveillance's farringtonFlexible algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#' preprocess_data() %>%
#' aggregate_data() %>%
#' add_rows_missing_dates()
#' results <- get_signals_farringtonflexible(data_aggregated)
#' }
get_signals_farringtonflexible <- function(data_aggregated,
                                           number_of_weeks = 52) {

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_years_total <- floor((num_weeks_total - 26) / 52) - 1

  if (num_years_total <= 0) {
    warning(paste0(
      "Your input data/stratification only stretches over ", num_weeks_total,
      " weeks. FarringtonFlexible needs at least a year of data to calibrate an ",
      " epidemiological baseline."
    ))
    return(NULL)
  }

  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    noPeriods = 10, populationOffset = FALSE,
    fitFun = "algo.farrington.fitGLM.flexible",
    b = num_years_total, w = 3, weightsThreshold = 2.58,
    pastWeeksNotIncluded = 26,
    pThresholdTrend = 1, trend = TRUE,
    thresholdMethod = "delta", alpha = 0.1
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
