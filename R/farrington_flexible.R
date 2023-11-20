#' Get signals of surveillance's farringtonFlexible algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#' preprocess_data() %>%
#' aggregate_data() %>%
#' add_rows_missing_dates()
#' results <- get_signals_farringtonflexible(data_aggregated)
#' }
get_signals_farringtonflexible <- function(data_aggregated) {

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks <- length(sts_cases@observed)
  num_years <- floor((num_weeks - 26) / 52) - 1

  if (num_years <= 0) {
    warning(paste0(
      "Your input data/stratification only stretches over ", num_weeks,
      " weeks. FarringtonFlexible needs at least a year of data to calibrate an ",
      " epidemiological baseline."
    ))
    return(NULL)
  }

  control <- list(
    range = ((num_weeks - 51):num_weeks),
    noPeriods = 10, populationOffset = FALSE,
    fitFun = "algo.farrington.fitGLM.flexible",
    b = num_years, w = 3, weightsThreshold = 2.58,
    pastWeeksNotIncluded = 26,
    pThresholdTrend = 1, trend = TRUE,
    thresholdMethod = "delta", alpha = 0.1
  )

  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases, control)

  pad <- rep(NA, num_weeks - 52)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)
  expected <- c(pad, results@control$expected)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  data_aggregated$expected <- expected

  return(data_aggregated)
}
