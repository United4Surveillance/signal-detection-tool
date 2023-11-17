#' Get signals of surveillance's farringtonFlexible algorithm
#' @param surveillance_data data frame to be converged
#' @param date_start A date object or character of format yyyy-mm-dd
#' @param date_end A date object or character of format yyyy-mm-dd
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' results <- get_signals_farringtonflexible(data)
#' }
get_signals_farringtonflexible <- function(surveillance_data,
                                           date_start=NULL,
                                           date_end=NULL,
                                           date_var = "date_report") {


  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  data <- preprocess_data(surveillance_data) %>%
    aggregate_data(date_var) %>%
    add_rows_missing_dates(date_start, date_end)

  sts_cases <- convert_to_sts(data)

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

  data$alarms <- alarms
  data$upperbound <- upperbound
  data$expected <- expected

  return(data)
}
