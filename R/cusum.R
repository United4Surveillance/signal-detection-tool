#' Get signals of CUSUM algorithm with reset
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' results <- get_signals_cusum(data_aggregated)
#' }
get_signals_cusum <- function(data_aggregated,
                              number_of_weeks = 52) {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_weeks_for_calibration <- num_weeks_total - number_of_weeks

  if (num_weeks_for_calibration < 0) {
    warning(paste0(
      "The number of weeks you want to generate alarms for (n = ", number_of_weeks, ")",
      " is higher than the number of weeks you have in your data (n = ", num_weeks_total, ")."
    ))
    return(NULL)
  }

  # TODO discuss what the minimum num_weeks_for_calibration should be
  else if (num_weeks_for_calibration == 0) {
    warning(paste0(
      "Your data/stratification covers ",
      num_weeks_total,
      " number of weeks in total and you want to generate alarms for ", number_of_weeks, ". ",
      "Cusum uses 1 week for calibrating an epidemiological baseline. You have ", num_weeks_for_calibration,
      " weeks left in your data/stratification."
    ))
    return(NULL)
  }

  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    k = 1.04,
    h = 2.26,
    m = NULL
  )

  # run CUSUM on data
  results <- cusum_with_reset(sts_cases, control)

  pad <- rep(NA, num_weeks_total - number_of_weeks)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  # this algorithm does not return an expected value
  data_aggregated$expected <- NA

  # recode 0,1 in alarms to TRUE,FALSE
  data_aggregated <- data_aggregated %>%
    dplyr::mutate(alarms = dplyr::case_when(
      alarms == 1 ~ T,
      alarms == 0 ~ F,
      is.na(alarms) ~ NA
    ))

  return(data_aggregated)
}
