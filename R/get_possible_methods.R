#' Determine Possible Outbreak Detection Methods Based on Available Data
#'
#' This function identifies which algorithms can be applied for outbreak detection
#' depending on the number of weeks available for model fitting. The selection is
#' based on the minimum and maximum dates in the dataset and the specified parameters
#' for fitting.
#'
#' The method selection criteria are:
#' - If 4 years (208 weeks) or more of data are available: All  methods are possible.
#' - If at least 3 of historic data are available: All methods except "glm farrington" and "glm farrington with timetrend" are possible.
#' - If at least 2 years of historic data are available: Same methods as above except for "glm harmonic with timetrend".
#' - If at least 26 weeks of historic data are available: Mean, CUSUM and Ears are possible.
#' - If at least 7 weeks of historic data is available: Mean and CUSUM are available.
#' - If at least 1 week of historic data is available: CUSUM is possible.
#' - If no training data is availble `NULL` is returned
#' @param min_date Date, minimum date in the time series used for fitting a model
#' @param max_date Date, maximum date in the time series used for fitting a model
#' @param number_of_weeks Integer, number of weeks to include for model fitting.
#'        Default is \code{6}.
#'
#' @details The function calculates the number of weeks available for fitting and
#' selects appropriate algorithms based on that. The more historical data available,
#' the more complex models can be used.
#'
#' @return A character vector of possible algorithm names. Returns \code{NULL} if
#'         no method is applicable.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(date_report = seq.Date(Sys.Date() - 500, Sys.Date(), by = "week"))
#' get_possible_methods(data)
#' }
#'
#' @seealso \code{\link{available_algorithms}}
#' @export
get_possible_methods <- function(min_date,
                                 max_date,
                                 number_of_weeks = 6) {
  checkmate::check_date(min_date)
  checkmate::check_date(max_date)

  max_date_fit <- max_date - lubridate::weeks(number_of_weeks)
  # we subtract 1 day because otherwise the time difference between the dates is computed and e.g. difftime("2020-01-08","2020-01-01", units = "weeks) gives 1 week we want to count end and start date in as well thus the number of days for this example is 8
  # furthermore result is rounded to the next integer as e.g. 1.3 weeks are 2 weeks in the aggregation
  number_of_weeks_available_fitting <- ceiling(as.numeric(difftime(max_date_fit, min_date - lubridate::days(1), units = "weeks")))

  algos <- available_algorithms()

  if (number_of_weeks_available_fitting >= 4 * 52) {
    # all glm methods
    methods_possible <- algos
  } else if (number_of_weeks_available_fitting >= 3 * 52) {
    # All possible except FN
    not_possible <- c("glm farrington with timetrend", "glm farrington")
    methods_possible <- algos[!algos %in% not_possible]
  } else if (number_of_weeks_available_fitting >= 2 * 52) {
    # All possible except FN and Harmonic with timetrend
    not_possible <- c(
      "glm farrington with timetrend", "glm farrington",
      "glm harmonic with timetrend"
    )
    methods_possible <- algos[!algos %in% not_possible]
  } else if (number_of_weeks_available_fitting >= 26) {
    methods_possible <- algos[c("Mean", "CUSUM", "EARS")]
  } else if (number_of_weeks_available_fitting >= 7) {
    methods_possible <- algos[c("CUSUM", "EARS")]
  } else if (number_of_weeks_available_fitting >= 1) {
    methods_possible <- algos[c("CUSUM")]
  } else {
    methods_possible <- NULL
  }

  methods_possible
}
