#' Determine Possible Outbreak Detection Methods Based on Available Data
#'
#' This function identifies which algorithms can be applied for outbreak detection
#' depending on the number of weeks available for model fitting. The selection is
#' based on the minimum and maximum dates in the dataset and the specified parameters
#' for fitting and excluded past weeks.
#'
#' The method selection criteria are:
#' - If 4 years (208 weeks) or more of data are available: All  methods are possible.
#' - If 3 to 4 years (156 to 208 weeks) of data are available: All methods except "glm farrington" and "glm farrington with timetrend" are possible.
#' - If 2 to 3 years (104 to 156 weeks) of data are available: Same methods as above except for "glm harmonic with timetrend".
#' - If 1.5 to 2 years (78 to 104 weeks) of data are available: Mean, CUSUM, Ears and FlexibleFarrington are possible.
#' - If 26 weeks to 1.5 years (26 to 78 weeks) of data are available: Mean, CUSUM and Ears are possible.
#' - If 7 weeks to 26 weeks of data is available: Mean and CUSUM are available.
#' - If 1 week to 7 weeks are available: CUSUM is available.
#' - If no training data is availble `NULL` is returned
#'
#' @param data A \code{data.frame} containing the case linelist.
#' @param date_var A character string specifying the name of the date variable in \code{data}.
#'        Default is \code{"date_report"}.
#' @param number_of_weeks Integer, number of weeks to include for model fitting.
#'        Default is \code{6}.
#' @param past_weeks_not_included Integer, number of recent weeks to exclude from fitting.
#'        Default is \code{4}.
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

get_possible_methods <- function(data,
                                 date_var = "date_report",
                                 number_of_weeks = 6,
                                 past_weeks_not_included = 4) {

  min_date <- min(data[[date_var]], na.rm = TRUE)
  max_date <- max(data[[date_var]], na.rm = TRUE)
  max_date_fit <- max_date - lubridate::weeks(number_of_weeks + past_weeks_not_included)
  number_of_weeks_available_fitting <- as.numeric(difftime(max_date_fit, min_date, units = "weeks"))

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
    not_possible <- c("glm farrington with timetrend", "glm farrington",
                      "glm harmonic with timetrend")
    methods_possible <- algos[!algos %in% not_possible]
  } else if (number_of_weeks_available_fitting >= (52 + 26)) {
    methods_possible <- algos[c("Mean", "CUSUM", "EARS", "FarringtonFlexible")]
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
