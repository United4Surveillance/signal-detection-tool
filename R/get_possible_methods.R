#' Determine Possible Outbreak Detection Methods Based on Available Historic Data
#'
#' This function identifies which algorithms can be applied for outbreak detection
#' depending on the amount of historic data available for model fitting. The
#' decision is based on the minimum and maximum dates of the time series and the
#' number of weeks reserved at the end of the series (e.g. for current detection).
#'
#' Historic data is defined as the period from \code{min_date} to
#' \code{max_date - number_of_weeks}. The number of available weeks is computed
#' on this interval, counting partial weeks as full weeks to match the weekly
#' aggregation used by the algorithms.
#'
#' The method selection criteria are approximately:
#' \itemize{
#'   \item If 4 years (about 208 weeks) or more of historic data are available:
#'         all methods are possible.
#'   \item If 3 to <4 years (about 156 to <208 weeks) of historic data are
#'         available: all methods except \code{"glm farrington"} and
#'         \code{"glm farrington with timetrend"} are possible.
#'   \item If 2 to <3 years (about 104 to <156 weeks) of historic data are
#'         available: all methods except \code{"glm farrington"},
#'         \code{"glm farrington with timetrend"} and
#'         \code{"glm harmonic with timetrend"} are possible.
#'   \item If 26 to <104 weeks of historic data are available:
#'         \code{"Mean"}, \code{"CUSUM"} and \code{"EARS"} are possible.
#'   \item If 7 to <26 weeks of historic data are available:
#'         \code{"Mean"} and \code{"CUSUM"} are possible.
#'   \item If 1 to <7 weeks of historic data are available:
#'         \code{"CUSUM"} is possible.
#'   \item If no training data is available (less than 1 week),
#'         \code{NULL} is returned.
#' }
#'
#' @param min_date A \code{Date} object, the minimum date in the time series
#'   used for fitting a model.
#' @param max_date A \code{Date} object, the maximum date in the time series
#'   used for fitting a model.
#' @param number_of_weeks Integer. Number of weeks at the end of the time series
#'   that are reserved for detection / monitoring and therefore not counted as
#'   historic data for model fitting. Default is \code{6}.
#'
#' @details
#' The function computes
#' \code{max_date_fit = max_date - number_of_weeks} and then calculates the
#' number of weeks between \code{min_date} and \code{max_date_fit}. Partial
#' weeks are counted as full weeks to align with the weekly aggregation of the
#' data. Based on this number of historic weeks, suitable algorithms are
#' selected using \code{\link{available_algorithms}}.
#'
#' @return A character vector of possible algorithm names. Returns \code{NULL}
#'   if no method is applicable.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   date_report = seq.Date(Sys.Date() - 500, Sys.Date(), by = "day")
#' )
#'
#' mm <- get_min_max_date(data, date_var = "date_report")
#'
#' get_possible_methods(
#'   min_date = mm$min_date,
#'   max_date = mm$max_date,
#'   number_of_weeks = 6
#' )
#' }
#'
#' @seealso \code{\link{available_algorithms}}, \code{\link{get_min_max_date}}
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
