#' @title Automated and Early Detection of Disease Outbreaks
#'
#' This function retrieves outbreak signals using the aeddo algorithm. It processes
#' surveillance data, aggregates it based on specified date variables, and applies
#' the aeddo algorithm to detect potential outbreaks.
#'
#' @param surveillance_data The surveillance data containing information about cases.
#' @param date_start A date object or character of format yyyy-mm-dd
#' @param date_end A date object or character of format yyyy-mm-dd
#' @param date_var A character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param population_size The population size for the aeddo algorithm. Default is 1.
#' @inheritParams aeddo::aeddo
#'
#' @return An object containing outbreak signals detected by the aeddo algorithm.
#'
#' @examples
#'
#' @seealso
#' \code{\link{aeddo}} for details on the aeddo algorithm.
#'
#' @references
#' For information on the aeddo algorithm, refer to the package documentation.
get_signals_aeddo <- function(surveillance_data,
                              date_start = NULL,
                              date_end = NULL,
                              date_var = "date_report",
                              population_size = 1,
                              formula = y ~ 1,
                              sig_level = 0.95,
                              exclude_past_outbreaks = TRUE,
                              k = 52*3,
                              init_theta = c(0, 1),
                              lower = c(-Inf, 1e-6),
                              upper = c(Inf, 1e2),
                              method = "L-BFGS-B") {

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

  # Append the 'time' and population size, 'n', for the 'aeddo' algorithm
  data <- data %>%
    dplyr::mutate(week = formatC(week, width = 2, flag = 0)) %>%
    dplyr::mutate(time = ISOweek::ISOweek2date(paste0(year, "-W", week, "-7")),
                  n = population_size) %>%
    dplyr::select("time", y = "cases", "n")

  # Employ the aeddo method to monitor the data
  aeddo_results <- aeddo::aeddo(
    data = data,
    formula = formula,
    k = k,
    sig_level = sig_level,
    exclude_past_outbreaks = exclude_past_outbreaks,
    init_theta = init_theta,
    lower = lower,
    upper = upper,
    method = method)

  pad <- rep(NA, k)
  alarms <- c(pad, aeddo_results$outbreak_alarm)
  upperbound <- c(pad,
                  dgamma(
                    x = sig_level,
                    shape = 1/aeddo_results$phi,
                    scale = aeddo_results$phi))
  ranef <- c(pad, aeddo_results$u)
  expected <- c(pad, aeddo_results$lambda)

  data$alarms <- alarms
  data$upperbound <- upperbound
  data$ranef <- ranef
  data$expected <- expected

  return(data)

}
