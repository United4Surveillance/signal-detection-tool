#' @title Automated and Early Detection of Disease Outbreaks
#'
#' @description
#' This function retrieves outbreak signals using the aeddo algorithm. It processes
#' surveillance data, aggregates it based on specified date variables, and applies
#' the aeddo algorithm to detect potential outbreaks.
#'
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying the number of weeks to generate alarms for
#' @param population_size The population size for the aeddo algorithm. Default is 1.
#' @inheritParams aeddo::aeddo
#'
#' @return An object containing outbreak signals detected by the aeddo algorithm.
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#' preprocess_data() %>%
#' aggregate_data() %>%
#' add_rows_missing_dates()
#' results <- get_signals_aeddo(data_aggregated)
#' }
#'
#' @seealso
#' \code{\link{aeddo}} for details on the aeddo algorithm.
#'
#' @references
#' For information on the aeddo algorithm, refer to the package documentation.
#' @importFrom rlang .data
get_signals_aeddo <- function(data_aggregated,
                              number_of_weeks = 52,
                              population_size = 1,
                              sig_level = 0.95,
                              exclude_past_outbreaks = TRUE,
                              k = 52*3,
                              init_theta = c(rep(0, 4), 1),
                              lower = c(-1, -0.01, -0.8, -0.8, -6),
                              upper = c(1e2, 0.5, 1, 1, 1e2),
                              method = "L-BFGS-B") {


  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  # Define the formula for the fixed effects
  fixed_effects_formula <- stats::as.formula(
    paste0("y ~ 1 + t + sin(2*pi*w/",
           number_of_weeks,
           ") + cos(2*pi*w/",
           number_of_weeks, ")"))

  # Append the 'time' and population size, 'n', for the 'aeddo' algorithm
  data_aggregated <- data_aggregated %>%
    dplyr::mutate(week = formatC(.data$week, width = 2, flag = 0)) %>%
    dplyr::mutate(time = ISOweek::ISOweek2date(
      paste0(.data$year, "-W", .data$week, "-7")),
      n = population_size,
      t = dplyr::row_number(),
      w = as.integer(.data$week)) %>%
    dplyr::rename(y = "cases")

  # Employ the aeddo method to monitor the data
  aeddo_results <- aeddo::aeddo(
    data = data_aggregated,
    formula = fixed_effects_formula,
    k = k,
    sig_level = sig_level,
    exclude_past_outbreaks = exclude_past_outbreaks,
    init_theta = init_theta,
    lower = lower,
    upper = upper,
    method = method)

  # Collect the results
  pad <- rep(NA, k)
  alarms <- c(pad, aeddo_results$outbreak_alarm)
  upperbound <- c(
    pad,
    stats::dgamma(
      x = sig_level,
      shape = 1/aeddo_results$phi,
      scale = aeddo_results$phi))
  ranef <- c(pad, aeddo_results$u)
  expected <- c(pad, aeddo_results$lambda)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  data_aggregated$ranef <- ranef
  data_aggregated$expected <- expected

  return(data_aggregated)

}
