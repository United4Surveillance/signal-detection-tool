#' Get Signals
#'
#' This function analyzes surveillance data to detect signals using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param method The method to use for signal detection (currently supports "farrington").
#' @param stratification A character vector specifying the columns to stratify the analysis. Default is NULL.
#'
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' data <- read.csv("../data/input/input.csv")
#' # results <- get_signals(data)
#' results <- get_signals(data, method = "farrington", stratification = c("county", "sex"))
get_signals <- function(data, method = "farrington", stratification = NULL) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = c("farrington"))
  )
  checkmate::assert(
    checkmate::check_null(stratification),
    checkmate::check_vector(stratification),
    combine = "or"
  )

  if (method == "farrington") {
    fun <- get_signals_farringtonflexible
  }

  if (is.null(stratification)) {
    results <- fun(data) %>% dplyr::mutate(category = NA, stratum = NA)
  } else {
    results <- get_signals_stratified(data, fun, stratification)
  }

  return(results)
}
