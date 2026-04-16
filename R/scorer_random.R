#' Generate random scores for alarm rows
#'
#' Returns a random score between 0 and 1 only for rows where
#' `signal_results$alarms == TRUE`. For all other rows, `score` is `NA_real_`.
#' This function is intended as a minimal example scorer.
#'
#' @param signal_results A data frame or tibble containing `.row_id` and
#'   `alarms`.
#'
#' @return A tibble with columns `.row_id` and `score`.
#'
#' @examples
#' signal_results <- tibble::tibble(
#'   .row_id = 1:4,
#'   alarms = c(TRUE, FALSE, NA, TRUE)
#' )
#'
#' set.seed(123)
#' score_randomly(signal_results)
#'
#' @export
score_randomly <- function(signal_results) {
  if (!".row_id" %in% names(signal_results)) {
    stop("`signal_results` must contain a `.row_id` column.", call. = FALSE)
  }

  if (!"alarms" %in% names(signal_results)) {
    stop("`signal_results` must contain an `alarms` column.", call. = FALSE)
  }

  is_alarm <- signal_results$alarms %in% TRUE

  score <- rep(NA_real_, nrow(signal_results))
  score[is_alarm] <- runif(sum(is_alarm), min = 0, max = 1)

  tibble::tibble(
    .row_id = signal_results$.row_id,
    score = score
  )
}
