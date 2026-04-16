#' Generate a random score
#'
#' Returns a random score between 0 and 1 for each row in `signal_results`.
#' This function is intended as a minimal example scorer.
#'
#' @param signal_results A data frame or tibble containing `.row_id`.
#'
#' @return A tibble with columns `.row_id` and `score`.
#'
#' @examples
#' score_randomly(tibble::tibble(.row_id = 1:3))
#'
#' @export
score_randomly <- function(signal_results) {
  tibble::tibble(
    .row_id = signal_results$.row_id,
    score = runif(nrow(signal_results), min = 0, max = 1)
  )
}
