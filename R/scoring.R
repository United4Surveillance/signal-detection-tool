#' Validate the output of a scorer function
#'
#' Ensures that a scorer returns a data frame with exactly two columns:
#' `.row_id` and `score`. Also checks that the output has the correct number
#' of rows, contains the same row identifiers as the input, and that `score`
#' is numeric, non-missing, and bounded in the interval `[0, 1]`.
#'
#' @param score_tbl A data frame returned by a scorer function.
#' @param signal_results The input data frame passed to the scorer, including `.row_id`.
#' @param scorer_name A character string used for informative error messages.
#'
#' @return The scorer output invisibly.
#' @noRd
validate_scorer_output <- function(score_tbl, signal_results, scorer_name = "<unknown>") {
  if (!is.data.frame(score_tbl)) {
    stop(sprintf("Scorer '%s' must return a data.frame or tibble.", scorer_name),
         call. = FALSE)
  }

  expected_names <- c(".row_id", "score")

  if (!identical(names(score_tbl), expected_names)) {
    stop(
      sprintf(
        "Scorer '%s' must return exactly the columns `.row_id` and `score`.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  if (nrow(score_tbl) != nrow(signal_results)) {
    stop(
      sprintf(
        "Scorer '%s' returned %d rows instead of %d.",
        scorer_name, nrow(score_tbl), nrow(signal_results)
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(score_tbl$.row_id)) {
    stop(sprintf("Scorer '%s' contains duplicated `.row_id` values.", scorer_name),
         call. = FALSE)
  }

  if (!setequal(score_tbl$.row_id, signal_results$.row_id)) {
    stop(
      sprintf(
        "Scorer '%s' does not contain the same `.row_id` values as `signal_results`.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(score_tbl$score)) {
    stop(sprintf("Scorer '%s': `score` must be numeric.", scorer_name),
         call. = FALSE)
  }

  if (anyNA(score_tbl$score)) {
    stop(sprintf("Scorer '%s': `score` must not contain missing values.", scorer_name),
         call. = FALSE)
  }

  if (any(score_tbl$score < 0 | score_tbl$score > 1)) {
    stop(sprintf("Scorer '%s': `score` must be in the interval [0, 1].", scorer_name),
         call. = FALSE)
  }

  invisible(score_tbl)
}

#' Aggregate multiple score columns row-wise
#'
#' Aggregates individual scorer outputs into one final score per row.
#' Supported aggregation methods are `"mean"` and `"sum"`.
#'
#' @param score_df A data frame containing `.row_id` and one or more numeric
#'   score columns.
#' @param aggregation A character string specifying the aggregation method.
#'   Must be either `"mean"` or `"sum"`.
#'
#' @return A numeric vector containing one aggregated score per row.
#' @noRd
aggregate_scores <- function(score_df, aggregation = c("mean", "sum")) {
  aggregation <- match.arg(aggregation)

  score_cols <- setdiff(names(score_df), ".row_id")

  if (length(score_cols) == 0L) {
    stop("No individual scores were found for aggregation.", call. = FALSE)
  }

  score_matrix <- score_df %>%
    dplyr::select(dplyr::all_of(score_cols)) %>%
    as.matrix()

  switch(
    aggregation,
    mean = rowMeans(score_matrix),
    sum  = rowSums(score_matrix)
  )
}

#' Calculate aggregated scores for epidemiological signals
#'
#' Adds an internal row identifier, applies multiple scorer functions to the
#' input data, validates each scorer output, joins the resulting individual
#' scores by `.row_id`, and returns the original input data with one final
#' aggregated `score` column.
#'
#' Each scorer must accept `signal_results` including the `.row_id` column and
#' must return a tibble or data frame with exactly two columns:
#' `.row_id` and `score`.
#'
#' @param signal_results A data frame or tibble containing the input signals.
#' @param scorers A named list of scorer functions.
#' @param aggregation A character string specifying how individual scores should
#'   be aggregated. Must be either `"mean"` or `"sum"`.
#'
#' @return A tibble containing the original `signal_results` columns plus one
#'   additional aggregated `score` column.
#'
#' @examples
#' signal_results <- tibble::tibble(
#'   signal_id = 1:5,
#'   cases = c(5, 20, 50, 10, 100),
#'   p_value = c(0.80, 0.30, 0.01, 0.60, 0.05)
#' )
#'
#' scorers <- list(
#'   random = score_randomly
#' )
#'
#' get_scores(
#'   signal_results = signal_results,
#'   scorers = scorers,
#'   aggregation = "mean"
#' )
#'
#' @export
get_scores <- function(signal_results, scorers, aggregation = c("mean", "sum")) {
  aggregation <- match.arg(aggregation)

  if (!is.list(scorers) || length(scorers) == 0L) {
    stop("`scorers` must be a non-empty list of functions.", call. = FALSE)
  }

  if (!all(purrr::map_lgl(scorers, is.function))) {
    stop("All elements in `scorers` must be functions.", call. = FALSE)
  }

  if (is.null(names(scorers)) || any(names(scorers) == "")) {
    names(scorers) <- paste0("scorer_", seq_along(scorers))
  }

  signal_results_with_id <- signal_results %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  score_tables <- purrr::imap(
    scorers,
    function(scorer_fun, scorer_name) {
      score_tbl <- scorer_fun(signal_results_with_id)

      validate_scorer_output(
        score_tbl = score_tbl,
        signal_results = signal_results_with_id,
        scorer_name = scorer_name
      )

      score_tbl %>%
        dplyr::rename(!!scorer_name := score)
    }
  )

  all_scores <- purrr::reduce(score_tables, dplyr::full_join, by = ".row_id")

  aggregated_score <- aggregate_scores(
    score_df = all_scores,
    aggregation = aggregation
  )

  signal_results_with_id %>%
    dplyr::left_join(
      tibble::tibble(
        .row_id = all_scores$.row_id,
        score = aggregated_score
      ),
      by = ".row_id"
    ) %>%
    dplyr::select(-.row_id)
}
