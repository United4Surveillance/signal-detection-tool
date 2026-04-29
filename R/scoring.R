#' Validate the output of a scorer function
#'
#' Ensures that a scorer returns a data frame with exactly two columns:
#' `.row_id` and `score`. Also checks that the output has the correct number
#' of rows and contains the same row identifiers as the input.
#'
#' Scorers must return numeric scores in the interval `[0, 1]` only for rows
#' where `signal_results$alarms == TRUE`. For all other rows, `score` must be
#' `NA`.
#'
#' Missing scores must be represented as `NA`, not `NaN`.
#'
#' @param score_tbl A data frame returned by a scorer function.
#' @param signal_results The input data frame passed to the scorer, including
#'   `.row_id` and `alarms`.
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

  if (!"alarms" %in% names(signal_results)) {
    stop(
      sprintf(
        "Scorer '%s': `signal_results` must contain an `alarms` column.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(score_tbl$score)) {
    stop(sprintf("Scorer '%s': `score` must be numeric.", scorer_name),
         call. = FALSE)
  }

  if (any(is.nan(score_tbl$score))) {
    stop(
      sprintf(
        "Scorer '%s': `score` must use `NA` for missing values, not `NaN`.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  is_alarm <- signal_results$alarms[match(score_tbl$.row_id, signal_results$.row_id)] %in% TRUE

  if (any(!is.na(score_tbl$score[!is_alarm]))) {
    stop(
      sprintf(
        "Scorer '%s': `score` must be `NA` when `alarms` is not TRUE.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  if (anyNA(score_tbl$score[is_alarm])) {
    stop(
      sprintf(
        "Scorer '%s': `score` must not be missing when `alarms` is TRUE.",
        scorer_name
      ),
      call. = FALSE
    )
  }

  if (any(score_tbl$score[is_alarm] < 0 | score_tbl$score[is_alarm] > 1)) {
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
#' Missing values in individual score columns are ignored during aggregation.
#' Therefore, rows with at least one non-missing individual score receive an
#' aggregated numeric value.
#'
#' Rows for which all individual scorer outputs are `NA` remain `NA` in the
#' aggregated result. This avoids returning `NaN` for row-wise means and `0`
#' for row-wise sums when no observed score is available.
#'
#' @param score_df A data frame containing `.row_id` and one or more numeric
#'   score columns.
#' @param aggregation A character string specifying the aggregation method.
#'   Must be either `"mean"` or `"sum"`.
#'
#' @return A double vector containing one aggregated score per row. Rows with
#'   no non-missing individual score receive `NA_real_`.
#' @noRd
aggregate_scores <- function(score_df, aggregation = c("mean", "sum")) {
  aggregation <- match.arg(aggregation)

  score_cols <- setdiff(names(score_df), ".row_id")

  if (length(score_cols) == 0L) {
    stop("No individual scores were found for aggregation.", call. = FALSE)
  }

  if (!all(vapply(score_df[score_cols], is.numeric, logical(1)))) {
    stop("All individual score columns must be numeric.", call. = FALSE)
  }

  score_matrix <- score_df %>%
    dplyr::select(dplyr::all_of(score_cols)) %>%
    as.matrix()

  n_observed_scores <- rowSums(!is.na(score_matrix))

  aggregated_score <- switch(
    aggregation,
    mean = rowMeans(score_matrix, na.rm = TRUE),
    sum  = rowSums(score_matrix, na.rm = TRUE)
  )

  aggregated_score[n_observed_scores == 0L] <- NA_real_

  as.double(aggregated_score)
}

#' Calculate aggregated scores for epidemiological signals
#'
#' Adds an internal row identifier, applies multiple scorer functions to the
#' input data, validates each scorer output, joins the resulting individual
#' scores by `.row_id`, and returns the original input data with one final
#' aggregated `score` column.
#'
#' Each scorer must accept `signal_results` including the `.row_id` and
#' `alarms` columns and must return a tibble or data frame with exactly two
#' columns: `.row_id` and `score`.
#'
#' Scores must be numeric in `[0, 1]` only for rows where `alarms == TRUE`.
#' For all other rows, scorers must return `NA`. Missing scores must be encoded
#' as `NA`, not `NaN`.
#'
#' During aggregation, missing individual scores are ignored. Rows for which all
#' individual scores are missing receive `NA_real_` as their final aggregated
#' score.
#'
#' @param signal_results A data frame or tibble containing the input signals,
#'   including an `alarms` column.
#' @param scorers A named list of scorer functions.
#' @param aggregation A character string specifying how individual scores should
#'   be aggregated. Must be either `"mean"` or `"sum"`.
#'
#' @return A tibble containing the original `signal_results` columns plus one
#'   additional aggregated `score` column. Rows with no non-missing individual
#'   score receive `NA_real_`.
#'
#' @examples
#' signal_results <- tibble::tibble(
#'   signal_id = 1:5,
#'   alarms = c(FALSE, TRUE, TRUE, FALSE, TRUE),
#'   cases = c(5, 20, 50, 10, 100),
#'   p_value = c(0.80, 0.30, 0.01, 0.60, 0.05)
#' )
#'
#' set.seed(123)
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

  if (!"alarms" %in% names(signal_results)) {
    stop("`signal_results` must contain an `alarms` column.", call. = FALSE)
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

  all_scores <- purrr::reduce(score_tables, dplyr::full_join, by = ".row_id") %>%
    dplyr::arrange(.row_id)

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
    dplyr::left_join(all_scores,
                     by = ".row_id") %>%
    dplyr::select(-.row_id)
}
