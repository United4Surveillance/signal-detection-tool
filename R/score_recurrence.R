#' @title Score signals based on temporal alarm recurrence within strata
#' @description Computes a score for each signal based on the frequency of alarms
#' within the test period of its stratum and category. Higher alarm frequency results in higher scores.
#' The score is set to `NA` when recurrence cannot be assessed, such as when the test period consists of only one week.
#' A single alarm across multiple observed time units is assigned a score of 0, because it does not indicate recurrence.
#' If more than one alarm occurs, the score is calculated as `n_alarms / n_time_units`.
#'
#' @param signal_results signal detection results obtained from get_signals()
#'
#' @returns signal detection results scored
#' @export
#'
#' @examples \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals_all() %>%
#'   dplyr::mutate(pathogen = "Pertussis")
#'
#' get_scores(signals, scorers = list(recurrence = score_recurrence))
#' }
score_recurrence <- function(signal_results) {

  time_unit <- unique(signal_results$time_unit)

  if (all(time_unit %in% c("weekly", "biweekly"))) {
    time_column <- "week"
  } else if (all(time_unit == "monthly")) {
    time_column <- "month"
  } else {
    stop(
      "`signal_results$time_unit` must contain either weekly/biweekly ",
      "values or monthly values, but not a mixture."
    )
  }

  # select test period per stratum
  ts_pathogen_strat <- signal_results %>%
    dplyr::group_by(.data$category, .data$stratum) %>%
    dplyr::arrange(.data$year, .data[[time_column]], .by_group = TRUE) %>%
    dplyr::group_modify(~ {
      n <- dplyr::first(.x$number_of_time_units)
      dplyr::slice_tail(.x, n = n)
    }) %>%
    dplyr::ungroup()

  # count alarms and compute individual score for each stratum
  score_stratum <- ts_pathogen_strat %>%
    dplyr::group_by(.data$category, .data$stratum) %>%
    dplyr::summarise(
      n_time_units = dplyr::n(),
      n_alarms = sum(.data$alarms, na.rm = TRUE),
      score_stratum = dplyr::if_else(n_alarms > 0, n_alarms / n_time_units, NA),
      score_stratum = dplyr::if_else(n_alarms == 1, 0, score_stratum),
      score_stratum = dplyr::if_else(n_time_units == 1, NA, score_stratum)
    ) %>%
    dplyr::ungroup()

  # add row_id
  score_tbl <- signal_results %>%
    dplyr::select(".row_id", "category", "stratum", "alarms") %>%
    dplyr::left_join(score_stratum, by = c("category", "stratum")) %>%
    dplyr::mutate(
      score = dplyr::if_else(.data$alarms %in% TRUE, .data$score_stratum, NA)
    ) %>%
    dplyr::select(".row_id", "score")

  return(score_tbl)
}
