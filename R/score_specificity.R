#' @title Score signals according to specificity of signal strength within category
#' @description Scores each signal in the signal_results object according to the
#' fraction of other alarms that are stronger in the category in the same time unit
#'
#' @param signal_results signal detection results obtained from get_signals_all()
#'
#' @returns signal detection results scored
score_specificity_stronger <- function(signal_results) {
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

  signal_scores <- signal_results %>%
    dplyr::filter(!is.na(alarms)) %>%
    dplyr::group_by(category, year, .data[[time_column]]) %>%
    dplyr::mutate(
      num_other_cat = dplyr::n() - 1,
      num_stronger_cat = sapply(score, function(x) sum(score > x, na.rm = TRUE)),
      score = ifelse(
        num_other_cat == 0,
        1,
        1 - (num_stronger_cat / num_other_cat)
      )
    ) %>%
    dplyr::ungroup()

  signal_results <- dplyr::left_join(signal_results,
    signal_scores %>% dplyr::select(.row_id, score),
    by = ".row_id"
  ) %>%
    dplyr::mutate(score = dplyr::if_else(alarms, score, NA))

  signal_results %>% dplyr::select(.row_id, score)
}

#' @title Score signals according to specificity of signal within category
#' @description Scores each signal in the signal_results object according to the
#' fraction of other alarms in the category in the same time unit
#'
#' @param signal_results signal detection results obtained from get_signals_all()
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
#' get_scores(signals, scorers = list(specificity = score_specificity_alarm))
#' }
score_specificity_alarm <- function(signal_results) {
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

  signal_scores <- signal_results %>%
    dplyr::filter(!is.na(alarms)) %>%
    # in category
    dplyr::group_by(category, year, .data[[time_column]]) %>%
    dplyr::mutate(
      n_strata_cat = dplyr::n(),
      n_alarms_cat = sum(alarms, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      num_other_strata_cat = n_strata_cat - 1,
      num_other_alarms_cat = pmax(n_alarms_cat - alarms, 0),
      score = dplyr::if_else(
        num_other_strata_cat == 0,
        1,
        1 - (num_other_alarms_cat / num_other_strata_cat)
      )
    )

  signal_results <- dplyr::left_join(signal_results,
    signal_scores %>% dplyr::select(.row_id, score),
    by = ".row_id"
  ) %>%
    dplyr::mutate(score = dplyr::if_else(alarms, score, NA))

  signal_results %>% dplyr::select(.row_id, score)
}
