#' @title Score signals according to specificity of signal strength within category
#' @description Scores each signal in the signal_results object according to the
#' fraction of other alarms that are stronger in the category in the same week
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
#' score_specififcity_stronger(signals)
#' }
score_specificity_stronger <- function(signal_results) {
  signal_scores <- signal_results %>%
    filter(!is.na(expected)) %>%
    group_by(category, year, week) %>%
    mutate(
      num_other_cat = n() - 1,
      num_stronger_cat = sapply(score, function(x) sum(score > x, na.rm = TRUE)),
      score = ifelse(
        num_other_cat == 0,
        1,
        1 - (num_stronger_cat / num_other_cat)
      )
    ) %>%
    ungroup()

  signal_results <- left_join(signal_results, signal_scores) %>% mutate(score = if_else(alarms, score, NA))

  signal_results %>% dplyr::select(.row_id, score)
}

#' @title Score signals according to specificity of signal within category
#' @description Scores each signal in the signal_results object according to the
#' fraction of other alarms in the category in the same week
#'
#' @param signal_results, signal detection results obtained from get_signals_all()
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
#' score_specififcity_alarm(signals)
#' }
score_specificity_alarm <- function(signal_results) {
  signal_scores <- signal_results %>%
    filter(!is.na(expected)) %>%
    # in category
    group_by(category, year, week) %>%
    mutate(
      n_strata_cat = n(),
      n_alarms_cat = sum(alarms, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      num_other_strata_cat = n_strata_cat - 1,
      num_other_alarms_cat = pmax(n_alarms_cat - alarms, 0),
      score = if_else(
        num_other_strata_cat == 0,
        1,
        1 - (num_other_alarms_cat / num_other_strata_cat)
      )
    )

  signal_results <- left_join(signal_results, signal_scores) %>% mutate(score = if_else(alarms, score, NA))

  signal_results %>% dplyr::select(.row_id, score)
}
