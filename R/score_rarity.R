#' @title Score signals based on temporal alarm rarity within strata
#' @description Computes a score for each signal based on the frequency of alarms
#' within the test period of its stratum and category. Higher alarm frequency results in higher scores.
#'
#' @param signal_results, signal detection results obtained from get_signals()
#'
#' @returns signal detection results scored
#' @export
score_rarity<- function(signal_results){

  # # select only the stratified
  # signal_results <- signal_results %>%
  #   dplyr::filter(!is.na(.data$category))

  # select test period per stratum
  ts_pathogen_strat <- signal_results %>%
    dplyr::group_by(.data$category, .data$stratum) %>%
    dplyr::arrange(.data$year, .data$week, .by_group = TRUE) %>%
    dplyr::group_modify(~{
      n <- dplyr::first(.x$number_of_weeks)
      dplyr::slice_tail(.x, n = n)
    }) %>%
    dplyr::ungroup()

  # count alarms and compute individual score for each stratum
  score_stratum <- ts_pathogen_strat %>%
    dplyr::group_by(.data$category, .data$stratum) %>%
    dplyr::summarise(
      n_weeks = dplyr::n(),
      n_alarms = sum(.data$alarms, na.rm = TRUE),
      score_stratum = dplyr::if_else(n_alarms > 0, n_alarms / n_weeks, NA),
      score_stratum = dplyr::if_else(n_alarms == 1, 0, score_stratum)
    ) %>%
    dplyr::ungroup()

  # add row_id
  score_tbl <- signal_results %>%
    dplyr::select(.data$.row_id, .data$category, .data$stratum, .data$alarms) %>%
    dplyr::left_join(score_stratum, by = c("category", "stratum")) %>%
    dplyr::mutate(
      score = dplyr::if_else(.data$alarms %in% TRUE, .data$score_stratum, NA)
    ) %>%
    dplyr::select(.data$.row_id, .data$score)

  return(score_tbl)
}

