#' @title Score signals according to size strength
#' @description Scores each signal in the signals_res object using the signal size,
#' defined by the distance from expected value and upperbound threshold value
#' the score follows the formula (size^3-1)/size^3 and then is rounded to the 2nd decimal point.
#' For EARS and CUSUM method, the function returns a score of NA.
#'
#' @param signals_res dataframe with the results of signal detection
#'
#' @returns strength scores for signals
#' @export
#'
#' @examples \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals_all() %>%
#'   dplyr::mutate(pathogen = "Pertussis")
#'
#' get_scores(signals, scorers = list(strength = score_strength))
#' }
score_strength <- function(signals_res) {
  method <- unique(signals_res$method)

  if (method %in% c("EARS", "CUSUM")) {
    # strength score not possible with EARS or CUSUM as they don't return an expected value
    signals_res <- signals_res %>%
      dplyr::mutate(
        size.alarm = NA_real_,
        score = NA_real_
      )
  } else {
    signals_res <- signals_res %>%
      dplyr::mutate(
        size.alarm = dplyr::case_when(
          .data$alarms ~ abs((.data$cases - .data$expected) / (.data$upperbound - .data$expected)),
          .default = NA_real_
        )
      )

    signals_res <- signals_res %>%
      dplyr::mutate(
        score = dplyr::case_when(
          .data$alarms ~ round(((.data$size.alarm^3) - 1) / (.data$size.alarm^3), digits = 2),
          .default = NA_real_
        )
      )
  }


  return(signals_res %>% dplyr::select(c(".row_id", "score")))
}
