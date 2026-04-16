#' @title Score signals according to size strength
#' @description Scores each signal in the signals_res object using the signal size,
#' defined by the distance from expected value and upperbound threhold value
#' the score follows the formula (1-size)/size
#'
#' @param signals_res
#'
#' @returns strength scores for signals
#' @export
score_strength <- function(signals_res){

  method <- unique(signals_res$method)
  possible_algorithms <- available_algorithms()[!names(available_algorithms()) %in% c("EARS", "CUSUM")]

  checkmate::assert_choice(method, possible_algorithms)

  signals_res <- signals_res %>%
    dplyr::mutate(size.alarm = (.data$cases-.data$expected)/(.data$upperbound-.data$expected))

  signals_res <- signals_res %>%
    dplyr::mutate(
      score = dplyr::case_when(
        is.na(.data$alarms) ~ NA,
        !alarms ~ NA,
        .default = (.data$size.alarm - 1)/.data$size.alarm
      )
    )

  return(signals_res %>% dplyr::select(c(".row_id", "score")))
}
