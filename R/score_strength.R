#' @title Score signals according to size strength
#' @description Scores each signal in the signals_res object using the signal size,
#' defined by the distance from expected value and upperbound threshold value
#' the score follows the formula (size^3-1)/size^3. For EARS and CUSOUM method, the 
#' function returns a score of NA
#'
#' @param signals_res dataframe with the results of signal detection
#'
#' @returns strength scores for signals
#' @export
score_strength <- function(signals_res){

  method <- unique(signals_res$method)

  if(method %in% c("EARS", "CUSUM")){
    # strength score not possible with EARS or CUSUM as they don't return an expected value
    signals_res <- signals_res %>% 
      dplyr::mutate(
        size.alarm = NA,
        score = NA
      )
  } else {
    signals_res <- signals_res %>%
      dplyr::mutate(
        size.alarm = dplyr::case_when(
          .data$alarms ~ (.data$cases-.data$expected)/(.data$upperbound-.data$expected),
          .default = NA
        )
      ) 
      
      signals_res <- signals_res %>%
        dplyr::mutate(
      score = dplyr::case_when(
        .data$alarms ~ ((.data$size.alarm^3) - 1)/(.data$size.alarm^3),
        .default = NA
      )
    )
  }
  

  return(signals_res %>% dplyr::select(c(".row_id", "score")))
}
