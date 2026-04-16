# returns vector of length nrow(signal_results)
score_randomly_vec <- function(signal_results) {
  runif(nrow(signal_results), min = 0, max = 1)
}
# returns tibble of shape nrow(signal_results),2 with columns .row_id, score
score_randomly <- function(signal_results){
  if (!".row_id" %in% colnames(signal_results)) {
    stop(sprintf(
      "Die Spalte `.row_id` existiert nicht."
    ), call. = FALSE)
  }

  signal_results %>%
    dplyr::mutate(score = runif(nrow(signal_results), min = 0, max = 1)) %>%
    dplyr::select(c(.row_id, score))

}
