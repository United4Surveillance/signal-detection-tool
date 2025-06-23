#' Save signals
#'
#' Save the processed results to a CSV file and return codes and messages for application use.
#'
#' @param signals The processed results data frame.
#' @param original_input_data The original input data used for analysis.
#' @param filepath The optional filepath to save the results. If not provided, a filename is generated.
#'
#' @return A list containing success status (TRUE or FALSE) and a message (NULL for success, a warning, or an error).
#'
#' @examples
#' # Save signals with default or custom filepath
#' \dontrun{
#' data <- preprocess_data(SignalDetectionTool::input_example)
#' save_signals(SignalDetectionTool::get_signals(data), data)
#' }
#'
#' @export
save_signals <- function(signals, original_input_data, filepath = "") {
  # get last day of week
  # renaming and reorganization of output is kept here for future use, once we change signal output
  to_save <- signals %>%
    dplyr::mutate(date = lubridate::ceiling_date(lubridate::ymd(paste0(signals$year, "-01-01")) + lubridate::weeks(signals$week), "week") - 1) %>%
    dplyr::filter(!is.na(alarms)) # %>%
  #   dplyr::rename(
  #     outbreak_status = "alarms",
  #     count_cases = "cases"
  #   ) %>%
  #   dplyr::mutate(expected = ifelse(is.na(expected), 0, expected)) %>%
  #   dplyr::mutate(count_outbreak_cases = outbreak_status * (count_cases - floor(upperbound))) # %>%
  #
  # # list of columns to reconstruct
  # reconstruct_columns <- SignalDetectionTool::input_metadata[SignalDetectionTool::input_metadata$Mandatory == "YES", "Variable"]
  # # remove columns that are not applicable for aggregated data
  # reconstruct_columns <- reconstruct_columns[-which(reconstruct_columns %in% c("case_id", "date_report"))]
  #
  # # if stratification was applied, get the stratification column and append
  # if ("stratum" %in% colnames(to_save)) {
  #   strat_columns <- unique(signals$category)
  #   reconstruct_columns <- union(reconstruct_columns, strat_columns)
  # }
  # # TODO might want to add column `<column>_id` if available
  #
  #
  # # retrieve original value if all identical, set to "all" otherwise
  # for (col in reconstruct_columns) {
  #   to_save[col] <- original_input_data[1, col] # set initial value
  #   if (length(unique(original_input_data[col])[[1]]) > 1) {
  #     to_save <- to_save %>% dplyr::mutate({{ col }} := "all")
  #   }
  # }
  #
  # # iterate over categories, write values into appropriate column
  # for (cat in unique(to_save[["category"]])) {
  #   rows_stratified <- !is.na(to_save[["category"]]) & to_save["category"] == cat
  #   to_save[rows_stratified, cat] <- as.character(to_save[rows_stratified, "stratum"])
  # }
  # # drop some output columns
  # to_save <- to_save %>% dplyr::select(-upperbound, -expected)
  #
  # # check if analysis was stratified, if so drop respective columns
  # if ("stratum" %in% colnames(to_save)) {
  #   to_save <- to_save %>% dplyr::select(-category, -stratum)
  # }

  if (filepath == "") {
    filepath <- conjure_filename(to_save)
  }

  # actually save signals to file an return codes, messages for app to utilize
  tryCatch(
    {
      write.csv(to_save, filepath, row.names = FALSE)

      # Return TRUE and NULL if there are no warnings or errors
      return(list(success = TRUE, message = NULL))
    },
    warning = function(warning_message) {
      # Return TRUE and the warning message if there are warnings
      return(list(success = TRUE, message = warning_message))
    },
    error = function(error_message) {
      # Return FALSE and the error message if there are errors
      return(list(success = FALSE, message = error_message))
    }
  )
}


#' Conjure Filename
#'
#' Generate an output filename based on the results from get_signals.
#'
#' @param data The data frame containing information for filename generation.
#'
#' @return The generated filename with the directory path, i.e. signals_farrington_Austria_Pertussis_2022-01-01_2023-07-08_.csv
#'
#' @examples
#' # Generate a filename based on results data
#' \dontrun{
#' data <- preprocess_data(SignalDetectionTool::input_example)
#' conjure_filename(SignalDetectionTool::get_signals(data))
#' }
#'
#' @export
conjure_filename <- function(data) {
  directory <- "outputs"

  # resulting filename will be
  # signals_method_country_pathogen_date-start_date-end.csv
  filename <- paste("signals", data[1, "method"], data[1, "country"],
    data[1, "pathogen"],
    min(data$date), max(data$date), ".csv",
    sep = "_"
  )

  return(paste(directory, filename, sep = "/"))
}
