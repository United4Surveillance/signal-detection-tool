#' Check input data for required columns and data types
#'
#' This function checks if a data frame contains all the necessary columns and
#' if the columns have the correct data types.
#'
#' @param data A data frame containing the input data.
#'
#' @return Error messages if any issues are found. If no issues are found,
#'         it returns TRUE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read.csv("../data/input/input.csv")
#' result <- check_input_data(data)
#' if (is.logical(result) && result == TRUE) {
#'   print("Data is valid.")
#' } else {
#'   cat(as.character(result), sep = "\n")
#' }
#' }
check_input_data <- function(data) {

  # expected data format
  data_structure <- list(
    case_id = "integer",
    date_report = "Date",
    country = "character",
    country_id = "character",
    age_group = "factor",
    sex = "factor",
    pathogen = "character"
  )

  # are all required columns present?
  necessary_columns <- names(data_structure)
  data_columns <- colnames(data)
  errors <- list()
  missing_columns <- setdiff(necessary_columns, data_columns)

  if (length(missing_columns) != 0) {
    errors <- lapply(missing_columns, function(col) paste0("Missing '", col, "' column in the data"))
  }

  # are column types correct?
  errors <- append(errors,
                   lapply(names(data_structure), function(col_name) {
                     expected_type <- data_structure[[col_name]]
                     actual_type <- data[[col_name]] %>% class()
                     if (expected_type != actual_type) {
                       return(paste0("Column '", col_name, "' is ", actual_type, " type.", " Expected ", expected_type))
                     }
                     else {
                       return(NULL)
                     }
                   }),
                   after = length(errors)
  )

  # remove empty slots
  errors <- errors[sapply(errors, function(element) !is.null(element))]

  # return TRUE or print error messages
  if (length(errors) != 0) {
    return(errors)
  }
  else {
    return(TRUE)
  }
}
