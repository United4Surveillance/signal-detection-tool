
#' Get the numeric columns that are not integer columns
#'
#' This function takes a data frame as input and returns the names of columns
#' that contain numeric values and are not integer columns.
#'
#' @param data A data frame.
#'
#' @return A character vector containing the names of numeric columns that are
#'   not integer columns.
#' @export
#'
#' @examples
#'
#' data <- data.frame(a = 1:5, b = 1:5, c = 1L:5L)
#' SignalDetectionTool::get_float_columns(data)
#'
get_float_columns <- function(data) {
  # Create a logical vector indicating whether each column is numeric
  is_numeric <- sapply(data, is.numeric)

  # Use lapply to apply is.integer to each column
  is_integer <- sapply(data, is.integer)

  # Get the names of all numeric columns
  numeric_columns <- names(data)[is_numeric]

  # Get the names of integer columns
  integer_columns <- names(data)[is_integer]

  # Find the set of numeric columns that are not integer columns
  return(setdiff(numeric_columns, integer_columns))
}

#' Create an interactive or static table
#'
#' This function creates an interactive DataTable or a static gt table depending
#' on the value of the `interactive` parameter. It can also format float columns
#' to have two decimal places.
#'
#' @param data A data frame.
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#'
#' @return An interactive DataTable or a static gt table, depending on the value
#'   of `interactive`.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(a = 1:5, b = 1:5, c = 1.234:5.234)
#' create_table(data)
#' }
create_table <- function(data, interactive = TRUE) {
  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )
  # get which columns contain floats
  float_columns <- get_float_columns(data)

  # transform NA in stratum to unknown
  if("stratum" %in% colnames(data) & any(!is.na(data[["category"]]))) {
    data <- data %>%
      dplyr::mutate(stratum = tidyr::replace_na(stratum, "unknown"))
  }

  if (interactive == TRUE) {
    # create interactive table
    table <- DT::datatable(data)
    if (length(float_columns)) {
      table <- table %>% DT::formatRound(float_columns, 2)
    }
  } else {
    # create static table for reports
    table <- data %>%
      gt::gt() %>%
      gt::tab_options(
        column_labels.padding = gt::px(3),
        column_labels.font.weight = "bold"
      )
    if (length(float_columns)) {
      table <- table %>% gt::fmt_number(columns = float_columns)
    }
  }

  return(table)
}

#' Convert specified columns to integer type
#'
#' This function takes a data frame and a vector of column names and converts
#' the specified columns to integer type.
#'
#' @param data A data frame.
#' @param columns_to_convert A character vector of column names to convert to integers.
#'
#' @return A data frame with the specified columns converted to integers.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(a = 1:5, b = 1:5, c = 1.234:5.234)
#' convert_columns_integer(data, c("a", "b"))
#' }
convert_columns_integer <- function(data, columns_to_convert) {
  # check that all columns are present in the data
  for (col in columns_to_convert) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  # Loop through the specified columns and convert them to integers
  for (col_name in columns_to_convert) {
    data[[col_name]] <- as.integer(data[[col_name]])
  }
  return(data)
}

#' Create a results table with optional filtering
#'
#' This function creates a results table based on the input data frame. It can
#' filter the data based on the `positive_only` parameter and converts certain
#' columns to integers for styling purposes.
#'
#' @param data A data frame.
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#' @param positive_only Logical indicating whether to filter only positive cases
#'   (default is TRUE).
#'
#' @return An interactive DataTable or a static gt table, depending on the value
#'   of `interactive`.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(year = 2020:2022,
#'                    week = 1:3,
#'                    cases = 10:12,
#'                    alarms = c(TRUE, FALSE, TRUE),
#'                    upperbound = c(15, NA, 14))
#' create_results_table(data)
#' }
create_results_table <- function(data,
                                 interactive = TRUE,
                                 positive_only = TRUE) {
  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_true(positive_only),
    checkmate::check_false(positive_only),
    combine = "or"
  )

  # Somehow columns are of type double when the should be integers
  # convert for styling later on
  data <- convert_columns_integer(data, c("year", "week", "cases"))

  data <- data %>% dplyr::filter(!is.na(.data$upperbound))
  if (positive_only) {
    data <- data %>% dplyr::filter(.data$alarms == TRUE)
  }

  return(create_table(data, interactive))
}
