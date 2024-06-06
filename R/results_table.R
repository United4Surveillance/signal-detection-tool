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

#' Format the signal results to an interactive or static table
#'
#' This function creates an interactive DataTable or a static gt table depending
#' on the value of the `interactive` parameter. It takes the stratum and category columns and transforms those to be headers of sections containing all signal results for these strata. It colors rows red for tables containing the any_alarms column when there was >= 1 alarm. It makes the table downloadable as CSV, Excel, PDF.
#'
#' @param data A data frame.
#'
#' @param positive_only Logical indicating whether to filter only those signal results where a signal was generated (default is TRUE). When TRUE then the signals column is not removed from the table.
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#'
#' @return An interactive DataTable or a static gt table, depending on the value
#'   of `interactive`.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   year = 2020:2023,
#'   week = 1:4,
#'   cases = 10:13,
#'   alarms = c(TRUE,TRUE,TRUE,FALSE),
#'   threshold = c(15, NA, 14, 14),
#'   category = c("age_group","age_group","sex","sex"),
#'   stratum = c("00-05","30-35","female","male")
#' )
#' format_table(data)
#'
#' data_agg <- data.frame(
#' stratum = c("00-04", "05-09", "10-14", "100-104", "105-109", "15-19"),
#' cases = c(74, 5, 0, 0, 0, 2),
#' any_alarms = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
#' n_alarms = c(1, 0, 0, 0, 0, 0),
#' category = rep("age_group", 6)
#' )
#'
#' format_table(data_agg)
#' }
format_table <- function(data, positive_only = TRUE, interactive = TRUE) {
  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  if (positive_only) {
    # only remove the alarms column when the positive_only TRUE thus only those with signals in the linelist are remaining
    data <- data %>% dplyr::select(-tidyselect::one_of("alarms"))
  } else {
    data <- data %>% dplyr::rename(signals = alarms)
  }

  data <- data %>% dplyr::rename_all(~ stringr::str_to_title(.x))
  data <- data %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.double), round, digits = 2)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor)) %>%
    dplyr::relocate(tidyselect::where(is.factor))

  # get which columns contain floats
  float_columns <- get_float_columns(data)

  if (interactive == TRUE) {
    # create interactive table
    table <- DT::datatable(data,
      class = "cell-border stripe hover", rownames = FALSE,
      filter = list(position = "bottom", plain = TRUE),
      extensions = c("Buttons", "RowGroup"),
      selection = "single",
      options = list(
        pageLength = 10,
        rowGroup = list(dataSrc = 0),
        columnDefs = list(list(visible = FALSE, targets = 0)),
        dom = "tfrBip",
        buttons = c("copy", "csv", "excel", "pdf"),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#304794', 'color': '#fff'});",
          "}"
        )
      )
    )
    if ("Any_alarms" %in% colnames(data)) {
      table <- table %>% DT::formatStyle(
        "Any_alarms",
        target = "row",
        backgroundColor = DT::styleEqual(c(TRUE), c("#ff8282"))
      )
    }
    if (length(float_columns)) {
      table <- table %>% DT::formatRound(float_columns, 2)
    }
  } else {
    # create static table for reports
    data <- data %>%
      dplyr::mutate(id = 1:nrow(data)) %>%
      tidyr::pivot_wider(names_from = Category, values_from = Stratum) %>%
      dplyr::select(-id) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor)) %>%
      dplyr::relocate(tidyselect::where(is.factor))

    table <- data %>%
      gt::gt() %>%
      gt::tab_options(
        column_labels.padding = gt::px(3),
        column_labels.font.weight = "bold"
      ) %>%
      gt::opt_stylize(style = 5, color = "blue")
    if (length(float_columns)) {
      table <- table %>% gt::fmt_number(columns = float_columns)
    }
  }

  table
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

#' Prepare the signal detection results for creation of table with results
#'
#' This function converts the columns week, year and cases to integer, columns are renamed and category with NA is replaced by None. It can
#' filter the data based on the `positive_only` parameter giving back only those data points where a signal was found.
#'
#' @param data data.frame containing signals from \code{\link{get_signals()}}
#' @param positive_only Logical indicating whether to filter only those signal results where a signal was generated (default is TRUE).
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   year = 2020:2022,
#'   week = 1:3,
#'   cases = 10:12,
#'   alarms = c(TRUE, FALSE, TRUE),
#'   upperbound = c(15, NA, 14),
#'   category = c(NA,NA,NA)
#' )
#' process_signals_for_table(data)
#' }
process_signals_for_table <- function(data,
                                      positive_only = TRUE) {
  checkmate::assert(
    checkmate::check_true(positive_only),
    checkmate::check_false(positive_only),
    combine = "or"
  )

  # Somehow columns are of type double when the should be integers
  # convert for styling later on
  data <- convert_columns_integer(data, c("year", "week", "cases"))

  data <- data %>%
    dplyr::mutate(category = dplyr::if_else(is.na(category), "None", category)) %>%
    dplyr::rename(threshold = upperbound)

  data <- data %>% dplyr::filter(!is.na(threshold))
  if (positive_only) {
    data <- data %>% dplyr::filter(alarms)
  }

  return(data)
}

#' Create and format the signal detection results table
#'
#' This function applies the \code{\link{process_signals_for_table()}} and \code{\link{format_table()}} to create a nicely formated results table based on the input data frame. It can
#' filter the data based on the `positive_only` parameter and converts certain
#' columns to integers for styling purposes. This table is used to show all signal detection results for different stratifications together in one table.
#' @param data data.frame containing signals from \code{\link{get_signals()}}
#' @param positive_only Logical indicating whether to filter only those signal results where a signal was generated (default is TRUE).
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#'
#' @return An interactive DataTable or a static gt table, depending on the value
#'   of `interactive`.
create_and_format_signals_table <- function(data,
                                            positive_only = TRUE,
                                            interactive = TRUE) {
  table <- data %>%
    process_signals_for_table(positive_only = positive_only) %>%
    format_table(interactive = interactive)

  return(table)
}

#' Prepares aggregated signals of one category for producing a table.
#'
#' It expects the aggregated signals input to only have one category. It converts certain columns
#' to integers and the stratum column to factor with NA converted to unknown for styling
#' purposes. It orders the strata by the factor levels. This table is used to show stratified signal results for one category, i.e. sex
#' and results for all the strata no matter whether there are signals or not.
#'
#' @param signals_agg A tibble or data.frame containing aggregated signals produced from \code{\link{aggregate_signals(signals,number_of_weeks = 6)}} for only one category
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#'
#' @return tibble with preprocessed aggregated signals
#' @export
#'
#' @examples
#' \dontrun{
#' signals_agg <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("age_group", "sex")) %>%
#'   aggregate_signals(number_of_weeks = 6) %>%
#'   filter(category == "age_group")
#'
#' process_signals_agg_for_table(signals_agg)
#' }
process_signals_agg_for_table <- function(signals_agg) {
  category <- unique(signals_agg$category)
  stopifnot(length(category) == 1)
  signals_agg <- signals_agg %>% convert_columns_integer(c("cases", "n_alarms"))

  signals_agg <- create_factor_with_unknown(signals_agg)
  signals_agg <- signals_agg %>%
    dplyr::arrange(dplyr::desc(n_alarms))

  return(signals_agg)
}

#' Create and format the aggregated signal results table for one category and orders the strata by the factor levels
#'
#' This function combines the preprocessing of the aggregated signals data.frame with the final formating of the table by applying \code{\link{process_signals_agg_for_table()}} and \code{\link{format_table()}}.
#' @param signals_agg A tibble or data.frame containing aggregated signals produced from \code{\link{aggregate_signals(signals,number_of_weeks = 6)}}
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#'
#' @return An interactive DataTable or a static gt table, depending on the value
#'   of `interactive`.
#'   #' @examples
#' \dontrun{
#' signals_agg <- input_example %>%
#' preprocess_data() %>%
#' get_signals(stratification = c("age_group","sex")) %>%
#' aggregate_signals(number_of_weeks = 6) %>%
#' filter(category == "age_group")
#'
#' signals_agg_processed <- process_signals_agg_for_table(signals_agg)
#' create_and_format_signals_agg_table(signals_agg_processed)
#' }
create_and_format_signals_agg_table <- function(signals_agg,
                                                interactive = TRUE) {
  table <- signals_agg %>%
    process_signals_agg_for_table() %>%
    format_table(positive_only = TRUE, interactive = interactive)

  return(table)
}
