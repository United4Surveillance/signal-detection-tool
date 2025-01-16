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
#' @param signals_only Logical indicating whether to filter the signal results to include only the weeks when a signal was generated (default is TRUE). If set to TRUE, the signals column is removed from the table. When FALSE the signals column is kept to distinguish the weeks with and without alarms.
#' @param interactive Logical indicating whether to create an interactive
#'   DataTable (default is TRUE).
#' @param dt_selection_type String controlling the DataTable selection argument. Expected values are "multiple", "single", "none" (default is 'single').
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
#'   alarms = c(TRUE, TRUE, TRUE, FALSE),
#'   threshold = c(15, NA, 14, 14),
#'   category = c("age_group", "age_group", "sex", "sex"),
#'   stratum = c("00-05", "30-35", "female", "male")
#' )
#' format_table(data)
#'
#' data_agg <- data.frame(
#'   stratum = c("00-04", "05-09", "10-14", "100-104", "105-109", "15-19"),
#'   cases = c(74, 5, 0, 0, 0, 2),
#'   any_alarms = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
#'   n_alarms = c(1, 0, 0, 0, 0, 0),
#'   category = rep("age_group", 6)
#' )
#'
#' format_table(data_agg)
#' }
format_table <- function(data, signals_only = TRUE, interactive = TRUE,
                         dt_selection_type = "single") {
  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_true(signals_only),
    checkmate::check_false(signals_only),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_choice(dt_selection_type,
      choices = c("multiple", "single", "none")
    )
  )

  if (signals_only) {
    # only remove the alarms column when the signals_only TRUE thus only those with signals in the linelist are remaining
    data <- data %>% dplyr::select(-dplyr::one_of("alarms"))
  } else {
    data <- data %>% dplyr::rename(signals = alarms)
  }

  if ("cases_in_outbreak" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(cases_in_outbreak = as.integer(cases_in_outbreak)) %>%
      dplyr::rename("Cases in outbreak" = cases_in_outbreak)
  }

  # remove columns which were added for the visualisation of the timeseries but we do not want to have for the table
  data <- data %>% dplyr::select(-dplyr::one_of("expected_pad", "upperbound_pad", "first_alarm_nonNA"))

  # when it is already a factor we do care about NA to unknown before
  if (!is.factor(data$stratum)) {
    data <- data %>%
      dplyr::mutate(stratum = as.character(stratum)) %>%
      dplyr::mutate(stratum = dplyr::if_else(category != "None", tidyr::replace_na(stratum, "unknown"), stratum))
  }

  data <- data %>%
    dplyr::select(-tidyselect::one_of(c("number_of_weeks", "method"))) %>%
    dplyr::rename_all(~ stringr::str_to_title(.x)) %>%
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
      selection = dt_selection_type,
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
    table <- data %>%
      dplyr::mutate(dplyr::across(dplyr::any_of("Year"), as.character)) %>%
      flextable::as_grouped_data(groups = "Category") %>%
      flextable::as_flextable(hide_grouplabel = TRUE) %>%
      flextable::theme_zebra() %>%
      flextable::bg(bg = "#304794", part = "header") %>%
      flextable::color(color = "white", part = "header") %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::bg(i = ~ !is.na(Category), bg = "grey", part = "body") %>%
      flextable::bold(i = ~ !is.na(Category), part = "body") %>%
      flextable::autofit()

    if (length(float_columns)) {
      table <- table %>%
        flextable::colformat_num(j = float_columns, digits = 2)
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
  data
}

#' Prepare the signal detection results for creation of table with results
#'
#' This function converts the columns week, year and cases to integer, columns are renamed and category with NA is replaced by None. It can
#' filter the data based on the `signals_only` parameter giving back only those weeks where a signal was found.
#'
#' @param data data.frame containing signals from \link{get_signals}
#' @param signals_only Logical indicating whether to filter the signal results to include only the weeks when a signal was generated (default is TRUE). If set to TRUE, the signals column is removed from the table. When FALSE the signals column is kept to distinguish the weeks with and without alarms.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   year = 2020:2022,
#'   week = 1:3,
#'   cases = 10:12,
#'   alarms = c(TRUE, FALSE, TRUE),
#'   upperbound = c(15, NA, 14),
#'   category = c(NA, NA, NA)
#' )
#' prepare_signals_table(data)
#' }
prepare_signals_table <- function(data,
                                  signals_only = TRUE) {
  checkmate::assert(
    checkmate::check_true(signals_only),
    checkmate::check_false(signals_only),
    combine = "or"
  )

  # Somehow columns are of type double when the should be integers
  # convert for styling later on
  data <- convert_columns_integer(data, c("year", "week", "cases"))

  data <- data %>%
    dplyr::mutate(category = dplyr::if_else(is.na(category), "None", category)) %>%
    dplyr::rename(threshold = upperbound)

  data <- data %>% dplyr::filter(!is.na(threshold))
  if (signals_only) {
    data <- data %>% dplyr::filter(alarms)
  }

  data
}

#' Builds the signal detection results table with different formating options. To get the raw data.frame containing method ald number_of_weeks as well use format = "data.frame", to obtain nicely formated tables in an interactive DataTable or as Flextable use format = "DataTable" or format = "Flextable".
#'
#' This function applies the \link{prepare_signals_table} and if format = c("DataTable","Flextable") \link{format_table} to create a nicely formated results table based on the input data frame. If format = "data.frame" \link{format_table} is not applied and the raw preprocessed signal_results are returned. It can
#' filter the data based on the `signals_only` parameter and converts certain
#' columns to integers for styling purposes. This table is used to show all signal detection results for different stratifications together in one table.
#' @param signal_results data.frame containing signals from \link{get_signals}
#' @param signals_only Logical indicating whether to filter the signal results to include only the weeks when a signal was generated (default is TRUE). If set to TRUE, the signals column is removed from the table. When FALSE the signals column is kept to distinguish the weeks with and without alarms.
#' @param format Character specifying the output format. Must be one of:
#'   - `"data.frame"`: A standard R data frame.
#'   - `"DataTable"`: An interactive table using the DataTable library.
#'   - `"Flextable"`: A formatted table suitable for reporting,i.e. word documents.
#'  Default is "DataTable".
#'
#' @param dt_selection_type String controlling the DataTable selection argument. Expected values are "multiple", "single", "none" (default is 'single').
#' @return data.frame or DataTable or Flextable depending on `format`
#' @export
#'
#' @examples
#' \dontrun{
#' signal_results <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("age_group"), number_of_weeks = 6)
#' build_signals_table(signal_results)
#' build_signals_table(signal_results, format = "data.frame")
#' }
build_signals_table <- function(signal_results,
                                signals_only = TRUE,
                                format = "DataTable",
                                dt_selection_type = "single") {
  checkmate::assert(
    checkmate::check_choice(format, choices = c(
      "data.frame",
      "DataTable",
      "Flextable"
    ))
  )
  checkmate::assert(
    checkmate::check_true(signals_only),
    checkmate::check_false(signals_only),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_choice(dt_selection_type,
      choices = c("multiple", "single", "none")
    )
  )

  table <- signal_results %>%
    prepare_signals_table(signals_only = signals_only)

  if (format == "DataTable") {
    table <- table %>%
      format_table(
        signals_only = signals_only, interactive = TRUE,
        dt_selection_type = dt_selection_type
      )
  }
  if (format == "Flextable") {
    table <- table %>%
      format_table(signals_only = signals_only, interactive = FALSE)
  }

  table
}

#' Create an Empty DataTable with a Custom Message
#'
#' This function generates a minimal `DT::datatable` displaying a custom message.
#' It is useful for placeholder content when there is no data to display.
#'
#' @param message A character string specifying the message to display in the table.
#'
#' @return A `DT::datatable` object containing a single-row, single-column table with the custom message.
#'
#' @details
#' The resulting DataTable will have the following features:
#' - No column headers.
#' - No search box, pagination, or sorting functionality.
#' - A single-row table displaying the provided message.
#'
#' @examples
#' # Create an empty DataTable with a custom message
#' build_empty_datatable("No data available")
#'
#' @importFrom DT datatable
#' @export
build_empty_datatable <- function(message) {
  DT::datatable(
    data.frame(Message = message),
    options = list(
      dom = "t", # Only show the table body (no search/filter controls)
      paging = FALSE,
      ordering = FALSE
    ),
    rownames = FALSE,
    colnames = NULL # Hide the column header
  )
}

#' Prepares aggregated signals of one category for producing a table.
#'
#' It expects the aggregated signals input to only have one category. It converts certain columns
#' to integers and the stratum column to factor with NA converted to unknown for styling
#' purposes. It orders the strata by the factor levels. This table is used to show stratified signal results for one category, i.e. sex
#' and results for all the strata no matter whether there are signals or not.
#'
#' @param signals_agg A tibble or data.frame containing aggregated signals produced from \link{aggregate_signals}(signals,number_of_weeks = 6) for only one category
#'
#' @return tibble with preprocessed aggregated signals
#'
#' @examples
#' \dontrun{
#' signals_agg <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("age_group", "sex")) %>%
#'   aggregate_signals(number_of_weeks = 6) %>%
#'   filter(category == "age_group")
#'
#' prepare_signals_agg_table(signals_agg)
#' }
prepare_signals_agg_table <- function(signals_agg) {
  category <- unique(signals_agg$category)
  stopifnot(length(category) == 1)
  signals_agg <- signals_agg %>% convert_columns_integer(c("cases", "n_alarms"))

  signals_agg <- create_factor_with_unknown(signals_agg)
  signals_agg <- signals_agg %>%
    dplyr::arrange(dplyr::desc(n_alarms))

  signals_agg
}

#' Builds the aggregated signal detection results table with different formating options.
#'
#' Prepares and formats the aggregated signal results table for one category and orders the strata by the factor levels.
#' This function combines the preparation of the aggregated signals data.frame with the final formating of the table by applying \link{prepare_signals_agg_table} and \link{format_table}.
#' @param signals_agg A tibble or data.frame containing aggregated signals produced from \link{aggregate_signals}(signals,number_of_weeks = 6).
#' @param format Character specifying the output format. Must be one of:
#'   - `"data.frame"`: A standard R data frame.
#'   - `"DataTable"`: An interactive table using the DataTable library.
#'   - `"Flextable"`: A formatted table suitable for reporting,i.e. word documents.
#'   Default is "DataTable".
#'
#' @return data.frame or DataTable or Flextable depending on `format`
#'
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
#' build_signals_agg_table(signals_agg)
#' }
build_signals_agg_table <- function(signals_agg,
                                    format = "DataTable") {
  checkmate::assert(
    checkmate::check_choice(format, choices = c(
      "data.frame",
      "DataTable",
      "Flextable"
    ))
  )
  table <- signals_agg %>%
    prepare_signals_agg_table()

  if (format == "DataTable") {
    table <- table %>%
      format_table(signals_only = TRUE, interactive = TRUE)
  }
  if (format == "Flextable") {
    table <- table %>%
      format_table(signals_only = TRUE, interactive = FALSE)
  }

  table
}
