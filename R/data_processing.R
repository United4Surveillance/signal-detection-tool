#' Preprocessing of linelist surveillance data with or without outbreak_ids
#' @param data data.frame, Linelist of surveillance data
#' @returns data.frame, preprocessed linelist with transformation of columns to date,
#' to lower, generation of isoyear and isoweek
#'
#' @export
#'
#' @examples
#' \dontrun{
#' preprocess_data(input_example)
#' }
preprocess_data <- function(data) {
  # remove completely empty columns from the dataset
  data <- remove_empty_columns(data)

  yes_no_unknown_vars <- intersect(colnames(data), yes_no_unknown_variables())
  # get all variables present in the data which might need transformation tolower
  to_lower_vars <- intersect(colnames(data), c(yes_no_unknown_variables(), "sex"))
  # get all regional stratification variables
  regional_id_vars <- intersect(colnames(data), region_id_variable_names())

  # get all variables that are characters and not case_id or date
  factorization_vars <- dplyr::select(data, dplyr::where(is.character) &
    !dplyr::any_of(c("sex", "age_group")) &
    !dplyr::all_of(yes_no_unknown_vars) &
    !dplyr::starts_with("date") &
    !dplyr::ends_with("id")) %>% names()

  # remove cases with missing values
  data <- data %>%
    dplyr::filter_at(check_for_missing_values(), dplyr::all_vars(!is.na(.)))

  na_tokens <- c("", "unknown", "NA", "na")  # cover both cases
  lvl_ynu   <- unlist(yes_no_unknown_levels())

  char_cols <- names(Filter(is.character, data))
  date_cols <- grep("^date", names(data), value = TRUE)

  data <- data %>%
    dplyr::mutate(
      # 1) Trim whitespaces only once
      dplyr::across(all_of(char_cols), ~ stringr::str_trim(.x)),

      # 2) Convert only the desired columns to lowercase
      dplyr::across(all_of(to_lower_vars), ~ tolower(.x)),

      # 3) Set missing values consistently (one pass instead of three na_if)
      dplyr::across(all_of(char_cols), ~ { .x[.x %in% na_tokens] <- NA_character_; .x }),

      # 4) Parse date columns specifically and efficiently
      dplyr::across(all_of(date_cols),
                    ~ readr::parse_date(.x, format = "%Y-%m-%d", na = na_tokens)),

      # 5) Type adjustments / factorization
      dplyr::across(all_of(regional_id_vars), as.character),
      dplyr::across(all_of(yes_no_unknown_vars), ~ factor(.x, levels = lvl_ynu)),
      dplyr::across(all_of(factorization_vars), as.factor)
    )

  # add columns for isoyear and isoweek for each date
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        starts_with("date") & !where(is.numeric),
        .fns = list(
          year = ~ lubridate::isoyear(.x),
          week = ~ lubridate::isoweek(.x)
        ),
        .names = "{.col}_{.fn}"
      )
    )

  if ("age" %in% names(data())) {
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of("age"), ~ dplyr::if_else(.x < 0, NA_integer_, .x)))
  }
  # age or age_group is mandatory thus we need to check whether column present in data
  # or else create age_group from age
  data <- age_groups(data)

  # sex is not mandatory
  if ("sex" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(sex = factor(sex, levels = sex_levels()))
  }

  data
}


#' Aggregates case data (linelist, i.e. one row per case) by isoyear and isoweek and adds missing isoweeks to the aggregated dataset.
#' Additionally number of cases part of a known outbreak is added if the variable outbreak_status exists in the data.
#'
#' @param data data.frame, linelist of cases to be aggregated
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param date_start A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the minimum date of the dataset. This parameter can be used when the dataset should be extended further than the minimum date of the dataset.
#' @param date_end A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the maximum date of the dataset. This can be used when the dataset should be extended further than the minimum date of the dataset.
#' @param group A character specifying another grouping variable. Usually used for stratification.
#' @examples
#' \dontrun{
#' data <- preprocess_data(input_example) %>% aggregate_data()
#' }
#' @export
aggregate_data <- function(data,
                           date_var = "date_report",
                           date_start = NULL,
                           date_end = NULL,
                           group = NULL) {
  checkmate::check_subset(c(group, date_var), names(data), empty.ok = TRUE)

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  if (is.null(group)) {
    data_agg <- data %>%
      dplyr::group_by(cw_iso, .drop = FALSE)
  } else {
    data_agg <- data %>%
      dplyr::group_by(cw_iso, !!rlang::sym(group), .drop = FALSE)
  }

  data_agg <- data_agg %>%
    dplyr::summarize(cases = dplyr::n(), .groups = "drop")

  if ("outbreak_status" %in% names(data)) {
    if (is.null(group)) {
      data_outbreak_agg <- data %>%
        dplyr::group_by(cw_iso, .drop = FALSE)
    } else {
      data_outbreak_agg <- data %>%
        dplyr::group_by(cw_iso, !!rlang::sym(group), .drop = FALSE)
    }
    data_outbreak_agg <- data_outbreak_agg %>%
      dplyr::summarize(
        cases_in_outbreak = sum(outbreak_status == "yes", na.rm = TRUE),
        .groups = "drop"
      )

    data_agg <- data_agg %>%
      dplyr::left_join(data_outbreak_agg, by = c("cw_iso", group)) %>%
      dplyr::mutate(cases_in_outbreak = dplyr::if_else(is.na(cases_in_outbreak), 0, cases_in_outbreak))
  }
  data_agg |>
    tidyr::separate_wider_delim(cw_iso, delim = "-", names = c("year", "week")) |>
    dplyr::mutate(year = as.numeric(year),
                  week = as.numeric(week)) |>
    dplyr::arrange(year, week) |>
    as.data.frame()
}

#' Filter Data Frame by Date Range
#'
#' This function filters a data frame to include only rows where a specified date column
#' falls within a given start and/or end date. The function accepts \code{Date} objects
#' or character strings (formatted as \code{"yyyy-mm-dd"}) for \code{date_start} and
#' \code{date_end} parameters.
#'
#' @param data A data frame containing the date column to filter by.
#' @param date_var A character string specifying the name of the date column in \code{data}.
#'   Default is \code{"date_report"}.
#' @param date_start A \code{Date} object or character string in \code{"yyyy-mm-dd"} format, or
#'   \code{NULL}. If provided, only rows where \code{date_var} is greater than or equal
#'   to \code{date_start} are included. Default is \code{NULL}.
#' @param date_end A \code{Date} object or character string in \code{"yyyy-mm-dd"} format, or
#'   \code{NULL}. If provided, only rows where \code{date_var} is less than or equal to
#'   \code{date_end} are included. Default is \code{NULL}.
#'
#' @return A filtered data frame containing only rows that match the specified date range.
#'
#' @examples
#' # Example data frame
#' \dontrun{
#' data <- data.frame(
#'   date_report = as.Date("2023-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#'
#' # Filter data from January 3, 2023 to January 8, 2023 (using Date format)
#' filtered_data <- filter_by_date(data,
#'   date_var = "date_report",
#'   date_start = as.Date("2023-01-03"),
#'   date_end = as.Date("2023-01-08")
#' )
#'
#' # Filter data using character format for dates
#' filtered_data <- filter_by_date(data,
#'   date_var = "date_report",
#'   date_start = "2023-01-03",
#'   date_end = "2023-01-08"
#' )
#' }
filter_by_date <- function(data, date_var = "date_report", date_start = NULL, date_end = NULL) {
  checkmate::check_subset(date_var, names(data))

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  if (!is.null(date_start)) {
    data <- data %>% dplyr::filter(!!rlang::sym(date_var) >= date_start)
  }
  if (!is.null(date_end)) {
    data <- data %>% dplyr::filter(!!rlang::sym(date_var) <= date_end)
  }
  data
}

#' Turns aggregated data into surveillance's sts format
#' @param case_counts case count data frame to be converted
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data <- preprocess_data(data) %>% aggregate_data()
#' sts_cases <- convert_to_sts(data)
#' }
convert_to_sts <- function(case_counts) {
  # create sts object
  return(surveillance::sts(case_counts$cases,
    start = c(
      case_counts$year[1],
      case_counts$week[1]
    ),
    frequency = 52
  ))
}

#' Filter the data so that only the data of the last n weeks are returned
#' This function can be used to filter for those last n weeks where signals were generated.
#' @param data_agg data.frame, aggregated surveillance or signals dataset, where aggregated means no linelist but cases or signals per week, year
#' @param number_of_weeks integer, specifying the number of weeks from the most recent week we want to filter the data for
#' @returns data.frame, aggregated data of last n weeks
filter_data_last_n_weeks <- function(data_agg,
                                     number_of_weeks) {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  data_agg %>%
    dplyr::group_by(category, stratum) %>%
    dplyr::arrange(year, week) %>%
    dplyr::slice_tail(n = number_of_weeks) %>%
    dplyr::ungroup()
}

#' Adds a column `cw_iso` to the data.frame `data`.
#'
#' Uses `date_var`to create a factor column of isoweeks. Factor levels are all
#' weeks between `date_start` and `date_end`.
#' @inheritParams aggregate_data
add_cw_iso <- function(data,
                       date_start = NULL,
                       date_end = NULL,
                       date_var = "date_report") {
  # get min and max date of the whole dataset before stratification
  # stratified aggregated data can be filled up with 0s until min and max date
  # of the full dataset
  if (is.null(date_start)) {
    date_start <- min(data[[date_var]], na.rm = TRUE)
  }
  if (is.null(date_end)) {
    date_end <- max(data[[date_var]], na.rm = TRUE)
  }

  # function to get all iso weeks between date_start and date_end
  get_all_cw_iso <- function(date_start, date_end) {
    all_weeks_as_dates <- c(seq.Date(from = date_start, to = date_end, by = "week"), date_end)
    unique(paste0(lubridate::isoyear(all_weeks_as_dates), "-", lubridate::isoweek(all_weeks_as_dates)))
  }

  # add cw_iso (isoweeks) as factor levels
  all_cw_iso <- get_all_cw_iso(date_start = date_start, date_end = date_end)
  data <- data |>
    dplyr::mutate(cw_iso = paste0(lubridate::isoyear(!!rlang::sym(date_var)), "-",
                                  lubridate::isoweek(!!rlang::sym(date_var))),
                  cw_iso = factor(cw_iso, levels = all_cw_iso))
  data
}
