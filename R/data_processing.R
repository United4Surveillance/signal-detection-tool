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

  data <- data %>%
    # strip trailing or leading whitespaces
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_lower_vars), ~ tolower(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "unknown"))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "NA"))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date"), ~ as.Date(.x, optional = T))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(regional_id_vars), ~ as.character(.x))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(yes_no_unknown_vars),
      ~ factor(.x, levels = unlist(yes_no_unknown_levels()))
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factorization_vars), ~ as.factor(.x)))


  # add columns for isoyear and isoweek for each date
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOYear,
      .names = "{.col}_year"
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOWeek,
      .names = "{.col}_week"
    ))

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
#' @examples
#' \dontrun{
#' data <- preprocess_data(input_example) %>% aggregate_data()
#' }
aggregate_data <- function(data,
                           date_var = "date_report",
                           date_start = NULL,
                           date_end = NULL) {
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

  week_var <- paste0(date_var, "_week")
  year_var <- paste0(date_var, "_year")

  data_agg <- data %>%
    dplyr::group_by(!!rlang::sym(week_var), !!rlang::sym(year_var)) %>%
    dplyr::summarize(cases = dplyr::n(), .groups = "drop") %>%
    dplyr::select(
      week = !!rlang::sym(week_var),
      year = !!rlang::sym(year_var),
      cases
    ) %>%
    dplyr::arrange(year, week)

  # add the missing isoweeks to the dataset
  data_agg <- data_agg %>% add_missing_isoweeks(
    date_start = date_start,
    date_end = date_end
  )

  if ("outbreak_status" %in% names(data)) {
    data_outbreak_agg <- data %>%
      dplyr::group_by(!!rlang::sym(week_var), !!rlang::sym(year_var)) %>%
      dplyr::summarize(
        cases_in_outbreak = sum(outbreak_status == "yes", na.rm = T),
        .groups = "drop"
      ) %>%
      dplyr::select(
        week = !!rlang::sym(week_var),
        year = !!rlang::sym(year_var),
        cases_in_outbreak
      ) %>%
      dplyr::arrange(year, week)

    data_agg <- data_agg %>%
      dplyr::left_join(data_outbreak_agg, by = c("year", "week")) %>%
      dplyr::mutate(cases_in_outbreak = dplyr::if_else(is.na(cases_in_outbreak), 0, cases_in_outbreak))
  }
  data_agg
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
    data <- data %>% dplyr::filter(!!sym(date_var) >= date_start)
  }
  if (!is.null(date_end)) {
    data <- data %>% dplyr::filter(!!sym(date_var) <= date_end)
  }
  data
}

#' Aggregate cases and signals over the number of weeks.

#' First the signals are filtered to obtain the signals for the last n weeks
#' aggregating the number of cases observed, create variable any signal generated and the aggregate the number of signals

#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per week, year
#' @param number_of_weeks integer, specifying the number of weeks we want to aggregate the number of cases and the generated signals
#' @returns tibble, with one line per groups containing the number of cases, any_alarms and n_alarms
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "county_id"))
#' signals %>% aggregate_signals(number_of_weeks = 6)
#' }
#' @export
aggregate_signals <- function(signals, number_of_weeks) {
  signals %>%
    filter_data_last_n_weeks(number_of_weeks = number_of_weeks) %>%
    dplyr::group_by(category, stratum) %>%
    dplyr::summarise(
      cases = sum(cases, na.rm = T),
      any_alarms = any(alarms, na.rm = T),
      n_alarms = sum(alarms, na.rm = T)
    ) %>%
    dplyr::ungroup()
}

#' Extend the computed threshold and expectation of the signal detection method to the past for visualisation purposes but not for signal generation

#' Inside the function it is computed what the maximum number of timepoints is the signal detection algorithms can be applied for. This depends on the algorithm and the amount of historic data. The already generated signals dataframe is then extended with the expectation and threshold into the past
#' @param data A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per week, year
#' @returns tibble, with padded signals
#' @examples
#' \dontrun{
#' input_example_prepro <- input_example %>%
#'   preprocess_data()
#' signals <- input_example_prepro %>%
#'   get_signals(stratification = c("sex", "county_id"))
#' pad_signals(input_example_prepro, signals)
#' }
#' @export
pad_signals <- function(data,
                        signals) {
  # get the stratification, method and number_of_weeks from the signals data
  stratification <- if (all(is.na(signals$category))) {
    NULL
  } else {
    unique(signals$category)[!is.na(unique(signals$category))]
  }

  number_of_weeks <- unique(signals$number_of_weeks)
  method <- unique(signals$method)

  stopifnot(length(number_of_weeks) == 1)
  stopifnot(length(method) == 1)

  available_thresholds <- list(26, 20, 14, 8, 2)
  signals_all_timeopts <- dplyr::bind_rows(purrr::map(unlist(available_thresholds), function(timeopt) {
    signals_timeopt <- get_signals(data,
      method = method,
      number_of_weeks = timeopt + number_of_weeks
    )
    if (!is.null(signals_timeopt)) {
      signals_timeopt <- signals_timeopt %>% dplyr::mutate(time_opt = timeopt)
    }
  }))

  time_opts_working <- unique(signals_all_timeopts$time_opt)
  time_opts_working_named <- available_thresholds[unlist(available_thresholds) %in% time_opts_working]
  max_time_opt <- max(unlist(time_opts_working_named))

  result_padding_unstratified <- signals_all_timeopts %>%
    dplyr::filter(time_opt == max_time_opt) %>%
    dplyr::select(year, week, category, stratum, upperbound_pad = upperbound, expected_pad = expected) %>%
    head(n = -(number_of_weeks - 1))

  # preparing dataset with padding
  if (is.null(stratification)) {
    result_padding <- result_padding_unstratified
  } else {
    result_padding_stratified <- SignalDetectionTool::get_signals(
      data = data,
      method = method,
      date_var = "date_report",
      stratification = stratification,
      number_of_weeks = (max_time_opt + number_of_weeks)
    ) %>%
      dplyr::select(year, week, upperbound_pad = upperbound, expected_pad = expected, category, stratum) %>%
      dplyr::group_by(category, stratum) %>%
      dplyr::slice_head(n = -(number_of_weeks - 1)) %>%
      dplyr::ungroup()

    result_padding <- dplyr::bind_rows(
      result_padding_stratified,
      result_padding_unstratified
    )
  }

  # preparing dataset within actual signal detection period
  results <- signals %>%
    dplyr::arrange(category, stratum, year, week) %>%
    dplyr::left_join(x = ., y = result_padding, by = c("category", "stratum", "year", "week"))

  # adjusting padding that the first upperbound which is calculated in the signals is set to the last upperbound padding such that no jump in the visualisation occurs
  results <- results %>%
    dplyr::group_by(category, stratum) %>%
    dplyr::mutate(first_timepoint_alarms = min(which(!is.na(alarms)))) %>%
    dplyr::mutate(first_alarm_nonNA = dplyr::if_else(dplyr::row_number() == first_timepoint_alarms, TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-first_timepoint_alarms) %>%
    dplyr::mutate(
      upperbound_pad = dplyr::if_else(first_alarm_nonNA, upperbound, upperbound_pad),
      expected_pad = dplyr::if_else(first_alarm_nonNA, expected, expected_pad)
    )


  return(results)
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

#' Add missing isoweeks to an aggregated dataframe of case counts by year and week
#'
#' This function takes a data frame containing year-week and case count and ensures
#' that it includes all possible year-week combinations within the specified
#' range. It fills in missing rows with 0 values for cases and returns the
#' updated data frame.
#'
#' @param data_agg An aggregated data frame containing at least the columns 'year','week','cases' representing the isoyear, isoweek and case counts.
#' @param date_start A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the minimum date of the dataset. This parameter can be used when the dataset should be extended until the date_start provided.
#' @param date_end A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the maximum date of the dataset. This can be used when the dataset should be extended until the date_end provided.
#' @return A data frame containing all year-week combinations within the input
#'   range, with previously missing year-weeks filled in with 0 values for cases.
#'
#' @examples
#' \dontrun{
#' data_agg <- data.frame(
#'   year = c(2021, 2022, 2022),
#'   week = c(1, 2, 4),
#'   cases = c(10, 15, 5)
#' )
#' updated_data <- add_missing_isoweeks(data_agg, "2022-01-21", "2023-05-01")
#' updated_data <- add_missing_isoweeks(data_agg)
#' updated_data
#' }
add_missing_isoweeks <- function(data_agg, date_start = NULL, date_end = NULL) {
  checkmate::assert(
    checkmate::check_subset("year", names(data_agg)),
    checkmate::check_subset("week", names(data_agg)),
    checkmate::check_subset("cases", names(data_agg)),
    combine = "and"
  )

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

  # add a date based on isoweek and isoyear
  data_agg <- data_agg %>%
    dplyr::mutate(date = isoweek_to_date(week, year))

  # extend the dataset nevertheless to min date if date_start greater
  min_date <- min(data_agg$date)
  if (is.null(date_start)) {
    date_start <- min_date
  } else if (date_start > min_date) {
    message("Notice: Your input date_start is greater than the smallest date in the dataset. Missing weeks are nevertheless filled until the smallest date in the dataset")
    date_start <- min_date
  }
  # extend the dataset nevertheless to max date if date_end is smaller
  max_date <- max(data_agg$date)
  if (is.null(date_end)) {
    date_end <- max_date
  } else if (date_end < max_date) {
    message("Notice: Your input date_end is smaller than the greatest date in the dataset. Missing weeks are nevertheless filled until the greatest date in the dataset")
    date_end <- max_date
  }

  # Generate a sequence of dates from start to end date
  # we add the date_end because the seq ends before date_end when there is a partial week remaining and we also want to have the isoweek of the date_end
  date_seq <- c(seq.Date(from = as.Date(date_start), to = as.Date(date_end), by = "week"), date_end)
  # Create a data frame with ISO weeks and ISO years
  df_all_years_weeks <- data.frame(
    year = lubridate::isoyear(date_seq),
    week = lubridate::isoweek(date_seq)
  ) %>%
    # in case date_end is there twice due to adding before
    dplyr::distinct(year, week)


  # Merge the template with the original data to fill in missing rows
  data_agg_complete <- merge(data_agg, df_all_years_weeks, by = c("year", "week"), all = TRUE) %>%
    dplyr::select(-date)
  # Replace missing cases with 0
  data_agg_complete$cases[is.na(data_agg_complete$cases)] <- 0

  data_agg_complete <- data_agg_complete %>%
    dplyr::arrange(year, week)

  return(data_agg_complete)
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
