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
  # Convert the date columns to date format
  yes_no_unknown_vars <- intersect(colnames(data), yes_no_unknown_variables())
  # get all variables present in the data which might need transformation tolower
  to_lower_vars <- intersect(colnames(data), c(yes_no_unknown_variables(), "sex"))
  # get all regional stratification variables
  regional_id_vars <- intersect(colnames(data), region_id_variable_names())
  # get all variables that are characters and not case_id or date
  factorization_vars <- dplyr::select(data, dplyr::where(is.character) &
                                        !dplyr::starts_with("date") &
                                        !dplyr::any_of(regional_id_vars) &
                                        !case_id) %>% names


  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_lower_vars), ~ tolower(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "unknown"))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "NA"))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date"), ~ as.Date(.x, optional = T))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(regional_id_vars), ~ as.character(.x))) %>%
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

  # age or age_group is mandatory thus we need to check whether column present in data
  if ("age_group" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(age_group = factor(age_group,
        levels = stringr::str_sort(unique(data$age_group), numeric = TRUE)
      ))
  }
  # sex is not mandatory
  if ("sex" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(sex = factor(sex))
  }

  data
}

#' Aggregates case data by year and week
#' @param data data.frame, linelist of cases to be aggregated
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @examples
#' \dontrun{
#' data <- preprocess_data(input_example) %>% aggregate_data()
#' }
aggregate_data <- function(data,
                           date_var = "date_report") {
  week_var <- paste0(date_var, "_week")
  year_var <- paste0(date_var, "_year")

  data %>%
    dplyr::group_by(!!rlang::sym(week_var), !!rlang::sym(year_var)) %>%
    dplyr::summarize(cases = dplyr::n(), .groups = "drop") %>%
    dplyr::select(
      week = !!rlang::sym(week_var),
      year = !!rlang::sym(year_var),
      .data$cases
    )
}

#' Aggregate cases and signals over the number of weeks.

#' First the signals are filtered to obtain the signals for the last n weeks
#' aggregating the number of cases observed, create variable any alarm generated and the aggregate the number of alarms

#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and alarm per week, year
#' @param number_of_weeks integer, specifying the number of weeks we want to aggregate the number of cases and the generated alarms
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
    dplyr::group_by(category,stratum) %>%
    dplyr::summarise(
      cases = sum(cases, na.rm = T),
      any_alarms = any(alarms, na.rm = T),
      n_alarms = sum(alarms, na.rm = T)
    ) %>%
    dplyr::ungroup()
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

#' Add rows of missing dates to a Year-Week Data Frame
#'
#' This function takes a data frame containing year-week information and ensures
#' that it includes all possible year-week combinations within the specified
#' range. It fills in missing rows with 0 values for cases and returns the
#' updated data frame.
#'
#' @param data A data frame with columns 'year' and 'week' representing the year
#'   and week values for each observation.
#' @param date_start A date object or character of format yyyy-mm-dd
#' @param date_end A date object or character of format yyyy-mm-dd
#'
#' @return A data frame containing all year-week combinations within the input
#'   range, with missing rows filled in with 0 values for cases.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   year = c(2021, 2022, 2022),
#'   week = c(1, 2, 4),
#'   cases = c(10, 15, 5)
#' )
#' # updated_data <- add_rows_missing_dates(data, "2022-01-21", "2023-05-01")
#' updated_data <- add_rows_missing_dates(data)
#' updated_data
#' }
add_rows_missing_dates <- function(data, date_start = NULL, date_end = NULL) {
  checkmate::assert(
    checkmate::check_subset("year", names(data)),
    checkmate::check_subset("week", names(data)),
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

  if (is.null(date_start)) {
    min_year_week <- min(data$year * 100 + data$week)
  } else {
    min_year_week <- lubridate::isoyear(date_start) * 100 +
      lubridate::isoweek(date_start)
  }
  if (is.null(date_end)) {
    max_year_week <- max(data$year * 100 + data$week)
  } else {
    max_year_week <- lubridate::isoyear(date_end) * 100 +
      lubridate::isoweek(date_end)
  }


  # Create a template data frame with all year-week combinations in specified
  # timeframe
  template <- expand.grid(
    year = floor(min_year_week / 100):floor(max_year_week / 100),
    week = 1:52
  )

  # Merge the template with the original data to fill in missing rows
  result <- merge(data, template, by = c("year", "week"), all = TRUE) %>%
    dplyr::mutate("yearweek" = .data$year * 100 + .data$week) %>%
    dplyr::filter((.data$yearweek >= min_year_week) & (.data$yearweek <= max_year_week)) %>%
    dplyr::select(-c("yearweek"))

  # Replace missing cases with 0
  result$cases[is.na(result$cases)] <- 0

  return(result)
}

#' Filter the data so that only the data of the last n weeks are returned
#' This function can be used to filter for those last n weeks where alarms were generated. For this we could use is.na(alarms) but because it might happen that algorithms generate NA for an alarm this function is the safer option.
#' @param data_agg data.frame, aggregated surveillance or signals dataset, where aggregated means no linelist but cases or signals per week, year
#' @param number_of_weeks integer, specifying the number of weeks from the most recent week we want to filter the data for
#' @returns data.frame, aggregated data of last n weeks
filter_data_last_n_weeks <- function(data_agg,
                                     number_of_weeks) {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  data_agg %>%
    dplyr::group_by(category,stratum) %>%
    dplyr::arrange(year, week) %>%
    dplyr::slice_tail(n = number_of_weeks) %>%
    dplyr::ungroup()
}
