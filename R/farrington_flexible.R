
preprocess_data <- function(data) {
  # Convert the date columns to date format
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date"), ~ as.Date(.x, optional = T)))

  # add columns for isoyear and isoweek for each date
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
                                ~ surveillance::isoWeekYear(.x)$ISOYear,
                                .names = "{.col}_year")) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
                                ~ surveillance::isoWeekYear(.x)$ISOWeek,
                                .names = "{.col}_week"))

  data
}

#' Aggregates case data by year and week of onset
#' @param data data frame to be converged
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data <- preprocess_data(data) %>% aggregate_data()
#' sts_cases <- convert_to_sts(data)
#' }
aggregate_data <- function(data) {
  data %>%
    dplyr::group_by(.data$date_onset_year, .data$date_onset_week) %>%
    dplyr::summarize(cases = dplyr::n()) %>%
    dplyr::select(year = .data$date_onset_year,
                  week = .data$date_onset_week,
                  .data$cases)
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
#' data <- data.frame(year = c(2021, 2022, 2022),
#'                    week = c(1, 2, 4),
#'                    cases = c(10, 15, 5))
#' # updated_data <- add_rows_missing_dates(data, "2022-01-21", "2023-05-01")
#' updated_data <- add_rows_missing_dates(data)
#' updated_data
#' }
add_rows_missing_dates <- function(data, date_start=NULL, date_end=NULL) {

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
  }
  else{
    min_year_week <- lubridate::isoyear(date_start) * 100 +
      lubridate::isoweek(date_start)
  }
  if (is.null(date_end)) {
    max_year_week <- max(data$year * 100 + data$week)
  }
  else{
    max_year_week <- lubridate::isoyear(date_end) * 100 +
      lubridate::isoweek(date_end)
  }


  # Create a template data frame with all year-week combinations in specified
  # timeframe
  template <- expand.grid(
    year = floor(min_year_week/100):floor(max_year_week/100),
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


#' Get signals of surveillance's farringtonFlexible algorithm
#' @param surveillance_data data frame to be converged
#' @param date_start A date object or character of format yyyy-mm-dd
#' @param date_end A date object or character of format yyyy-mm-dd
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' results <- get_signals_farringtonflexible(data)
#' }
get_signals_farringtonflexible <- function(surveillance_data,
                                           date_start=NULL,
                                           date_end=NULL) {


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

  data <- preprocess_data(surveillance_data) %>%
    aggregate_data() %>%
    add_rows_missing_dates(date_start, date_end)

  sts_cases <- convert_to_sts(data)

  num_weeks <- length(sts_cases@observed)
  num_years <- floor((num_weeks - 26) / 52) - 1

  if (num_years <= 0) {
    warning(paste0(
      "Your input data/stratification only stretches over ", num_weeks,
      " weeks. FarringtonFlexible needs at least a year of data to calibrate an ",
      " epidemiological baseline."
    ))
    return(NULL)
  }

  control <- list(
    range = ((num_weeks - 51):num_weeks),
    noPeriods = 10, populationOffset = FALSE,
    fitFun = "algo.farrington.fitGLM.flexible",
    b = num_years, w = 3, weightsThreshold = 2.58,
    pastWeeksNotIncluded = 26,
    pThresholdTrend = 1, trend = TRUE,
    thresholdMethod = "delta", alpha = 0.1
  )

  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases, control)

  pad <- rep(NA, num_weeks - 52)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)
  expected <- c(pad, results@control$expected)

  data$alarms <- alarms
  data$upperbound <- upperbound
  data$expected <- expected

  return(data)
}
