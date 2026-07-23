#' Filter linelist by reporting period and optional stratum
#'
#' Filters a case linelist to a given reporting date interval. If a stratum
#' definition is provided, the linelist is additionally filtered to the
#' corresponding category-stratum combination.
#'
#' @param linelist A data frame containing case-level surveillance data.
#'   Must contain a `date_report` column.
#' @param start_date Start date of the reporting period. Rows with
#'   `date_report >= start_date` are retained.
#' @param end_date End date of the reporting period. Rows with
#'   `date_report <= end_date` are retained.
#' @param signal_row Optional one-row data frame from the signal results
#'   containing the columns `category` and `stratum`. If provided and both
#'   values are not `NA`, the linelist is filtered to rows where the column
#'   named in `category` equals `stratum`
#'
#' @return A filtered data frame containing linelist rows within the selected
#'   reporting period and, if applicable, the selected stratum.
filter_linelist_by_period_and_stratum <- function(linelist, start_date, end_date, signal_row = NULL) {
  checkmate::assert(
    checkmate::check_null(signal_row),
    checkmate::check_data_frame(signal_row, nrows = 1),
    combine = "or"
  )

  filtered_df <- linelist %>%
    dplyr::filter(date_report >= start_date & date_report <= end_date)

  # filtering for the specific stratum in the linelist
  if (!is.null(signal_row)) {
    category <- signal_row$category
    stratum <- signal_row$stratum
    if (!is.na(category) && !is.na(stratum)) {
      filtered_df <- filtered_df %>%
        dplyr::filter(!!rlang::sym(category) == stratum)
    }
  }

  filtered_df
}

#' Filter linelist to the signal detection period
#'
#' Determines the date range covered by the time units in `signals_padded` for which
#' signal results are available, and filters the linelist to this period.
#'
#' The start of each time unit is calculated from the `year` and `week` or `year` and `month` columns respectively.
#' The returned linelist covers the period from the Monday of the first signal
#' week to the Sunday of the last signal week, if weeks are selected as time unit.
#'
#' @param signals_padded A data frame containing padded signal detection results.
#'   Must contain `year`, `week` or `month`, and `alarms` columns.
#' @param linelist A data frame containing case-level surveillance data.
#'   Must contain a `date_report` column.
#'
#' @return A filtered linelist containing cases reported during the signal
#'   detection period.
filter_rows_past_time_units <- function(signals_padded, linelist) {
  number_of_time_units <- unique(signals_padded$number_of_time_units)
  time_unit <- unique(signals_padded$time_unit)

  # solve zero signals (entails zero time_unit-values) problem
  if (length(time_unit) != 1 || is.na(time_unit)) {
    return(linelist[0, ])
  }

  if (time_unit %in% c("weekly", "biweekly")) {
    signals_padded_n_time_units <- signals_padded %>%
      dplyr::filter(!is.na(alarms)) %>%
      dplyr::mutate(
        week_start = ISOweek::ISOweek2date(
          paste0(year, "-W", sprintf("%02d", week), "-1")
        )
      )
    start_date <- min(signals_padded_n_time_units$week_start)
    if (time_unit == "weekly") {
      end_date <- max(signals_padded_n_time_units$week_start) + lubridate::days(6)
    } else {
      end_date <- max(signals_padded_n_time_units$week_start) + lubridate::days(13)
    }
  } else if (time_unit == "monthly") {
    signals_padded_n_time_units <- signals_padded %>%
      dplyr::filter(!is.na(alarms)) %>%
      dplyr::mutate(
        month_start = lubridate::make_date(
          year = year,
          month = month,
          day = 1
        )
      )

    start_date <- min(signals_padded_n_time_units$month_start)

    end_date <- max(signals_padded_n_time_units$month_start) %>%
      lubridate::ceiling_date(unit = "month") - lubridate::days(1)
  }

  filter_linelist_by_period_and_stratum(linelist, start_date, end_date)
}

#' Filter linelist to one signal week and stratum
#'
#' Filters a linelist to the ISO week represented by one row of signal results.
#' If the signal row contains a non-missing `category` and `stratum`, the
#' linelist is additionally restricted to that stratum.
#'
#' @param signal_row A one-row data frame from weekly aggregated signal results
#'   containing at least `year` and `week`/`month`. May also contain `category` and
#'   `stratum` for stratified signals.
#' @param linelist A data frame containing a linelist of surveillance data. Must contain
#'   a `date_report` column and, for stratified signals, the column named in
#'   `signal_row$category`, i.e. "age_group" or "sex".
#'
#' @return A filtered linelist containing cases reported during the selected
#'   signal week and, if applicable, matching the selected stratum.
filter_rows_signal_time_unit <- function(signal_row, linelist) {
  time_unit <- unique(signal_row$time_unit)

  # solve zero signals (entails zero time_unit-values) problem
  if (length(time_unit) != 1 || is.na(time_unit)) {
    return(linelist[0, ])
  }

  if (time_unit %in% c("weekly", "biweekly")) {
    start_date <- ISOweek::ISOweek2date(
      paste0(signal_row$year, "-W", sprintf("%02d", signal_row$week), "-1")
    )

    if (time_unit == "weekly") {
      end_date <- start_date + lubridate::days(6)
    } else {
      end_date <- start_date + lubridate::days(13)
    }
  } else if (time_unit == "monthly") {
    start_date <- lubridate::make_date(
      year = signal_row$year,
      month = signal_row$month,
      day = 1
    )

    end_date <- lubridate::ceiling_date(start_date, unit = "month") -
      lubridate::days(1)
  }

  filter_linelist_by_period_and_stratum(linelist, start_date, end_date, signal_row)
}

#' Build linelists for selected signal cases and comparison cases
#'
#' Creates two linelists for visualisation or case review:
#' one containing cases that occurred in selected signals, and one
#' comparison linelist containing cases from the signal detection period
#' that are not already included in the selected signal cases.
#'
#' @param selected_signal_ids Integer vector of row positions in `true_signals`
#'   identifying the signal rows selected by the user.
#' @param true_signals A data frame containing signal rows, typically filtered
#'   to true or selected signals. Must contain `year` and `week`/`month`, and may contain
#'   `category` and `stratum`.
#' @param signals_padded A data frame containing padded signal detection results.
#'   Used to determine the full comparison period. Must contain `year`, `week`/`month`,
#'   and `alarms`.
#' @param filtered_data A case linelist after applying the current app filters.
#'   Must contain `case_id` and `date_report`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{cases}{A data frame of cases from the selected signal weeks, with a
#'   `signal_id` column added.}
#'   \item{cases_comparison}{A data frame of cases from the wider signal
#'   detection period, excluding cases already present in `cases`.}
#' }
build_signal_and_comparison_linelist <- function(selected_signal_ids, true_signals, signals_padded, filtered_data) {
  cases <- purrr::map_dfr(selected_signal_ids, function(ssid) {
    signal_row <- true_signals %>% dplyr::slice(ssid)

    filter_rows_signal_time_unit(signal_row, filtered_data) %>%
      dplyr::mutate(signal_id = ssid, .before = 1)
  })

  # deduplicate cases, add both signal_id numbers to those cases occuring in two signals
  cases <- cases %>%
    dplyr::group_by(case_id) %>%
    dplyr::summarise(
      signal_id = paste(sort(unique(signal_id)), collapse = ","),
      dplyr::across(-signal_id, dplyr::first),
      .groups = "drop"
    ) %>%
    dplyr::relocate(signal_id, .before = 1)


  cases_comparison <- filter_rows_past_time_units(signals_padded, filtered_data) %>%
    dplyr::anti_join(cases, by = "case_id")

  list(
    cases = cases,
    cases_comparison = cases_comparison
  )
}

#' Build table with summary statistics for selected signals vs rest
#' Calculates the median and quantiles for age, and the male/female ratio, for cases in the selected signals and for the rest of the cases within the signal detection period.
#'
#' @param comparison_linelist list containing two linelist data frames: `cases` and `cases_comparison`, as returned by `build_signal_and_comparison_linelist()`.
#'
#' @returns data frame with summary statistics for selected signals and rest of cases, formatted as strings.
build_comparison_summary <- function(comparison_linelist) {
  comparison_linelist <- comparison_linelist %>%
      dplyr::bind_rows(.id = "signal") %>%
      dplyr::mutate(signal = dplyr::if_else(signal == "cases", "cases.in.selected.signals", "rest.of.cases")) %>%
      dplyr::group_by(signal) 
  
  comparison_summary <- tibble::tibble(signal = c("cases.in.selected.signals", "rest.of.cases"))

  # checks age exist in dataframes
  if(checkmate::test_choice("age", names(comparison_linelist))){
    comparison_summary <- dplyr::left_join(
      comparison_summary,
      comparison_linelist %>%
        dplyr::summarise(
          `Q1 age` = as.character(quantile(age, 0.25, na.rm = TRUE)),
          `Median age` = as.character(median(age, na.rm = TRUE)),
          `Q3 age` = as.character(quantile(age, 0.75, na.rm = TRUE)),
          .groups = "drop"
        ), 
      by = "signal" 
    )
  } 

  # checks sex exist in dataframes
  if(checkmate::test_choice("sex", names(comparison_linelist))
  ){
    comparison_summary <- dplyr::left_join(
      comparison_summary, 
      comparison_linelist %>% 
        dplyr::summarise(
          `Male/Female ratio` = as.character(MASS::fractions(sum(sex == "male", na.rm = TRUE) / sum(sex == "female", na.rm = TRUE))),
          .groups = "drop"
        ), 
      by = "signal"
    )
  } 

  # ungroup and rearrange 
  if(ncol(comparison_summary) > 1){
      comparison_summary <- comparison_summary  %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_longer(-signal, names_to = "measure") %>% 
      tidyr::pivot_wider(id_cols = measure, names_from = signal)
  } else {
    comparison_summary <- tibble::tibble(
      measure = character(0),
      cases.in.selected.signals = character(0),
      rest.of.cases = character(0))
  }

  
  return(comparison_summary)
}
