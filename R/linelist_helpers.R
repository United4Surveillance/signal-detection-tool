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
#' Determines the date range covered by the weeks in `signals_padded` for which
#' signal results are available, and filters the linelist to this period.
#'
#' The start of each ISO week is calculated from the `year` and `week` columns.
#' The returned linelist covers the period from the Monday of the first signal
#' week to the Sunday of the last signal week.
#'
#' @param signals_padded A data frame containing padded signal detection results.
#'   Must contain `year`, `week`, and `alarms` columns.
#' @param linelist A data frame containing case-level surveillance data.
#'   Must contain a `date_report` column.
#'
#' @return A filtered linelist containing cases reported during the signal
#'   detection period.
filter_rows_past_weeks <- function(signals_padded, linelist) {
  number_of_weeks <- unique(signals_padded$number_of_weeks)
  signals_padded_n_weeks <- signals_padded |>
    dplyr::filter(!is.na(alarms)) |>
    dplyr::mutate(
      week_start = ISOweek::ISOweek2date(
        paste0(year, "-W", sprintf("%02d", week), "-1")
      )
    )

  start_date <- min(signals_padded_n_weeks$week_start)
  end_date <- max(signals_padded_n_weeks$week_start) + lubridate::days(6)

  filter_linelist_by_period_and_stratum(linelist, start_date, end_date)
}

#' Filter linelist to one signal week and stratum
#'
#' Filters a linelist to the ISO week represented by one row of signal results.
#' If the signal row contains a non-missing `category` and `stratum`, the
#' linelist is additionally restricted to that stratum.
#'
#' @param signal_row A one-row data frame from weekly aggregated signal results
#'   containing at least `year` and `week`. May also contain `category` and
#'   `stratum` for stratified signals.
#' @param linelist A data frame containing a linelist of surveillance data. Must contain
#'   a `date_report` column and, for stratified signals, the column named in
#'   `signal_row$category`, i.e. "age_group" or "sex".
#'
#' @return A filtered linelist containing cases reported during the selected
#'   signal week and, if applicable, matching the selected stratum.
filter_rows_signal_week <- function(signal_row, linelist) {
  start_date <- ISOweek::ISOweek2date(
    paste0(signal_row$year, "-W", sprintf("%02d", signal_row$week), "-1")
  )
  end_date <- start_date + lubridate::days(6)

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
#'   to true or selected signals. Must contain `year` and `week`, and may contain
#'   `category` and `stratum`.
#' @param signals_padded A data frame containing padded signal detection results.
#'   Used to determine the full comparison period. Must contain `year`, `week`,
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
    signal_row <- true_signals |> dplyr::slice(ssid)

    filter_rows_signal_week(signal_row, filtered_data) |>
      dplyr::mutate(signal_id = ssid, .before = 1)
  })

  # deduplicate cases, add both signal_id numbers to those cases occuring in two signals
  cases <- cases |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(
      signal_id = paste(sort(unique(signal_id)), collapse = ","),
      dplyr::across(-signal_id, dplyr::first),
      .groups = "drop"
    ) |>
    dplyr::relocate(signal_id, .before = 1)


  cases_comparison <- filter_rows_past_weeks(signals_padded, filtered_data) |>
    dplyr::anti_join(cases, by = "case_id")

  list(
    cases = cases,
    cases_comparison = cases_comparison
  )
}
