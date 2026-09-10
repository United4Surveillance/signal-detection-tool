#' @title Score signals according to seasonal distribution of cases
#' @description Scores each signal in the signal detection results according to the
#' calculated seasonal distribution. This distribution is calculated by the average percentage of
#' cases that happen each month for each year of complete data. The score is defined by 1 - scaled percentage,
#' were scaled percentage is given by an s-curve \eqn{f(x)=x^2/(1/12^2 + x^2)}. Then it is rounded to the 2nd decimal point.
#'
#' @param signals_res signal detection results obtained from get_signals()
#'
#' @returns signal detection results scored
#' @export
#'
#' @examples \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals() %>%
#'   dplyr::mutate(pathogen = "Pertussis")
#'
#' get_scores(signals, scorers = list(seasonal = score_seasonal))
#' }
score_seasonal <- function(signals_res) {
  # select the unstratified
  ts_pathogen <- signals_res %>% dplyr::filter(is.na(category))

  # select years that are complete
  time_unit <- unique(signals_res$time_unit)
  required_time_units <- dplyr::case_when(
    length(time_unit) == 1L && time_unit == "weekly" ~ 52L,
    length(time_unit) == 1L && time_unit == "biweekly" ~ 26L,
    length(time_unit) == 1L && time_unit == "monthly" ~ 12L,
    TRUE ~ NA_integer_
  )

  if (is.na(required_time_units)) {
    stop(
      "`time_unit` must contain exactly one of: ",
      "`weekly`, `biweekly`, or `monthly`."
    )
  }

  sel_years <- ts_pathogen %>%
    dplyr::count(year) %>%
    dplyr::filter(n >= required_time_units) %>%
    dplyr::pull(year)

  # if no complete years, seasonal score is skipped and returns NA
  if (length(sel_years) == 0) {
    signals_res <- signals_res %>%
      dplyr::mutate(score = NA_real_)
  } else {
    # generate cases distribution
    scores_per_month <- case_yearly_dist(ts_pathogen, sel_years) %>%
      dplyr::select(-"cases.dist")

    # score_signals
    # Assign each weekly or biweekly period to the month of its reference week.
    # Periods spanning two months are not split proportionally between months.
    if (time_unit %in% c("weekly", "biweekly")) {
      signals_res <- signals_res %>%
        dplyr::mutate(
          month = factor(
            lubridate::month(isoweek_to_date(.data$week, .data$year)),
            levels = 1:12
          )
        )
    } else if (time_unit == "monthly") {
      signals_res <- signals_res %>%
        dplyr::mutate(
          month = factor(.data$month, levels = 1:12)
        )
    }

    signals_res <- signals_res %>%
      dplyr::left_join(scores_per_month, by = c("month")) %>%
      dplyr::mutate(
        score = dplyr::case_when(
          .data$alarms ~ .data$score,
          .default = NA_real_
        )
      )
  }

  return(signals_res %>% dplyr::select(c(".row_id", "score")))
}


#' @title Calculate case seasonal distribution for scoring
#' @description For each year of complete data, it calculates the average percentage of
#' cases that happen each month. The score is defined by 1 - scaled percentage,
#' were scaled percentage is given by an s-curve \eqn{f(x)=x^2/(1/12^2 + x^2)}
#'
#' @param dat_ts timeseries dataframe. Must include the columns year, week/month, and cases
#' @param selected_years vector of years to filter the data
#'
#' @returns dataframe with the empirical distribution of cases in the year and its corresponding score
#' @noRd
#'
case_yearly_dist <- function(dat_ts, selected_years) {
  # per year, group total cases in month, normalize against total cases in year,
  # calculate mean cases across years for every month
  # score is 1 - cases.scaledpercentage

  checkmate::assert_numeric(selected_years)

  time_unit <- unique(dat_ts$time_unit)

  cases_dist <- dat_ts %>%
    dplyr::filter(.data$year %in% selected_years)

  if (all(time_unit %in% c("weekly", "biweekly"))) {
    cases_dist <- cases_dist %>%
      dplyr::mutate(
        month = factor(
          lubridate::month(isoweek_to_date(.data$week, .data$year)),
          levels = 1:12
        )
      )
  } else if (all(time_unit == "monthly")) {
    cases_dist <- cases_dist %>%
      dplyr::mutate(
        month = factor(.data$month, levels = 1:12)
      )
  }

  cases_dist <- cases_dist %>%
    dplyr::group_by(.data$year, .data$month) %>%
    dplyr::summarise(
      cases = sum(.data$cases),
      .groups = "drop"
    ) %>%
    tidyr::complete(.data$year, .data$month, fill = list(cases = 0)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(cases_perc = .data$cases / sum(.data$cases)) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::summarise(
      cases.dist = mean(.data$cases_perc),
      .groups = "drop"
    )

  # scale distribution
  # If no seasonal, expected proportion each month should be 1/12
  # proportions are scaled with an s-curve, where score(p = 1/12) = 0.5
  cases_dist <- cases_dist %>%
    dplyr::mutate(
      cases.dist.scaled = (.data$cases.dist^2) / (1 / 12^2 + .data$cases.dist^2),
      score = round((1 - .data$cases.dist.scaled), digits = 2)
    )

  return(cases_dist)
}
