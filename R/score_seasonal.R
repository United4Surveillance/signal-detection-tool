#' @title Score signals according to seasonal distribution of cases
#' @description Scores each signal in the signal detection results according to the
#' calculated seasonal distribution. See [case_yearly_dist]
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
#'   dplyr::mutate(pathogen = "Pertussis", .row_id = dplyr::row_number())
#'
#' score_seasonal(signals)
#' }
score_seasonal <- function(signals_res) {
  # select the unstratified
  ts_pathogen <- signals_res %>% dplyr::filter(is.na(category))

  # select years that are complete (52, 53 weeks)
  sel_years <- ts_pathogen %>%
    dplyr::count(year) %>%
    dplyr::filter(n >= 52) %>%
    dplyr::pull(year)

  # if no complete years, seasonal score is skipped and returns NA
  if (length(sel_years) == 0) {
    signals_res <- signals_res %>%
      dplyr::mutate(score = NA)
  } else {
    # generate cases distribution
    scores_per_month <- case_yearly_dist(ts_pathogen, sel_years) %>%
      dplyr::select(-"cases.dist")

    # score_signals
    signals_res <- signals_res %>%
      dplyr::mutate(
        month = factor(
          lubridate::month(isoweek_to_date(.data$week, .data$year)),
          levels = 1:12
        )
      ) %>%
      dplyr::left_join(scores_per_month, by = c("month")) %>%
      dplyr::mutate(
        score = dplyr::case_when(
          .data$alarms ~ .data$score,
          .default = NA
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
#' @param dat_ts timeseries dataframe. Must include the columns year, week, and cases
#' @param selected_years vector of years to filter the data
#'
#' @returns dataframe with the empirical distribution of cases in the year and its corresponding score
#' @export
#'
case_yearly_dist <- function(dat_ts, selected_years) {
  # per year, group total cases in month, normalize against total cases in year,
  # calculate mean cases across years for every month
  # score is 1 - cases.scaledpercentage

  checkmate::assert_numeric(selected_years)

  cases_dist <- dat_ts %>%
    dplyr::filter(.data$year %in% selected_years) %>%
    dplyr::mutate(
      month = factor(
        lubridate::month(isoweek_to_date(.data$week, .data$year)),
        levels = 1:12
      )
    ) %>%
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
      score = 1 - .data$cases.dist.scaled
    )

  return(cases_dist)
}
