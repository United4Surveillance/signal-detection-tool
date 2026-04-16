#' @title Score signals according to seasonal distribution of cases
#' @description Scores each signal in the signals_pad object according to the
#' calculated seasonal distribution. See [case_yearly_dist]
#'
#' @param signals_pad, signal detection results obtained from get_signals()
#'
#' @returns signal detection results scored
#' @export
#'
#' @examples \dontrun{
#' signals <- input_example %>% preprocess_data() %>% get_signals() %>% dplyr::mutate(pathogen = "Pertussis")
#'
#' score_seasonal(signals)
#' }
score_seasonal <- function(signals_pad){

  # select the unstratified
  ts_pathogen <- signals_pad %>% dplyr::filter(is.na(category))

  # select years that are complete (52, 53 weeks)
  sel_years <- ts_pathogen %>%
    dplyr::count(year) %>%
    dplyr::filter(n >= 52) %>%
    dplyr::pull(year)

  # generate cases distribution
  scores_per_month <- case_yearly_dist(ts_pathogen, sel_years) %>%
    dplyr::select(-"cases.dist")

  # score_signals
  signals_pad <- signals_pad %>%
    dplyr::mutate(
      month = factor(
        lubridate::month(isoweek_to_date(.data$week, .data$year)),
        levels = 1:12
      )
    ) %>%
    dplyr::left_join(scores_per_month, by = c("month")) %>%
    dplyr::mutate(
      score = dplyr::case_when(
        is.na(.data$alarms) ~ NA,
        !alarms ~ NA,
        .default = .data$score
      )
    )

  return(signals_pad %>% dplyr::select(c(.row_id, score)))
}


#' @title Calculate case seasonal distribution for scoring
#' @description For each year of complete data, it calculates the average percentage of
#' cases that happen each month. The score is defined by 1 - percentage
#'
#' @param dat_ts timeseries dataframe. Must include the columns pathogen, year, and week
#' @param selected_years vector of years to filter the data
#'
#' @returns dataframe with the empirical distribution of cases in the year and its corresponding score
#' @export
#'
case_yearly_dist <- function(dat_ts, selected_years){
  # per year, group total cases in month, normalize against total cases in year,
  # calculate mean cases across years for every month
  # score is 1 - cases.percentage

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
    tidyr::complete(.data$month, fill = list(cases = 0, cases_in_outbreak = 0)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(cases_perc = .data$cases/sum(.data$cases)) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::summarise(
      cases.dist = mean(.data$cases_perc),
      score = 1 - .data$cases.dist,
      .groups = "drop")

  return(cases_dist)
}
