library(surveillance)
library(tidyverse)

# turn data into sts object
convert_to_sts <- function(data) {
  # Convert the 'date_onset' column to a date format
  data <- data %>%
    dplyr::mutate(date_onset = as.Date(date_onset)) %>%
    dplyr::arrange(date_onset)
  
  year_week <- surveillance::isoWeekYear(data$date_onset)
  
  # aggregate case counts per week
  case_counts <- data %>%
    dplyr::mutate(isoyear = year_week$ISOYear) %>%
    dplyr::mutate(isoweek = year_week$ISOWeek) %>%
    dplyr::group_by(isoyear, isoweek) %>%
    dplyr::summarize(cases = n())
  
  # create sts object
  return(surveillance::sts(case_counts$cases,
                           start = c(
                             case_counts$isoyear[1],
                             case_counts$isoweek[1]
                           ),
                           frequency = 52
  ))
}

preprocess_data <- function(data){
  
  # Convert the date columns to date format
  data <- data %>%
    dplyr::mutate(across(starts_with("date"), ~ as.Date(.x,optional=T)))
  
  # add columns for isoyear and isoweek for each date
  data <- data %>%
    dplyr::mutate(across(starts_with("date")&!where(is.numeric), ~ surveillance::isoWeekYear(.x)$ISOYear, .names = "{.col}_year")) %>%
    dplyr::mutate(across(starts_with("date")&!where(is.numeric), ~ surveillance::isoWeekYear(.x)$ISOWeek, .names = "{.col}_week"))
  
  data
}

aggregate_data <- function(data){
  
  data %>%
    dplyr::group_by(date_onset_year, date_onset_week) %>%
    dplyr::summarize(cases = n())
}

get_signals_farringtonflexible <- function(surveillance_data){
  sts_cases <- convert_to_sts(data)
  
  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases)
  
  return(results)
}
