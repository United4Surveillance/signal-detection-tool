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

get_signals_farringtonflexible <- function(surveillance_data){
  sts_cases <- convert_to_sts(data)
  
  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases)
  
  return(results)
}
