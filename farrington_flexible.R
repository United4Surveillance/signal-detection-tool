library(surveillance)
library(tidyverse)

# turn data into sts object
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
    dplyr::summarize(cases = n()) %>%
    dplyr::select(year = date_onset_year, week = date_onset_week, cases)
}

get_signals_farringtonflexible <- function(surveillance_data){
  
  data <- preprocess_data(surveillance_data) %>% aggregate_data()
  
  sts_cases <- convert_to_sts(data)
  
  num_weeks <- length(sts_cases@observed)
  num_years <- floor((num_weeks - 26)/52)-1
  
  control  <- list(range=((num_weeks-51):num_weeks),
                   noPeriods=10,populationOffset=FALSE,
                   fitFun="algo.farrington.fitGLM.flexible",
                   b=num_years,w=3,weightsThreshold=2.58,
                   pastWeeksNotIncluded=26,
                   pThresholdTrend=1,trend=TRUE,
                   thresholdMethod="delta",alpha=0.1)
  
  
  # run Farrington Flexible on data
  results <- surveillance::farringtonFlexible(sts_cases, control)
  
  pad <- rep(NA,num_weeks-52)
  alarms <- c(pad,results@alarm)
  upperbound <- c(pad, results@upperbound)
  expected <- c(pad, results@control$expected)
  
  data$alarms <- alarms
  data$upperbound <- upperbound
  data$expected <- expected
  
  return(data)
}
