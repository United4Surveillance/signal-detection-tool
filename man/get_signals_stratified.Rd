% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_functions.R
\name{get_signals_stratified}
\alias{get_signals_stratified}
\title{Get Signals Stratified}
\usage{
get_signals_stratified(
  data,
  fun,
  model = "",
  intervention_date = NULL,
  time_trend = FALSE,
  stratification_columns,
  date_start = NULL,
  date_end = NULL,
  date_var = "date_report",
  number_of_weeks = 52
)
}
\arguments{
\item{data}{A data frame containing the surveillance data.}

\item{fun}{The signal detection function to apply to each stratum.}

\item{model}{character, default empty string which is the choice if farrington, ears or cusum are used and if a glm method was chosen as outbreak detection method then one of c("mean","sincos", "FN")}

\item{intervention_date}{A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.}

\item{time_trend}{boolean default TRUE setting time_trend in the get_signals_glm(). This parameter is only used when an the glm based outbreak detection models are used, i.e. for the models c("mean","sincos", "FN")}

\item{stratification_columns}{A character vector specifying the columns to
stratify the data by.}

\item{date_start}{A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.}

\item{date_end}{A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.}

\item{date_var}{a character specifying the date variable name used for the aggregation. Default is "date_report".}

\item{number_of_weeks}{integer, specifying number of weeks to generate signals for.}
}
\value{
A tibble containing the results of the signal detection analysis
  stratified by the specified columns.
}
\description{
This function stratifies and aggregates surveillance data by specified columns and analyzes
each stratum separately using the specified method.
}
\examples{
\dontrun{
data <- read.csv("../data/input/input.csv")
categories <- c("county", "sex", "age_group") # Replace with actual column names
results <- get_signals_stratified(
  data,
  fun = get_signals_farringtonflexible,
  stratification_columns = categories
)
print(results)
}
}
