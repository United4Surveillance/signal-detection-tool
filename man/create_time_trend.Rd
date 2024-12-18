% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_algorithm.R
\name{create_time_trend}
\alias{create_time_trend}
\title{Create a data.frame with variable time trend for regression modeling.}
\usage{
create_time_trend(
  ts_len,
  intervention_start = NULL,
  min_timepoints_trend = 12,
  past_weeks_not_included = 4
)
}
\arguments{
\item{ts_len}{integer, specifying the length of the time series.}

\item{intervention_start}{integer, default NULL, specifying the row number in the time series corresponding to an intervention date. If NULL no intervention is modeled.}

\item{min_timepoints_trend}{integer, default 12, specifying the minimum number of time points required after the intervention to fit a new time trend.}

\item{past_weeks_not_included}{An integer specifying the number of past weeks to exclude from
the fitting process. This can be useful for excluding recent data with outbreaks or data that may not be fully reported.
Default is `4`.}
}
\value{
A data frame with columns representing the time trend before and after the intervention (if applicable).
}
\description{
This function generates a data.frame that contains a time trend, which can be used in modeling to account for changes over time. Based on intervention_start there can be two time trend variables. The first one stays constant from the intervention_start onwards.
}
\examples{
\dontrun{
create_time_trend(100)
create_time_trend(100, intervention_start = 50)
}
}
