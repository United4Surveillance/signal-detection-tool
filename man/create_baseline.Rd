% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_algorithm.R
\name{create_baseline}
\alias{create_baseline}
\title{Create a data.frame with a constant baseline for an intercept only regression model.}
\usage{
create_baseline(
  ts_len,
  intervention_start = NULL,
  min_timepoints_baseline = 12,
  past_weeks_not_included = 4
)
}
\arguments{
\item{ts_len}{integer, specifying the length of the time series.}

\item{intervention_start}{integer, default NULL, specifying the row number in the time series corresponding to an intervention date. When NULL no intervention is modeled.}

\item{min_timepoints_baseline}{integer, default 12, specifying the minimum number of time points required after the intervention to fit a new baseline.}

\item{past_weeks_not_included}{An integer specifying the number of past weeks to exclude from
the fitting process. This can be useful for excluding recent data with outbreaks or data that may not be fully reported.
Default is `4`.}
}
\value{
NULL when intervention_start = NULL, otherwise a data frame with a column representing the baseline before and after the intervention (if applicable).

A data frame with columns representing the time trend before and after the intervention (if applicable).
\dontrun{
create_baseline(100)
create_baseline(100,intervention_start = 50)
}
}
\description{
This function generates a baseline data.frame for use in modeling, which can account for different baselines before and after an intervention. If no intervention is done (intervention_start = NULL) NULL is returned as the regression can just use ~1 in the formula.
}
