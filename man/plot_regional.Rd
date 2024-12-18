% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_regional.R
\name{plot_regional}
\alias{plot_regional}
\title{Plot number of cases with number of signals by region}
\usage{
plot_regional(
  shape_with_signals,
  signals_agg_unknown_region = NULL,
  interactive = FALSE,
  toggle_alarms = FALSE
)
}
\arguments{
\item{shape_with_signals}{sf shapefile, with additional columns from signals cases, n_alarms, any_alarms}

\item{signals_agg_unknown_region}{tibble default NULL, if not NULL tibble containing only the row for signals_agg for the missing regions (is.na(stratum)) with the columns cases and n_alarms which are used for creating the annotation text below the map}

\item{interactive}{boolean identifying whether the plot should be static or interactive}

\item{toggle_alarms}{boolean identifying whether the plot should showing number of signals explicitly or only when hovering}
}
\value{
either a ggplot object if static plot is chosen or a plotly object for the interactive plot
}
\description{
Plot number of cases with number of signals by region
}
