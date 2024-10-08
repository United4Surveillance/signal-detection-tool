% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_checks.R
\name{is_ISO8601}
\alias{is_ISO8601}
\title{checking YYYYY-mm-dd format of date variables}
\usage{
is_ISO8601(date_col)
}
\arguments{
\item{date_col}{character vector, vector containing dates to check}
}
\value{
boolean, when TRUE all values of date_col are in the required format, when FALSE at least one is not in required format
}
\description{
checking YYYYY-mm-dd format of date variables
}
\examples{
\dontrun{
is_ISO8601(c("2023-04-01", "2022-12-31"))
}
}
