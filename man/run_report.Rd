% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_report.R
\name{run_report}
\alias{run_report}
\title{Renders signal detection report}
\usage{
run_report(
  report_format = "HTML",
  data = SignalDetectionTool::input_example,
  algo = "farrington",
  number_of_weeks = 6,
  strata = c("county_id", "community_id", "sex", "age_group"),
  interactive = TRUE,
  tables = TRUE,
  output_file = NULL,
  signals_padded = NULL,
  signals_agg = NULL
)
}
\arguments{
\item{report_format}{format of the report: HTML or DOCX}

\item{data}{data.frame containing surveillance data in linelist format}

\item{algo}{algorithm to be used}

\item{number_of_weeks}{number of weeks for which signals are generated}

\item{strata}{A character vector specifying the columns to stratify
the analysis. Default is NULL.}

\item{interactive}{Logical (only applicable to HTML report)}

\item{tables}{Logical. True if tables should be included in report.}

\item{output_file}{The name of the output file \link[rmarkdown]{render}}

\item{signals_padded}{calculated and padded signals (for use within the app, default is NULL)}

\item{signals_agg}{aggregated signals  (for use within the app, default is NULL)}
}
\value{
the compiled document is written into the output file, and the path of the output file is returned; see \link[rmarkdown]{render}
}
\description{
Renders signal detection report
}
\examples{
\dontrun{
run_report(report_format = "HTML",
           data = SignalDetectionTool::input_example,
           algo = "farrington",
           strata = c("county_id", "community_id", "sex", "age_group"),
           interactive = TRUE,
           tables = TRUE,
           number_of_weeks = 6)
}
}
