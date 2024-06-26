% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_functions.R
\name{age_groups}
\alias{age_groups}
\title{Creates age grouping variable for a given data set}
\usage{
age_groups(df, break_at = NULL)
}
\arguments{
\item{df}{data frame on which the age grouping is created}

\item{break_at}{integer that controls the length of the age groups}
}
\description{
Creates age grouping variable for a given data set
}
\examples{
\dontrun{
input_path <- "data/input/input_sample.csv"
data <- read.csv(input_path, header = TRUE, sep = ",")
data$age <- sample(1:125, 10, replace = TRUE)
age_groups(data) # default age groups
age_groups(data, c(15L, 35L, 65L, 100L)) # custom age groups
}
}
