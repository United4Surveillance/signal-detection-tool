% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_algorithm.R
\name{create_formula}
\alias{create_formula}
\title{Create a model formula based on the columns in the model_data dataframe.}
\usage{
create_formula(model_data)
}
\arguments{
\item{model_data}{a data.frame containing all variables and their values over the timeseries for the model}
}
\value{
string specifying the formula for the regression model
}
\description{
It uses all columns present in the model_data data.frame and models the number of cases by an additive model of all covariates.
}
\examples{
\dontrun{
model_data_1 <- create_model_data(100, model = "sincos")
model_data_2 <- create_model_data(100, model = "sincos", intervention_start = 50)
create_formula(model_data_1)
create_formula(model_data_2)
}
}
