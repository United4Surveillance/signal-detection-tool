# Create a model formula based on the columns in the model_data dataframe.

It uses all columns present in the model_data data.frame and models the
number of cases by an additive model of all covariates.

## Usage

``` r
create_formula(model_data)
```

## Arguments

- model_data:

  a data.frame containing all variables and their values over the
  timeseries for the model

## Value

string specifying the formula for the regression model

## Examples

``` r
if (FALSE) { # \dontrun{
model_data_1 <- create_model_data(100, model = "sincos")
model_data_2 <- create_model_data(100, model = "sincos", intervention_start = 50)
create_formula(model_data_1)
create_formula(model_data_2)
} # }
```
