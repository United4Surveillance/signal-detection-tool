# Create a data.frame with 10 seasgroups components for harmonic modeling.

This function generates seasonal group data required for fitting a
Farrington-like Generalized Linear Model (GLM). The function leverages
the \`surveillance\` package to create a dataset that includes seasonal
groupings based on historical data, which is particularly useful for
modeling seasonality in time series data.

## Usage

``` r
create_fn_data(ts_length, freq = 52.25)
```

## Arguments

- ts_length:

  integer, specifying the length of the time series for which the
  seasonal group data is to be generated.

- freq:

  integer, default 52, specifying the frequency of the time series
  (e.g., 52 for weekly data).

## Value

A data frame containing the seasonal groupings (\`seasgroups\`) for the
specified time series length. These seasonal groupings are used as
covariates in the Farrington GLM model to account for seasonality.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate seasonal group data for a time series of length 100 with weekly frequency
create_fn_data(100)
} # }
```
