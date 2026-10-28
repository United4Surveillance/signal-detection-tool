# Run method for signal detection using aggregated data

Wrapper for running different signal detection algorithms using already
aggregated data. Used in combination with
\[get_method_func_parameters()\].

## Usage

``` r
run_method_parameters(
  method_list,
  data_aggregated,
  time_unit,
  n_time_units,
  ...
)
```

## Arguments

- method_list:

  List with method specification generated with
  \[get_method_func_parameters()\]

- data_aggregated:

  data.frame with timeseries of cases of one single stratification.

- time_unit:

  Character indicating the time unit aggregation (weekly, biweekly, or
  monthly)

- n_time_units:

  Integer. Time units of the Signal detection test period.

- ...:

  additional arguments for each specific method

## Value

data.frame with signal detection results

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- input_example %>% preprocess_data()
dat_agg <- aggregate_data(dat)

run_method_parameters(get_method_func_parameters("farrington"),
  data_aggregated = dat_agg,
  time_unit = "weekly",
  n_time_units = 6,
  alpha_upper = 0.05
)
} # }
```
