# Get signals of CUSUM algorithm with reset

Get signals of CUSUM algorithm with reset

## Usage

``` r
get_signals_cusum(
  data_aggregated,
  number_of_time_units = 52,
  time_unit = "weekly"
)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_time_units:

  integer, specifying number of time units to generate signals for.

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
results <- get_signals_cusum(data_aggregated)
results
} # }
```
