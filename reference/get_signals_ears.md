# Get signals of surveillance's EARS algorithm

Get signals of surveillance's EARS algorithm

## Usage

``` r
get_signals_ears(
  data_aggregated,
  number_of_time_units = 52,
  method = "C1",
  time_unit = "weekly"
)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_time_units:

  integer, specifying number of time units to generate signals for. The
  default is weeks

- method:

  string indicating which method to use: one of "C1", "C2", "C3"

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
results <- get_signals_ears(data_aggregated)
results
} # }
```
