# Get signals of surveillance's EARS algorithm

Get signals of surveillance's EARS algorithm

## Usage

``` r
get_signals_ears(data_aggregated, number_of_weeks = 52, method = "C1")
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_weeks:

  integer, specifying number of weeks to generate signals for

- method:

  string indicating which method to use: one of "C1", "C2", "C3"

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data() %>%
  add_rows_missing_dates()
results <- get_signals_ears(data_aggregated)
} # }
```
