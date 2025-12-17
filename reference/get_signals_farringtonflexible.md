# Get signals of surveillance's farringtonFlexible algorithm

Get signals of surveillance's farringtonFlexible algorithm

## Usage

``` r
get_signals_farringtonflexible(data_aggregated, number_of_weeks = 52)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_weeks:

  integer, specifying number of weeks to generate signals for

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data() %>%
  add_rows_missing_dates()
results <- get_signals_farringtonflexible(data_aggregated)
} # }
```
