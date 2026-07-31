# Get signals of surveillance's farringtonFlexible algorithm

Get signals of surveillance's farringtonFlexible algorithm

## Usage

``` r
get_signals_farringtonflexible(
  data_aggregated,
  number_of_time_units = 52,
  alpha_upper = 0.05
)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_time_units:

  integer, specifying number of time units to generate signals for. The
  default is weeks and should not be changed.

- alpha_upper:

  numeric between 0.001 and 0.2 (default: 0.05). Specifies the p-value
  cutoff used to define the threshold; for example, a value of 0.05
  corresponds to using the 0.95 quantile.

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
results <- get_signals_farringtonflexible(data_aggregated)
results
} # }
```
