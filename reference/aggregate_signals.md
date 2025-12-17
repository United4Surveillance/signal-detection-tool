# Aggregate cases and signals over the number of weeks. First the signals are filtered to obtain the signals for the last n weeks aggregating the number of cases observed, create variable any signal generated and the aggregate the number of signals

Aggregate cases and signals over the number of weeks. First the signals
are filtered to obtain the signals for the last n weeks aggregating the
number of cases observed, create variable any signal generated and the
aggregate the number of signals

## Usage

``` r
aggregate_signals(signals, number_of_weeks)
```

## Arguments

- signals:

  tibble, output of the
  [`get_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)
  function with number of cases and signal per week, year

- number_of_weeks:

  integer, specifying the number of weeks we want to aggregate the
  number of cases and the generated signals

## Value

tibble, with one line per groups containing the number of cases,
any_alarms and n_alarms

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("sex", "county_id"))
signals %>% aggregate_signals(number_of_weeks = 6)
} # }
```
