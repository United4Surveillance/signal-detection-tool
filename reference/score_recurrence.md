# Score signals based on temporal alarm recurrence within strata

Computes a score for each signal based on the frequency of alarms within
the test period of its stratum and category. Higher alarm frequency
results in higher scores. The score is set to \`NA\` when recurrence
cannot be assessed, such as when the test period consists of only one
week. A single alarm across multiple observed time units is assigned a
score of 0, because it does not indicate recurrence. If more than one
alarm occurs, the score is calculated as \`n_alarms / n_time_units\`.

## Usage

``` r
score_recurrence(signal_results)
```

## Arguments

- signal_results:

  signal detection results obtained from get_signals()

## Value

signal detection results scored

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals_all() %>%
  dplyr::mutate(pathogen = "Pertussis")

get_scores(signals, scorers = list(recurrence = score_recurrence))
} # }
```
