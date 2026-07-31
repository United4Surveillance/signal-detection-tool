# Score signals according to specificity of signal within category

Scores each signal in the signal_results object according to the
fraction of other alarms in the category in the same time unit

## Usage

``` r
score_specificity_alarm(signal_results)
```

## Arguments

- signal_results:

  signal detection results obtained from get_signals_all()

## Value

signal detection results scored

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals_all() %>%
  dplyr::mutate(pathogen = "Pertussis")

get_scores(signals, scorers = list(specificity = score_specificity_alarm))
} # }
```
