# Score signals according to specificity of signal strength within category

Scores each signal in the signal_results object according to the
fraction of other alarms that are stronger in the category in the same
time unit

## Usage

``` r
score_specificity_stronger(signal_results)
```

## Arguments

- signal_results:

  signal detection results obtained from get_signals_all()

## Value

signal detection results scored
