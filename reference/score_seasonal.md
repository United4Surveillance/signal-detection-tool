# Score signals according to seasonal distribution of cases

Scores each signal in the signal detection results according to the
calculated seasonal distribution. This distribution is calculated by the
average percentage of cases that happen each month for each year of
complete data. The score is defined by 1 - scaled percentage, were
scaled percentage is given by an s-curve \\f(x)=x^2/(1/12^2 + x^2)\\.
Then it is rounded to the 2nd decimal point.

## Usage

``` r
score_seasonal(signals_res)
```

## Arguments

- signals_res:

  signal detection results obtained from get_signals()

## Value

signal detection results scored

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals() %>%
  dplyr::mutate(pathogen = "Pertussis")

get_scores(signals, scorers = list(seasonal = score_seasonal))
} # }
```
