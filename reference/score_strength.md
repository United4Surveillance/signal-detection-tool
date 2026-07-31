# Score signals according to size strength

Scores each signal in the signals_res object using the signal size,
defined by the distance from expected value and upperbound threshold
value the score follows the formula (size^3-1)/size^3 and then is
rounded to the 2nd decimal point. For EARS and CUSUM method, the
function returns a score of NA.

## Usage

``` r
score_strength(signals_res)
```

## Arguments

- signals_res:

  dataframe with the results of signal detection

## Value

strength scores for signals

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals_all() %>%
  dplyr::mutate(pathogen = "Pertussis")

get_scores(signals, scorers = list(strength = score_strength))
} # }
```
