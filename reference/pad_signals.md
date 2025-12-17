# Extend the computed threshold and expectation of the signal detection method to the past for visualisation purposes but not for signal generation Inside the function it is computed what the maximum number of timepoints is the signal detection algorithms can be applied for. This depends on the algorithm and the amount of historic data. The already generated signals dataframe is then extended with the expectation and threshold into the past

Extend the computed threshold and expectation of the signal detection
method to the past for visualisation purposes but not for signal
generation Inside the function it is computed what the maximum number of
timepoints is the signal detection algorithms can be applied for. This
depends on the algorithm and the amount of historic data. The already
generated signals dataframe is then extended with the expectation and
threshold into the past

## Usage

``` r
pad_signals(data, signals)
```

## Arguments

- data:

  A data frame containing the surveillance data preprocessed with
  \[preprocess_data()\].

- signals:

  tibble, output of the
  [`get_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)
  function with number of cases and signal per week, year

## Value

tibble, with padded signals

## Examples

``` r
if (FALSE) { # \dontrun{
input_example_prepro <- input_example %>%
  preprocess_data()
signals <- input_example_prepro %>%
  get_signals(stratification = c("sex", "county_id"))
pad_signals(input_example_prepro, signals)
} # }
```
