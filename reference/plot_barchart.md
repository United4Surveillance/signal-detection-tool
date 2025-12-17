# Barplot visualising the number of cases and information about any signals

Bars showing number of cases and coloring around the bar showing whether
any signal was generated in the last n weeks for this stratum

## Usage

``` r
plot_barchart(
  signals_agg,
  interactive = TRUE,
  toggle_alarms = FALSE,
  partial = FALSE
)
```

## Arguments

- signals_agg:

  tibble, aggregated signals which can be obtained from using the
  function
  [`aggregate_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md).
  It contains the number of cases, any_alarms and n_alarms for one
  category, i.e. age group summed over the number of weeks used in
  [`aggregate_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md).

- interactive:

  boolean identifying whether the plot should be static or interactive

- toggle_alarms:

  boolean identifying whether the plot should showing number of signals
  explicitly or only when hovering

- partial:

  logical, add partial bundle to plotly

## Value

either a gg or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
signals_agg_sex <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("sex")) %>%
  aggregate_signals(number_of_weeks = 12)
plot_barchart(signals_agg_sex)
} # }
```
