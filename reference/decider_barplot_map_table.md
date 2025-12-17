# Decider function whether create_map_or_table or create_barplot_or_table is used

Depending on the category which should be visualised (regional variable)
or non regional category such as age_group, sex, ... a map is tried for
plotting or a barchart.

## Usage

``` r
decider_barplot_map_table(
  signals_agg,
  data_surveillance,
  signal_category,
  interactive = TRUE,
  toggle_alarms = FALSE,
  partial = FALSE
)
```

## Arguments

- signals_agg:

  tibble, aggregated signals over n weeks with columns number of cases,
  any_alarms and n_alarms
  [`aggregate_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md).
  This tibble can contain the aggregated signals for multiple categories
  i.e. state and county.

- data_surveillance:

  data.frame, surveillance linelist

- signal_category:

  character, naming the category which should be visualised, i.e.
  "state","age_group","sex"

- interactive:

  boolean identifying whether the plot should be static or interactive

- toggle_alarms:

  boolean identifying whether the plot should showing number of signals
  explicitly or only when hovering

- partial:

  logical, add partial bundle to plotly

## Value

a table or a plot depending on signal_category, the table and plots can
be interactive or not depening on the interactive parameter, can be
class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables"
for table
