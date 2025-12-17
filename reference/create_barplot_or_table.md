# Decider function to create barplot or table of aggregated cases with signals Depending on the number of unique levels to visualise it is decided whether a barplot or a table is shown. The aggregated number of cases for each stratum and whether any signal are shown.

Decider function to create barplot or table of aggregated cases with
signals Depending on the number of unique levels to visualise it is
decided whether a barplot or a table is shown. The aggregated number of
cases for each stratum and whether any signal are shown.

## Usage

``` r
create_barplot_or_table(
  signals_agg,
  category_selected,
  n_levels = 25,
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
  i.e. age_group and county.

- category_selected:

  the category from the signals_agg we want to visualise

- n_levels:

  the threshold for the number of levels from which we decide when a
  table is generated instead of a barchart visualisation

- interactive:

  boolean identifying whether the plot should be static or interactive

- toggle_alarms:

  boolean identifying whether the plot should showing number of signals
  explicitly or only when hovering

- partial:

  logical, add partial bundle to plotly

## Value

a table or a plot depending on whether number of unique levels for the
category to visualise, the table and plots can be interactive or not
depening on the interactive parameter, can be class "ggplot" or "plotly"
for plot and class "gt_tbl" or "datatables" for table

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("sex", "age_group"))
signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
create_barplot_or_table(signals_agg, "age_group")
} # }
```
