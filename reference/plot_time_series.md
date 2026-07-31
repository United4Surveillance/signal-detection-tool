# Plot time-series based on the results of a signal detection algorithm, being alarms, threshold and expectation

Static plots (default) are only based on the dates of the latest
\`number_of_time_units\` time units. Interactive plots are based on all
data, but zoom in by default on the latest \`number_of_time_units\` time
units.

## Usage

``` r
plot_time_series(
  results,
  interactive = FALSE,
  intervention_date = NULL,
  number_of_time_units = 52
)
```

## Arguments

- results:

  data returned by the get_signals_farringtonflexible() or signal
  detection results

- interactive:

  logical, if TRUE, interactive plot is returned; default, static plot.

- intervention_date:

  A date object or character of format yyyy-mm-dd or NULL specifying the
  date for the intervention in the pandemic correction models. Default
  is NULL which indicates that no intervention is done.The intervention
  is marked with a dashed line.

- number_of_time_units:

  number of time units to be covered in the plot

## Value

either a gg or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
signals <- data_preprocessed %>% get_signals_farringtonflexible()
signals_time_series <- signals %>% plot_time_series()
signals_time_series
} # }
```
