# Plot time-series based on the results of a signal detection algorithm, being alarms, threshold and expectation

Static plots (default) are only based on the dates of the latest
\`number_of_weeks\` weeks. Interactive plots are based on all data, but
zoom in by default on the latest \`number_of_weeks\` weeks.

## Usage

``` r
plot_time_series(
  results,
  interactive = FALSE,
  intervention_date = NULL,
  number_of_weeks = 52,
  partial = FALSE
)
```

## Arguments

- results:

  data returned by the get_signals_farringtonflexible()

- interactive:

  logical, if TRUE, interactive plot is returned; default, static plot.

- intervention_date:

  A date object or character of format yyyy-mm-dd or NULL specifying the
  date for the intervention in the pandemic correction models. Default
  is NULL which indicates that no intervention is done.The intervention
  is marked with a dashed line.

- number_of_weeks:

  number of weeks to be covered in the plot

- partial:

  logical, add partial bundle to plotly

## Value

either a gg or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read.csv("data/input/input.csv", header = TRUE, sep = ",")
results <- get_signals_farringtonflexible(data)
plot_time_series(results)
} # }
```
