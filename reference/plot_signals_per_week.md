# Plot in how many strata an signal was detected under the detection period

Using the results of signal detection, plot a week-to-week
representation of strata had higher than expected case numbers

## Usage

``` r
plot_signals_per_week(
  results,
  interactive = FALSE,
  branding = NULL,
  partial = FALSE
)
```

## Arguments

- results:

  dataframe of a single-pathogen signal detection results for a strata
  category

- interactive:

  logical, if TRUE, interactive plot is returned; default, static plot.

- branding:

  named vector with branding colours

- partial:

  logical, add partial bundle to plotly

## Value

either a ggplot or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- preprocess_data(input_example)
signals <- get_signals(dat, stratification = "county")

plot_signals_per_week(signals)
} # }
```
