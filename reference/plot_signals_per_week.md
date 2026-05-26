# Plot in how many strata an signal was detected under the detection period

Using the results of signal detection, plot a week-to-week
representation of strata that had alarms

## Usage

``` r
plot_signals_per_week(results, n_strata, interactive = FALSE, branding = NULL)
```

## Arguments

- results:

  dataframe of a single-pathogen signal detection results for a strata
  category

- n_strata:

  integer. Number of stratification levels in category. Usually
  determined automatically by signals_agg

- interactive:

  logical, if TRUE, interactive plot is returned; default, static plot.

- branding:

  named vector with branding colours

## Value

either a ggplot or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
signals <- data_preprocessed %>% get_signals(stratification = "county")
n.strata <- 9
signals_week_barchart <- plot_signals_per_week(
  signals,
  n_strata = n.strata
)
signals_week_barchart
} # }
```
