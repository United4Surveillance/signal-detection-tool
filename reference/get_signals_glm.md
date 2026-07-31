# Get signals based on a weigthed GLM quasipoisson regression model for the expected case counts The GLM is flexible being able to just fit a mean, add a time trend, fit a harmonic sin/cos model (one or two seasonal components) or the seasons from the farringtonflexible.

Get signals based on a weigthed GLM quasipoisson regression model for
the expected case counts The GLM is flexible being able to just fit a
mean, add a time trend, fit a harmonic sin/cos model (one or two
seasonal components) or the seasons from the farringtonflexible.

## Usage

``` r
get_signals_glm(
  data_aggregated,
  number_of_time_units = 6,
  model = "mean",
  time_trend = TRUE,
  return_full_model = TRUE,
  alpha_upper = 0.05,
  intervention_date = NULL,
  min_timepoints_baseline = 12,
  min_timepoints_trend = 12,
  past_time_units_not_included = 4,
  exclude_outbreak_cases_from_fitting = FALSE,
  time_unit = "weekly"
)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts.

- number_of_time_units:

  integer, specifying number of time units to generate signals for.

- model:

  character, default "mean" one of c("mean", "sincos", "sincos_multiS",
  "FN") specifying which kind of model the glm is fitting. "mean" fits
  an intercept model, "sincos" a harmonic sincos model, "FN" uses the
  seasgroups from farrington to fit parameters for seasonality.

- time_trend:

  boolean, default TRUE, when TRUE a timetrend is fitted in the glm
  describing the expected number of cases.

- return_full_model:

  boolean, default TRUE, specifying whether the fitted values of the
  model obtained from fitting the model to the first week of
  number_of_time_units should be returned and attached to
  data_aggregated as well.

- alpha_upper:

  numeric between 0.001 and 0.2 (default: 0.05). Specifies the p-value
  cutoff used to compute the threshold; for example, a value of 0.05
  corresponds to using the 0.95 quantile.

- intervention_date:

  A date object or character of format yyyy-mm-dd or NULL specifying the
  date for the intervention in the pandemic correction models. Default
  is NULL which indicates that no intervention is done, i.e. no
  additional intercept and possibly new time trend is fitted. When a
  date is given a new intercept and possibly time_trend (if time_trend
  == TRUE) is fitted.

- min_timepoints_baseline:

  integer, default 12, this parameter is only used when
  intervention_date is not NULL, specifying the number of weeks at least
  needed for fitting a new baseline after the intervention.

- min_timepoints_trend:

  integer, default 12, this parameter is only used when
  intervention_date is not NULL, specifying the number of weeks at least
  needed for fitting a new timetrend after the intervention.

- past_time_units_not_included:

  An integer specifying the number of past time units to exclude from
  the fitting process. This can be useful for excluding recent data with
  outbreaks or data that may not be fully reported. Default is \`4\`.

- exclude_outbreak_cases_from_fitting:

  A boolean specifying whether outbreak-associated cases should be
  excluded from case counts. If \`data_aggregated\` does not contain a
  \`cases_not_in_outbreak\` column indicating the number of cases not
  associated with outbreaks, no exclusion is applied, even when
  \`exclude_outbreak_cases_from_fitting = TRUE\`. Default is \`FALSE\`.

- time_unit:

  character, specifying the time units to aggregate case data on.
  Default is "weekly". Algorithms using the farrington framework can
  only be used with weekly aggregated data.

## Value

data.frame aggregated data with case counts and additional columns
alarms, upperbound and expected obtained from the signal detection
algorithm. If return_full_model == TRUE then expected_pad is also added
as a column to data_aggregated.

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
results <- get_signals_glm(data_aggregated)
results
} # }
```
