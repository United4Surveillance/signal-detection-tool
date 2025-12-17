# Get a default and minimum and maximum date for the intervention time point for the glm algorithms with pandemic correction. This is based on the data provided and the settings for the delays.

Get a default and minimum and maximum date for the intervention time
point for the glm algorithms with pandemic correction. This is based on
the data provided and the settings for the delays.

## Usage

``` r
get_valid_dates_intervention_start(
  data,
  date_var = "date_report",
  number_of_weeks = 6,
  time_trend = TRUE,
  min_timepoints_baseline = 12,
  min_timepoints_trend = 12,
  past_weeks_not_included = 4
)
```

## Arguments

- data:

  data.frame, preprocessed linelist of surveillance data obtained using
  preprocess_data()

- date_var:

  a character specifying the date variable name used for the
  aggregation. Default is "date_report".

- number_of_weeks:

  integer, specifying number of weeks to generate signals for

- time_trend:

  boolean, default TRUE, when TRUE a timetrend is fitted in the glm
  describing the expected number of cases

- min_timepoints_baseline:

  integer, default 12, specifying the number of weeks at least needed
  for fitting a new baseline after the intervention.

- min_timepoints_trend:

  integer, default 12, specifying the number of weeks at least needed
  for fitting a new timetrend after the intervention.

- past_weeks_not_included:

  An integer specifying the number of past weeks to exclude from the
  fitting process. This can be useful for excluding recent data with
  outbreaks or data that may not be fully reported. Default is \`4\`.

## Value

list with three dates or NULL values. valid_start_date is the first date
which is valid to chose as intervention_date, valid_end_date is the last
date which is valid to chose to chose as intervention_date,
default_intervention is a default date which is used for the
intervention_date and usually set to "2020-03-15" but checked whether
this is possible with the data we have

## Examples

``` r
if (FALSE) { # \dontrun{
input_prepro <- input_example %>% preprocess_data()
get_valid_dates_intervention_start(input_prepro) # this just gives the default date "2020-03-15" back
get_valid_dates_intervention_start(input_prepro %>% dplyr::filter(date_report >= "2020-04-01")) # this gives the valid_start date back as default date
get_valid_dates_intervention_start(input_prepro %>% dplyr::filter(date_report >= "2020-04-01") %>% dplyr::filter(date_report <= "2020-05-01")) # this gives NULL as the timeperiod of date provided is too short to do a intervention
} # }
```
