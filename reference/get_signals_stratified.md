# Get Signals Stratified

This function stratifies and aggregates surveillance data by specified
columns and analyzes each stratum separately using the specified method.

## Usage

``` r
get_signals_stratified(
  data,
  fun,
  model = "",
  intervention_date = NULL,
  time_trend = FALSE,
  stratification_columns,
  date_start = NULL,
  date_end = NULL,
  date_ext = NULL,
  date_var = "date_report",
  number_of_weeks = 6,
  alpha_upper = 0.05
)
```

## Arguments

- data:

  A data frame containing the surveillance data.

- fun:

  The signal detection function to apply to each stratum.

- model:

  character, default empty string which is the choice if farrington,
  ears or cusum are used and if a glm method was chosen as outbreak
  detection method then one of c("mean","sincos", "FN")

- intervention_date:

  A date object or character of format yyyy-mm-dd specifying the date
  for the intervention in the pandemic correction models. After this
  date a new intercept and possibly time_trend is fitted.

- time_trend:

  boolean default TRUE setting time_trend in the get_signals_glm(). This
  parameter is only used when an the glm based outbreak detection models
  are used, i.e. for the models c("mean","sincos", "FN")

- stratification_columns:

  A character vector specifying the columns to stratify the data by.

- date_start:

  A date object or character of format yyyy-mm-dd specifying the start
  date to filter the data by. Default is NULL.

- date_end:

  A date object or character of format yyyy-mm-dd specifying the end
  date to filter the data by. Default is NULL.

- date_ext:

  A date object or character of format yyyy-mm-dd. Extends the
  aggregated dataset until this date. Default is NULL.

- date_var:

  a character specifying the date variable name used for the
  aggregation. Default is "date_report".

- number_of_weeks:

  integer, specifying number of weeks to generate signals for.

- alpha_upper:

  numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not
  use the value; for these, the argument is ignored and internally set
  to NULL. Specifies the p-value cutoff used to compute the threshold;
  for example, a value of 0.05 corresponds to using the 0.95 quantile.

## Value

A tibble containing the results of the signal detection analysis
stratified by the specified columns.

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
categories <- c("county", "sex", "age_group") # Replace with actual column names
results <- get_signals_stratified(
  data_preprocessed,
  fun = get_signals_farringtonflexible,
  stratification_columns = categories
)
} # }
```
