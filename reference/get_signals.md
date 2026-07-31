# Get Signals

This function analyzes surveillance data to detect signals using the
specified method.

## Usage

``` r
get_signals(
  data,
  method = "farrington",
  intervention_date = NULL,
  stratification = NULL,
  date_start = NULL,
  date_end = NULL,
  date_ext = NULL,
  date_var = "date_report",
  time_unit = "weekly",
  number_of_time_units = 6,
  alpha_upper = 0.05,
  exclude_outbreak_cases_from_fitting = FALSE
)
```

## Arguments

- data:

  A data frame containing the surveillance data preprocessed with
  \[preprocess_data()\].

- method:

  A character string specifying the signal detection method to use.
  Available options include: \`"farrington"\`, \`"ears"\`, \`"cusum"\`,
  \`"glm mean"\`, \`"glm timetrend"\`, \`"glm harmonic"\`, \`"glm
  harmonic with timetrend"\`, \`"glm harmonic multi"\`, \`"glm
  farrington"\`, \`"glm farrington with timetrend"\`. You can retrieve
  the full list using \[available_algorithms()\].

- intervention_date:

  A date object or character of format yyyy-mm-dd specifying the date
  for the intervention in the pandemic correction models. After this
  date a new intercept and possibly time_trend is fitted.

- stratification:

  A character vector specifying the columns to stratify the analysis.
  Default is NULL.

- date_start:

  A date object or character of format yyyy-mm-dd specifying the start
  date to filter the data by. Default is NULL.

- date_end:

  A date object or character of format yyyy-mm-dd specifying the end
  date to filter the data by. Default is NULL.

- date_ext:

  A date object or character of format yyyy-mm-dd. Extends the
  aggregated dataset until this date. Default is NULL

- date_var:

  a character specifying the date variable name used for the
  aggregation. Default is "date_report".

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly". Algorithms using the farrington framework can
  only be used with weekly aggregated data.

- number_of_time_units:

  integer, specifying number of time units to generate signals for.

- alpha_upper:

  numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not
  use the value; for these, the argument is ignored and internally set
  to NULL. Specifies the p-value cutoff used to compute the threshold;
  for example, a value of 0.05 corresponds to using the 0.95 quantile.

- exclude_outbreak_cases_from_fitting:

  A boolean specifying whether outbreak-associated case counts should be
  excluded only when fitting the baseline. \`TRUE\` can only be applied
  when GLM-based outbreak detection models are used. For other models
  only \`FALSE\` is a valid input. If \`data\` does not contain an
  \`outbreak_status\` column indicating the number of cases associated
  with outbreaks, no exclusion is applied, even when
  \`exclude_outbreak_cases_from_fitting = TRUE\`. Default is \`FALSE\`.

## Value

A tibble containing the results of the signal detection analysis.

## See also

\[available_algorithms()\]

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>%
  preprocess_data()
results <- get_signals(
  data_preprocessed,
  method = "farrington",
  stratification = c("county", "sex")
)
results
} # }
```
