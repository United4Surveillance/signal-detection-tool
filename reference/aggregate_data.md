# Aggregates case data (linelist, i.e. one row per case) by isoyear and isoweek or the month and adds missing isoweeks or months to the aggregated dataset. Additionally number of cases part of a known outbreak is added if the variable outbreak_status exists in the data.

Aggregates case data (linelist, i.e. one row per case) by isoyear and
isoweek or the month and adds missing isoweeks or months to the
aggregated dataset. Additionally number of cases part of a known
outbreak is added if the variable outbreak_status exists in the data.

## Usage

``` r
aggregate_data(
  data,
  date_var = "date_report",
  date_start = NULL,
  date_end = NULL,
  date_ext = NULL,
  group = NULL,
  time_unit = "weekly",
  exclude_outbreak_cases_from_fitting = FALSE
)
```

## Arguments

- data:

  data.frame, linelist of cases to be aggregated

- date_var:

  a character specifying the date variable name used for the
  aggregation. Default is "date_report".

- date_start:

  A date object or character of format yyyy-mm-dd. Default is NULL which
  means that missing isoweeks are added until the minimum date of the
  dataset. This parameter can be used when the dataset should be
  extended further than the minimum date of the dataset.

- date_end:

  A date object or character of format yyyy-mm-dd. Default is NULL which
  means that missing isoweeks are added until the maximum date of the
  dataset. This can be used when the dataset should be extended further
  than the minimum date of the dataset.

- date_ext:

  A date object or character of format yyyy-mm-dd. Extends the
  aggregated dataset until this date. Default is NULL

- group:

  A character specifying another grouping variable. Usually used for
  stratification.

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

- exclude_outbreak_cases_from_fitting:

  A boolean specifying whether outbreak-associated case counts should be
  excluded only when fitting the baseline. If \`data\` does not contain
  an \`outbreak_status\` column indicating the number of cases
  associated with outbreaks, the number of cases not in outbreaks is not
  calculated, even when \`exclude_outbreak_cases_from_fitting = TRUE\`.
  Default is \`FALSE\`. The default should only be changed if it is
  planned to use a GLM-based algorithm.

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
data_aggregated
} # }
```
