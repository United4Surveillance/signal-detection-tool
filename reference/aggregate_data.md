# Aggregates case data (linelist, i.e. one row per case) by isoyear and isoweek and adds missing isoweeks to the aggregated dataset. Additionally number of cases part of a known outbreak is added if the variable outbreak_status exists in the data.

Aggregates case data (linelist, i.e. one row per case) by isoyear and
isoweek and adds missing isoweeks to the aggregated dataset.
Additionally number of cases part of a known outbreak is added if the
variable outbreak_status exists in the data.

## Usage

``` r
aggregate_data(
  data,
  date_var = "date_report",
  date_start = NULL,
  date_end = NULL,
  group = NULL
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

- group:

  A character specifying another grouping variable. Usually used for
  stratification.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- preprocess_data(input_example) %>% aggregate_data()
} # }
```
