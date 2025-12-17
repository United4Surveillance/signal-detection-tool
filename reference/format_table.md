# Format the signal results to an interactive or static table

This function creates an interactive DataTable or a static gt table
depending on the value of the \`interactive\` parameter. It takes the
stratum and category columns and transforms those to be headers of
sections containing all signal results for these strata. It colors rows
red for tables containing the any_alarms column when there was \>= 1
alarm. It makes the table downloadable as CSV, Excel, PDF.

## Usage

``` r
format_table(
  data,
  signals_only = TRUE,
  interactive = TRUE,
  dt_selection_type = "single"
)
```

## Arguments

- data:

  A data frame.

- signals_only:

  Logical indicating whether to filter the signal results to include
  only the weeks when a signal was generated (default is TRUE). If set
  to TRUE, the signals column is removed from the table. When FALSE the
  signals column is kept to distinguish the weeks with and without
  alarms.

- interactive:

  Logical indicating whether to create an interactive DataTable (default
  is TRUE).

- dt_selection_type:

  String controlling the DataTable selection argument. Expected values
  are "multiple", "single", "none" (default is 'single').

## Value

An interactive DataTable or a static gt table, depending on the value of
\`interactive\`.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  year = 2020:2023,
  week = 1:4,
  cases = 10:13,
  alarms = c(TRUE, TRUE, TRUE, FALSE),
  threshold = c(15, NA, 14, 14),
  category = c("age_group", "age_group", "sex", "sex"),
  stratum = c("00-05", "30-35", "female", "male")
)
format_table(data)

data_agg <- data.frame(
  stratum = c("00-04", "05-09", "10-14", "100-104", "105-109", "15-19"),
  cases = c(74, 5, 0, 0, 0, 2),
  any_alarms = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  n_alarms = c(1, 0, 0, 0, 0, 0),
  category = rep("age_group", 6)
)

format_table(data_agg)
} # }
```
