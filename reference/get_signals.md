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
  date_var = "date_report",
  number_of_weeks = 6
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
  harmonic with timetrend"\`, \`"glm farrington"\`, \`"glm farrington
  with timetrend"\`. You can retrieve the full list using
  \[available_algorithms()\].

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

- date_var:

  a character specifying the date variable name used for the
  aggregation. Default is "date_report".

- number_of_weeks:

  integer, specifying number of weeks to generate signals for.

## Value

A tibble containing the results of the signal detection analysis.

## See also

\[available_algorithms()\]

## Examples

``` r
if (FALSE) { # \dontrun{
results <- input_example %>%
  preprocess_data() %>%
  get_signals(
    method = "farrington",
    stratification = c("county", "sex")
  )
} # }
```
