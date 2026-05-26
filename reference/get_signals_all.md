# Get Signals for All Strata Including Unstratified

This function computes signals for the provided preprocessed
surveillance data, using the specified detection method and optionally
stratifies by given variables. If stratification is applied, it also
computes the unstratified signals and appends them to the result,
ensuring a unified output suitable for visualization.

## Usage

``` r
get_signals_all(
  preprocessed_data,
  method = "farrington",
  intervention_date = NULL,
  stratification = NULL,
  date_start = NULL,
  date_end = NULL,
  date_ext = NULL,
  date_var = "date_report",
  number_of_weeks = 6,
  alpha_upper = 0.05
)
```

## Arguments

- preprocessed_data:

  A data frame that has been preprocessed using \[preprocess_data()\].

- method:

  A character string specifying the signal detection method to use. See
  \[available_algorithms()\] for options.

- intervention_date:

  A date or character string in yyyy-mm-dd format indicating the start
  of a post-intervention period for time series correction (only
  relevant for certain models).

- stratification:

  A character vector specifying the variables to stratify the analysis
  on.

- date_start:

  Optional. A date or character string in yyyy-mm-dd format indicating
  the beginning of the analysis period.

- date_end:

  Optional. A date or character string in yyyy-mm-dd format indicating
  the end of the analysis period.

- date_ext:

  A date object or character of format yyyy-mm-dd. Extends the
  aggregated dataset until this date. Default is NULL

- date_var:

  A character string specifying the column name of the date variable to
  use. Default is "date_report".

- number_of_weeks:

  Integer specifying how many weeks to generate signals for.

- alpha_upper:

  numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not
  use the value; for these, the argument is ignored and internally set
  to NULL. Specifies the p-value cutoff used to compute the threshold;
  for example, a value of 0.05 corresponds to using the 0.95 quantile.

## Value

A tibble with columns for signals, expected values, thresholds, and
stratification information (if applicable), containing both stratified
and unstratified results for comprehensive comparison or plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
results_all <- get_signals_all(
  data_preprocessed,
  method = "farrington",
  stratification = c("sex", "age_group")
)
} # }
```
