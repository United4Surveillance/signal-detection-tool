# Aggregate and Pad Signals for Reporting

This function combines the final weekly signal results into aggregate
counts (e.g. total cases, any alarms, number of alarms), and
conditionally pads the time series with historical expected values and
thresholds prior to the signal generation window. This is primarily used
for report generation and visualization.

## Usage

``` r
aggregate_pad_signals(signal_results, preprocessed, number_of_weeks, method)
```

## Arguments

- signal_results:

  A tibble returned by \[get_signals()\], containing weekly signal
  detection results (cases, alarms, upperbound, expected, etc.).

- preprocessed:

  A data frame containing the surveillance data preprocessed with
  \[preprocess_data()\].

- number_of_weeks:

  Integer specifying how many weeks to include in the aggregation.

- method:

  A character string specifying the method used to generate the signals.
  Determines whether padding is necessary. For \`"glm"\` methods,
  padding is skipped as it is assumed to be already included.

## Value

A named list with two elements:

- signals_agg:

  A tibble with aggregated results per stratum, including total cases,
  whether any alarms occurred, and the number of alarms in the last
  \`number_of_weeks\`.

- signals_padded:

  A tibble with the original \`signal_results\` augmented with
  additional rows containing historical \`expected\` and \`upperbound\`
  values (if padding was applied).

## Details

Padding is applied only for non-GLM methods. It reconstructs the
expected trajectory before the signal detection window, which is useful
for plotting full time series trends.

## See also

\[get_signals()\], \[aggregate_signals()\], \[pad_signals()\]

## Examples

``` r
if (FALSE) { # \dontrun{
results <- get_signals(preprocessed_data, method = "farrington")
output <- aggregate_pad_signals(results, number_of_weeks = 6, method = "farrington")
output$signals_agg
output$signals_padded
} # }
```
