# Builds the aggregated signal detection results table with different formating options.

Prepares and formats the aggregated signal results table for one
category and orders the strata by the factor levels. This function
combines the preparation of the aggregated signals data.frame with the
final formating of the table by applying
[prepare_signals_agg_table](https://united4surveillance.github.io/signal-detection-tool/reference/prepare_signals_agg_table.md)
and
[format_table](https://united4surveillance.github.io/signal-detection-tool/reference/format_table.md).

## Usage

``` r
build_signals_agg_table(signals_agg, format = "DataTable")
```

## Arguments

- signals_agg:

  A tibble or data.frame containing aggregated signals produced from
  [aggregate_signals](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md)(signals,number_of_weeks
  = 6).

- format:

  Character specifying the output format. Must be one of: -
  \`"data.frame"\`: A standard R data frame. - \`"DataTable"\`: An
  interactive table using the DataTable library. - \`"Flextable"\`: A
  formatted table suitable for reporting,i.e. word documents. Default is
  "DataTable".

## Value

data.frame or DataTable or Flextable depending on \`format\`

## Examples

``` r
if (FALSE) { # \dontrun{
signals_agg <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("age_group", "sex")) %>%
  aggregate_signals(number_of_weeks = 6) %>%
  filter(category == "age_group")

build_signals_agg_table(signals_agg)
} # }
```
