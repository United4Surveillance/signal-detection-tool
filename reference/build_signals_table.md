# Builds the signal detection results table with different formating options. To get the raw data.frame containing method ald number_of_weeks as well use format = "data.frame", to obtain nicely formated tables in an interactive DataTable or as Flextable use format = "DataTable" or format = "Flextable".

This function applies the
[prepare_signals_table](https://united4surveillance.github.io/signal-detection-tool/reference/prepare_signals_table.md)
and if format = c("DataTable","Flextable")
[format_table](https://united4surveillance.github.io/signal-detection-tool/reference/format_table.md)
to create a nicely formated results table based on the input data frame.
If format = "data.frame"
[format_table](https://united4surveillance.github.io/signal-detection-tool/reference/format_table.md)
is not applied and the raw preprocessed signal_results are returned. It
can filter the data based on the \`signals_only\` parameter and converts
certain columns to integers for styling purposes. This table is used to
show all signal detection results for different stratifications together
in one table.

## Usage

``` r
build_signals_table(
  signal_results,
  signals_only = TRUE,
  format = "DataTable",
  dt_selection_type = "single"
)
```

## Arguments

- signal_results:

  data.frame containing signals from
  [get_signals](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)

- signals_only:

  Logical indicating whether to filter the signal results to include
  only the weeks when a signal was generated (default is TRUE). If set
  to TRUE, the signals column is removed from the table. When FALSE the
  signals column is kept to distinguish the weeks with and without
  alarms.

- format:

  Character specifying the output format. Must be one of: -
  \`"data.frame"\`: A standard R data frame. - \`"DataTable"\`: An
  interactive table using the DataTable library. - \`"Flextable"\`: A
  formatted table suitable for reporting,i.e. word documents. Default is
  "DataTable".

- dt_selection_type:

  String controlling the DataTable selection argument. Expected values
  are "multiple", "single", "none" (default is 'single').

## Value

data.frame or DataTable or Flextable depending on \`format\`

## Examples

``` r
if (FALSE) { # \dontrun{
signal_results <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("age_group"), number_of_weeks = 6)
build_signals_table(signal_results)
build_signals_table(signal_results, format = "data.frame")
} # }
```
