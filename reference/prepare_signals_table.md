# Prepare the signal detection results for creation of table with results

This function converts the columns week, year and cases to integer,
columns are renamed and category with NA is replaced by None. It can
filter the data based on the \`signals_only\` parameter giving back only
those weeks where a signal was found.

## Usage

``` r
prepare_signals_table(data, signals_only = TRUE)
```

## Arguments

- data:

  data.frame containing signals from
  [get_signals](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)

- signals_only:

  Logical indicating whether to filter the signal results to include
  only the weeks when a signal was generated (default is TRUE). If set
  to TRUE, the signals column is removed from the table. When FALSE the
  signals column is kept to distinguish the weeks with and without
  alarms.

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  year = 2020:2022,
  week = 1:3,
  cases = 10:12,
  alarms = c(TRUE, FALSE, TRUE),
  upperbound = c(15, NA, 14),
  category = c(NA, NA, NA)
)
prepare_signals_table(data)
} # }
```
