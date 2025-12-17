# Create a factor out of the stratum column with transforming NA to unknown

Create a factor out of the stratum column with transforming NA to
unknown

## Usage

``` r
create_factor_with_unknown(signals_agg)
```

## Arguments

- signals_agg:

  tibble or data.frame, aggregated signals over n weeks with columns
  number of cases, any_alarms and n_alarms
  [`aggregate_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md)
  for only one category.

## Value

tibble with stratum column being a factor

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  year = 2020:2023,
  week = 1:4,
  cases = 10:13,
  any_alarms = c(TRUE, FALSE, TRUE, FALSE),
  n_alarms = c(0, 2, 0, 1),
  category = c("sex", "sex", "sex", "sex"),
  stratum = c("female", "male", "diverse", NA)
)
create_factor_with_unknown(data)
} # }
```
