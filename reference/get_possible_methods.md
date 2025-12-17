# Determine Possible Outbreak Detection Methods Based on Available Historic Data

This function identifies which algorithms can be applied for outbreak
detection depending on the amount of historic data available for model
fitting. The decision is based on the minimum and maximum dates of the
time series and the number of weeks reserved at the end of the series
(e.g. for current detection).

## Usage

``` r
get_possible_methods(min_date, max_date, number_of_weeks = 6)
```

## Arguments

- min_date:

  A `Date` object, the minimum date in the time series used for fitting
  a model.

- max_date:

  A `Date` object, the maximum date in the time series used for fitting
  a model.

- number_of_weeks:

  Integer. Number of weeks at the end of the time series that are
  reserved for detection / monitoring and therefore not counted as
  historic data for model fitting. Default is `6`.

## Value

A character vector of possible algorithm names. Returns `NULL` if no
method is applicable.

## Details

Historic data is defined as the period from `min_date` to
`max_date - number_of_weeks`. The number of available weeks is computed
on this interval, counting partial weeks as full weeks to match the
weekly aggregation used by the algorithms.

The method selection criteria are approximately:

- If 4 years (about 208 weeks) or more of historic data are available:
  all methods are possible.

- If 3 to \<4 years (about 156 to \<208 weeks) of historic data are
  available: all methods except `"glm farrington"` and
  `"glm farrington with timetrend"` are possible.

- If 2 to \<3 years (about 104 to \<156 weeks) of historic data are
  available: all methods except `"glm farrington"`,
  `"glm farrington with timetrend"` and `"glm harmonic with timetrend"`
  are possible.

- If 26 to \<104 weeks of historic data are available: `"Mean"`,
  `"CUSUM"` and `"EARS"` are possible.

- If 7 to \<26 weeks of historic data are available: `"Mean"` and
  `"CUSUM"` are possible.

- If 1 to \<7 weeks of historic data are available: `"CUSUM"` is
  possible.

- If no training data is available (less than 1 week), `NULL` is
  returned.

The function computes `max_date_fit = max_date - number_of_weeks` and
then calculates the number of weeks between `min_date` and
`max_date_fit`. Partial weeks are counted as full weeks to align with
the weekly aggregation of the data. Based on this number of historic
weeks, suitable algorithms are selected using
[`available_algorithms`](https://united4surveillance.github.io/signal-detection-tool/reference/available_algorithms.md).

## See also

[`available_algorithms`](https://united4surveillance.github.io/signal-detection-tool/reference/available_algorithms.md),
[`get_min_max_date`](https://united4surveillance.github.io/signal-detection-tool/reference/get_min_max_date.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  date_report = seq.Date(Sys.Date() - 500, Sys.Date(), by = "day")
)

mm <- get_min_max_date(data, date_var = "date_report")

get_possible_methods(
  min_date = mm$min_date,
  max_date = mm$max_date,
  number_of_weeks = 6
)
} # }
```
