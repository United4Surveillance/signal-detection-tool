# Filter Data Frame by Date Range

This function filters a data frame to include only rows where a
specified date column falls within a given start and/or end date. The
function accepts `Date` objects or character strings (formatted as
`"yyyy-mm-dd"`) for `date_start` and `date_end` parameters.

## Usage

``` r
filter_by_date(
  data,
  date_var = "date_report",
  date_start = NULL,
  date_end = NULL
)
```

## Arguments

- data:

  A data frame containing the date column to filter by.

- date_var:

  A character string specifying the name of the date column in `data`.
  Default is `"date_report"`.

- date_start:

  A `Date` object or character string in `"yyyy-mm-dd"` format, or
  `NULL`. If provided, only rows where `date_var` is greater than or
  equal to `date_start` are included. Default is `NULL`.

- date_end:

  A `Date` object or character string in `"yyyy-mm-dd"` format, or
  `NULL`. If provided, only rows where `date_var` is less than or equal
  to `date_end` are included. Default is `NULL`.

## Value

A filtered data frame containing only rows that match the specified date
range.

## Examples

``` r
# Example data frame
if (FALSE) { # \dontrun{
data <- data.frame(
  date_report = as.Date("2023-01-01") + 0:9,
  value = rnorm(10)
)

# Filter data from January 3, 2023 to January 8, 2023 (using Date format)
filtered_data <- filter_by_date(data,
  date_var = "date_report",
  date_start = as.Date("2023-01-03"),
  date_end = as.Date("2023-01-08")
)

# Filter data using character format for dates
filtered_data <- filter_by_date(data,
  date_var = "date_report",
  date_start = "2023-01-03",
  date_end = "2023-01-08"
)
} # }
```
