# Get row number of aggregated data which is the isoweek and isoyear corresponding to the date given

Get row number of aggregated data which is the isoweek and isoyear
corresponding to the date given

## Usage

``` r
get_intervention_timepoint(date, data_aggregated)
```

## Arguments

- date:

  character in the format "yyyy-mm-dd"

- data_aggregated:

  data.frame, aggregated data with case counts

## Value

integer row number of data_aggregated where the isoweek and isoyear of
the given date are

## Examples

``` r
if (FALSE) { # \dontrun{
data_agg <- input_example %>%
  preprocess_data() %>%
  aggregate_data() %>%
  add_rows_missing_dates()
get_intervention_timepoint("2020-03-04", data_agg)
} # }
```
