# Filter the data so that only the data of the last n time units are returned This function can be used to filter for those last n time units where signals were generated.

Filter the data so that only the data of the last n time units are
returned This function can be used to filter for those last n time units
where signals were generated.

## Usage

``` r
filter_data_last_n_time_units(
  data_agg,
  time_unit = "weekly",
  number_of_time_units
)
```

## Arguments

- data_agg:

  data.frame, aggregated surveillance or signals dataset, where
  aggregated means no linelist but cases or signals per week/month, year

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

- number_of_time_units:

  integer, specifying the number of time units from the most recent time
  unit we want to filter the data for

## Value

data.frame, aggregated data of last n time units
