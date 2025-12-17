# Filter the data so that only the data of the last n weeks are returned This function can be used to filter for those last n weeks where signals were generated.

Filter the data so that only the data of the last n weeks are returned
This function can be used to filter for those last n weeks where signals
were generated.

## Usage

``` r
filter_data_last_n_weeks(data_agg, number_of_weeks)
```

## Arguments

- data_agg:

  data.frame, aggregated surveillance or signals dataset, where
  aggregated means no linelist but cases or signals per week, year

- number_of_weeks:

  integer, specifying the number of weeks from the most recent week we
  want to filter the data for

## Value

data.frame, aggregated data of last n weeks
