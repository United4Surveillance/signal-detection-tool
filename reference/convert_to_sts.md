# Turns aggregated data into surveillance's sts format

Turns aggregated data into surveillance's sts format

## Usage

``` r
convert_to_sts(case_counts, time_unit = "weekly")
```

## Arguments

- case_counts:

  case count data frame to be converted

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

## Examples

``` r
if (FALSE) { # \dontrun{
data <- input_example %>%
  preprocess_data() %>%
  aggregate_data()
sts_cases <- convert_to_sts(data)
sts_cases
} # }
```
