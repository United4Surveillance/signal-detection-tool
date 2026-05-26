# Turns aggregated data into surveillance's sts format

Turns aggregated data into surveillance's sts format

## Usage

``` r
convert_to_sts(case_counts)
```

## Arguments

- case_counts:

  case count data frame to be converted

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
