# Get the numeric columns that are not integer columns

This function takes a data frame as input and returns the names of
columns that contain numeric values and are not integer columns.

## Usage

``` r
get_float_columns(data)
```

## Arguments

- data:

  A data frame.

## Value

A character vector containing the names of numeric columns that are not
integer columns.

## Examples

``` r
data <- data.frame(a = 1:5, b = 1:5, c = 1L:5L)
SignalDetectionTool::get_float_columns(data)
#> character(0)
```
