# Convert specified columns to integer type

This function takes a data frame and a vector of column names and
converts the specified columns to integer type.

## Usage

``` r
convert_columns_integer(data, columns_to_convert)
```

## Arguments

- data:

  A data frame.

- columns_to_convert:

  A character vector of column names to convert to integers.

## Value

A data frame with the specified columns converted to integers.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(a = 1:5, b = 1:5, c = 1.234:5.234)
convert_columns_integer(data, c("a", "b"))
} # }
```
