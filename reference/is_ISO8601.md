# checking YYYYY-mm-dd format of date variables

checking YYYYY-mm-dd format of date variables

## Usage

``` r
is_ISO8601(date_col)
```

## Arguments

- date_col:

  character vector, vector containing dates to check

## Value

boolean, when TRUE all values of date_col are in the required format,
when FALSE at least one is not in required format

## Examples

``` r
if (FALSE) { # \dontrun{
is_ISO8601(c("2023-04-01", "2022-12-31"))
} # }
```
