# detailed check of date variables check that months are numbers between 01-12 and days are from 01-31

detailed check of date variables check that months are numbers between
01-12 and days are from 01-31

## Usage

``` r
is_ISO8601_detailed(date_col)
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
is_ISO8601_detailed(c("2023-04-01", "2022-12-32")) # Should return FALSE
is_ISO8601_detailed(c("2023-04-01", "2022-12-31")) # Should return TRUE
} # }
```
