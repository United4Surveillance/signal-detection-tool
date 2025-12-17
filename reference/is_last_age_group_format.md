# checking the format of the last/biggest age group to follow the format digit separator digit, digit+ or \>digit

checking the format of the last/biggest age group to follow the format
digit separator digit, digit+ or \>digit

## Usage

``` r
is_last_age_group_format(col)
```

## Arguments

- col:

  character vector, vector containing age groups to check

## Value

boolean, when TRUE the largest age group of col is in the required
format, when FALSE it is not in required format

## Examples

``` r
if (FALSE) { # \dontrun{
is_last_age_group_format(c("<30", "30_35", "40_45", "45+")) # Should return TRUE
is_last_age_group_format(c("<30", "30-35", "40-45", "45-50")) # Should return TRUE
is_last_age_group_format(c("<30", "30-35", "40-45", ">45")) # Should return TRUE
is_last_age_group_format(c("<30", "30-35", "40-45", "45-")) # Should return FALSE
is_last_age_group_format(c("<30", "30-35", "40-45", "45")) # Should return FALSE
} # }
```
