# checking age_group column only containing digits, separators and \<,\>,+ For the first age_group using seperator, i.e. 00-05, \<5 is allowed. For the last age group using seperators, i.e. 95-100, \>100 and 100+ is allowed. Separators like - (dash), \_ (underscore), and — (em dash) are also allowed for intermediate age ranges.

checking age_group column only containing digits, separators and \<,\>,+
For the first age_group using seperator, i.e. 00-05, \<5 is allowed. For
the last age group using seperators, i.e. 95-100, \>100 and 100+ is
allowed. Separators like - (dash), \_ (underscore), and — (em dash) are
also allowed for intermediate age ranges.

## Usage

``` r
is_age_group_format(col)
```

## Arguments

- col:

  character vector, vector containing age groups to check

## Value

boolean, when TRUE all values of col are in the required format, when
FALSE at least one is not in required format

## Examples

``` r
if (FALSE) { # \dontrun{
is_age_group_format(c("<30", "30-35", "40-45", "45+")) # Should return TRUE
is_age_group_format(c("<30", "30-35", "40-45", ">45")) # Should return TRUE
is_age_group_format(c("below_one", "one_to_five")) # Should return FALSE
is_age_group_format(c("<30", "30-35", "40/45", "45+")) # Should return FALSE
is_age_group_format(c("<30", "30-35", "40+45", "45+")) # Should return FALSE
} # }
```
