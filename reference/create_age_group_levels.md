# Creation of age_group levels from different formats of the age_group column

This function returns a character vector with age_group levels based on
the data provided. It uses the age_format_check() and
complete_agegrp_arr() functions to create the levels

## Usage

``` r
create_age_group_levels(df)
```

## Arguments

- df:

  A data frame containing an 'age_group' variable.

## Value

character vector containing all age_group levels

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
data_frame <- data.frame(age = c(2, 6, 16), age_group = c("01-05", "6-10", "16-20"))
create_age_group_levels(data_frame)
} # }
```
