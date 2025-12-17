# Age Group Format Check

This function checks the format of the 'age_group' variable in the given
data frame. It performs several checks including: 1. Checking if the
lengths of age groups are equidistant. 2. Verifying if the age group
format is in the "xx-xx" format. 3. Checking if any punctuation
characters are used at either end of the age group.

## Usage

``` r
age_format_check(df)
```

## Arguments

- df:

  A data frame containing an 'age_group' variable.

## Value

A list containing the results of the formatting checks:

- agegrp_div:

  The most frequently used punctuation character, which serves as the
  divider in age groups.

- other_punct_character:

  Any other punctuation character used. Also used as logical indicator.

- equal_sizing:

  Logical indicating whether the lengths of age groups are equidistant.

- format_agegrp_xx:

  Indices of entries in 'age_group' not following the "xx-xx" format.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
data_frame <- data.frame(age = c(2, 5, 15, 16), age_group = c("01-05", "6-10", "11-15", "16-20"))
check_results <- age_format_check(data_frame)
print(check_results)
} # }
```
