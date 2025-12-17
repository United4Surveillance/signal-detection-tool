# Create a Date from ISO Year and Week

This function converts an ISO year and ISO week number into a date. The
date returned corresponds to the first day (Monday) of the specified ISO
week.

## Usage

``` r
isoweek_to_date(week, year)
```

## Arguments

- week:

  Integer. The ISO week number (1 to 53).

- year:

  Integer. The ISO year.

## Value

A \`Date\` object representing the first day of the specified ISO week.

## Examples

``` r
if (FALSE) { # \dontrun{
isoweek_to_date(week = 15, year = 2023) # Returns the date for the first day of ISO week 15 in 2023
} # }
```
