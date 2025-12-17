# Helper to check that values of a character variable are in given levels

Helper to check that values of a character variable are in given levels

## Usage

``` r
check_character_levels(vector, levels)
```

## Arguments

- vector:

  a vector i.e. column of the data whose values should be checked

- levels:

  vector with levels the vector should have

## Value

boolean, TRUE when all values are inside levels, FALSE if there is a
value which was not specified in levels
