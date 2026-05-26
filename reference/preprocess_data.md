# Preprocessing of linelist surveillance data with or without outbreak_ids

Preprocessing of linelist surveillance data with or without outbreak_ids

## Usage

``` r
preprocess_data(data)
```

## Arguments

- data:

  data.frame, Linelist of surveillance data

## Value

data.frame, preprocessed linelist with transformation of columns to
date, to lower, generation of isoyear and isoweek

## Examples

``` r
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
data_preprocessed
} # }
```
