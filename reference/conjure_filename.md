# Conjure Filename

Generate an output filename based on the results from get_signals.

## Usage

``` r
conjure_filename(data)
```

## Arguments

- data:

  The data frame containing information for filename generation.

## Value

The generated filename with the directory path, i.e.
signals_farrington_Austria_Pertussis_2022-01-01_2023-07-08\_.csv

## Examples

``` r
# Generate a filename based on results data
if (FALSE) { # \dontrun{
data_preprocessed <- input_example %>% preprocess_data()
data_preprocessed %>%
  get_signals() %>%
  conjure_filename()
} # }
```
