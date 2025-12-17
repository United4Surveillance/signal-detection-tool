# Function to retrieve name from named vector given its value Can be used to retrieve the "pretty" names to show to the user and in the background work with the values

Function to retrieve name from named vector given its value Can be used
to retrieve the "pretty" names to show to the user and in the background
work with the values

## Usage

``` r
get_name_by_value(value, named_vector)
```

## Arguments

- value:

  character, value of the named list, i.e. "farrington"

- named_vector:

  named vector

## Value

character, name of the named_vector corresponding to value

## Examples

``` r
if (FALSE) { # \dontrun{
get_name_by_value("farrington", available_algorithms())
} # }
```
