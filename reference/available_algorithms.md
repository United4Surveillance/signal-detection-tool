# List Available Signal Detection Algorithms

This function returns a named character vector of the signal detection
algorithms currently supported by the package. The names describe the
algorithm shown in the shiny app, and the corresponding values are the
method identifiers used when calling functions internally like
\[get_signals()\].

## Usage

``` r
available_algorithms()
```

## Value

A named character vector of available algorithms. The names are shown in
the app to the user, and \#' the values represent the internal method
names to be used in function calls.
