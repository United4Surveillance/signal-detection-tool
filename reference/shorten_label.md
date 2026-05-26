# Shorten labels for plotting

Truncates character labels to a fixed width and replacing the rest with
... .

## Usage

``` r
shorten_label(x, width = 16)
```

## Arguments

- x:

  A character vector of labels.

- width:

  Integer specifying the maximum number of characters to keep.

## Value

A character vector with labels truncated to the specified width.

## Examples

``` r
if (FALSE) { # \dontrun{
shorten_label(c("Very long category name", "Short"))
shorten_label("This is a long string", width = 10)
} # }
```
