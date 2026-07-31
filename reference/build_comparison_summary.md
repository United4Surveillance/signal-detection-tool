# Build table with summary statistics for selected signals vs rest Calculates the median and quantiles for age, and the male/female ratio, if present in linelist, for cases in the selected signals and for the rest of the cases within the signal detection period.

Build table with summary statistics for selected signals vs rest
Calculates the median and quantiles for age, and the male/female ratio,
if present in linelist, for cases in the selected signals and for the
rest of the cases within the signal detection period.

## Usage

``` r
build_comparison_summary(comparison_linelist)
```

## Arguments

- comparison_linelist:

  list containing two linelist data frames: \`cases\` and
  \`cases_comparison\`, as returned by
  \`build_signal_and_comparison_linelist()\`.

## Value

data frame with summary statistics for selected signals and rest of
cases, formatted as strings.
