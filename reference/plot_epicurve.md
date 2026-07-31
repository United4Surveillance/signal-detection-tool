# Plot epidemic curve for signal detection for different strata

Creates an interactive epidemic curve visualizing the daily case counts
by stratum category for selected signals.

## Usage

``` r
plot_epicurve(data_cases, stratum = NULL)
```

## Arguments

- data_cases:

  A data frame containing case-level data. It has to include the
  variables \`signal\` and \`date_report\`. If \`stratum\` is not
  \`NULL\`, the corresponding stratum variable must also be present.

- stratum:

  A character specifying the stratum which should be used for the
  epicurve

## Value

A Plotly object representing an epicurve of case counts by selected
stratum for each reporting day for each selected signal.

## Details

The function ensures consistent factor levels, fills missing
combinations with zero counts, and returns a Plotly interactive
visualization with a minimal dashboard-style theme.
