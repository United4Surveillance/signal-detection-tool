# Age group comparison plot for signal detection

Creates an interactive bar plot comparing the distribution of cases
across age groups, split by signal status (cases part of a signal vs.
non-signal cases over a specified time window).

## Usage

``` r
plot_agegroup_comparison(data_agg, number_of_time_units, time_unit)
```

## Arguments

- data_agg:

  A data frame containing aggregated case counts. Must include at least
  the variables \`signal\`, \`age_group\`, \`n\`, \`perc\` and
  \`total_n\`. Typically created using \`dplyr::count()\`.

- number_of_time_units:

  Integer scalar. Number of time units signal detection was applied for,
  which defines the time window or cases used for the comparison. Must
  be a single positive integer (\>= 1).

- time_unit:

  a character specifying the time unit the case aggregation is performed
  on. Default is "weekly".

## Value

A Plotly object representing a grouped bar chart of case counts by age
group and signal status.

## Details

The function ensures consistent factor levels, fills missing
combinations with zero counts, and returns a Plotly interactive
visualization with a minimal dashboard-style theme.
