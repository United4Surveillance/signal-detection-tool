# Filter linelist to the signal detection period

Determines the date range covered by the time units in
\`signals_padded\` for which signal results are available, and filters
the linelist to this period.

## Usage

``` r
filter_rows_past_time_units(signals_padded, linelist)
```

## Arguments

- signals_padded:

  A data frame containing padded signal detection results. Must contain
  \`year\`, \`week\` or \`month\`, and \`alarms\` columns.

- linelist:

  A data frame containing case-level surveillance data. Must contain a
  \`date_report\` column.

## Value

A filtered linelist containing cases reported during the signal
detection period.

## Details

The start of each time unit is calculated from the \`year\` and \`week\`
or \`year\` and \`month\` columns respectively. The returned linelist
covers the period from the Monday of the first signal week to the Sunday
of the last signal week, if weeks are selected as time unit.
