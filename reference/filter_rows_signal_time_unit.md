# Filter linelist to one signal week and stratum

Filters a linelist to the ISO week represented by one row of signal
results. If the signal row contains a non-missing \`category\` and
\`stratum\`, the linelist is additionally restricted to that stratum.

## Usage

``` r
filter_rows_signal_time_unit(signal_row, linelist)
```

## Arguments

- signal_row:

  A one-row data frame from weekly aggregated signal results containing
  at least \`year\` and \`week\`/\`month\`. May also contain
  \`category\` and \`stratum\` for stratified signals.

- linelist:

  A data frame containing a linelist of surveillance data. Must contain
  a \`date_report\` column and, for stratified signals, the column named
  in \`signal_row\$category\`, i.e. "age_group" or "sex".

## Value

A filtered linelist containing cases reported during the selected signal
week and, if applicable, matching the selected stratum.
