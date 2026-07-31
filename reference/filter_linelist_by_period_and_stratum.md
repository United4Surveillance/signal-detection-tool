# Filter linelist by reporting period and optional stratum

Filters a case linelist to a given reporting date interval. If a stratum
definition is provided, the linelist is additionally filtered to the
corresponding category-stratum combination.

## Usage

``` r
filter_linelist_by_period_and_stratum(
  linelist,
  start_date,
  end_date,
  signal_row = NULL
)
```

## Arguments

- linelist:

  A data frame containing case-level surveillance data. Must contain a
  \`date_report\` column.

- start_date:

  Start date of the reporting period. Rows with \`date_report \>=
  start_date\` are retained.

- end_date:

  End date of the reporting period. Rows with \`date_report \<=
  end_date\` are retained.

- signal_row:

  Optional one-row data frame from the signal results containing the
  columns \`category\` and \`stratum\`. If provided, and the value of
  \`category\` is not NA, the linelist is filtered to rows where the
  column named in \`category\` equals \`stratum\`.

## Value

A filtered data frame containing linelist rows within the selected
reporting period and, if applicable, the selected stratum.
