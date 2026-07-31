# Build linelists for selected signal cases and comparison cases

Creates two linelists for visualisation or case review: one containing
cases that occurred in selected signals, and one comparison linelist
containing cases from the signal detection period that are not already
included in the selected signal cases.

## Usage

``` r
build_signal_and_comparison_linelist(
  selected_signal_ids,
  true_signals,
  signals_padded,
  filtered_data
)
```

## Arguments

- selected_signal_ids:

  Integer vector of row positions in \`true_signals\` identifying the
  signal rows selected by the user.

- true_signals:

  A data frame containing signal rows, typically filtered to true or
  selected signals. Must contain \`year\` and \`week\`/\`month\`, and
  may contain \`category\` and \`stratum\`.

- signals_padded:

  A data frame containing padded signal detection results. Used to
  determine the full comparison period. Must contain \`year\`,
  \`week\`/\`month\`, and \`alarms\`.

- filtered_data:

  A case linelist after applying the current app filters. Must contain
  \`case_id\` and \`date_report\`.

## Value

A list with two elements:

- cases:

  A data frame of cases from the selected signal weeks, with a
  \`signal_id\` column added.

- cases_comparison:

  A data frame of cases from the wider signal detection period,
  excluding cases already present in \`cases\`.
