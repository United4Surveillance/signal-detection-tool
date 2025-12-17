# Renders signal detection report

The function supports the generation of a single-pathogen and
multi-pathogen report when \`report_format = "HTML"\`. For Word (DOCX)
output, only single-pathogen reports are currently supported.

## Usage

``` r
run_report(
  data,
  report_format = "HTML",
  method = "FarringtonFlexible",
  number_of_weeks = 6,
  pathogens = NULL,
  strata = c("county", "age_group"),
  tables = TRUE,
  output_file = NULL,
  output_dir = ".",
  signals_padded = NULL,
  signals_agg = NULL,
  intervention_date = NULL,
  custom_logo = NULL,
  custom_theme = NULL,
  min_cases_signals = 1,
  title = NULL
)
```

## Arguments

- data:

  data.frame containing surveillance data in linelist format following
  the format specified in \`input_metadata\`

- report_format:

  character, format of the report: "HTML" or "DOCX"

- method:

  a character string, signal detection method to be used. One of
  "FarringtonFlexible", "EARS" , "CUSUM", "Mean", "Timetrend",
  "Harmonic", "Harmonic with timetrend", "Step harmonic", "Step harmonic
  with timetrend". You can retrieve the full list using
  \[names(available_algorithms())\].

- number_of_weeks:

  integer, number of weeks for which signals are generated

- pathogens:

  A character vector specifying which pathogens to include in the
  report. If \`NULL\` (default), all pathogens present in \`data\`,
  \`signals_padded\`, or \`signals_agg\` are used. Multi-pathogen
  reports are supported only for HTML output.

- strata:

  A character vector specifying the columns to stratify. If \`NULL\` no
  strata are used. If precomputed signals are provided this argument is
  ignored and strata are inferred from the provided signals. Defaults to
  c("county", "age_group") when no precomputed signals were provided.

- tables:

  Logical, default TRUE. True if Signal Detection Tables should be
  included in report. Only used for DOCX reports, the parameter is
  ignored for HTML reports.

- output_file:

  A character string specifying the name of the output file (without
  directory path). If \`NULL\` (default), the file name is automatically
  generated to be SignalDetectionReport. See
  [render](https://pkgs.rstudio.com/rmarkdown/reference/render.html) for
  more details.

- output_dir:

  A character string specifying the output directory for the rendered
  output file (default is ".", which means the rendered file will be
  saved in the current working directory. See
  [render](https://pkgs.rstudio.com/rmarkdown/reference/render.html) for
  more details. \`NULL\` is used when running the report from shiny app
  which will take the Downloads folder as default option for saving.

- signals_padded:

  A tibble of precomputed and padded signals containing a \`pathogen\`
  column. If multiple pathogens are present, the tibble should represent
  all of them stacked together (e.g., using \`dplyr::bind_rows()\`).
  Defaults to \`NULL\`, in which case the signal data will be computed
  from the linelist within the \`run_report()\` function. If not
  \`NULL\`, the provided \`signals_padded\` is used as-is, and signals
  are not recomputed.

- signals_agg:

  A tibble of aggregated signals containing a \`pathogen\` column. If
  multiple pathogens are included, the tibble should represent all of
  them stacked together (e.g., using \`dplyr::bind_rows()\`). Defaults
  to \`NULL\`, in which case the aggregated signals are computed from
  the linelist within the \`run_report()\` function. If not \`NULL\`,
  the provided \`signals_agg\` is used directly and signals are not
  recomputed.

- intervention_date:

  A date object or character of format yyyy-mm-dd or NULL specifying the
  date for the intervention. This can be used for interrupted timeseries
  analysis. It only works with the following methods: "Mean",
  "Timetrend", "Harmonic", "Harmonic with timetrend", "Step harmonic",
  "Step harmonic with timetrend". Default is NULL which indicates that
  no intervention is done.

- custom_logo:

  A character string with a path to a png or svg logo, to replace the
  default United4Surveillance logo. Only used when \`report_format\` is
  \`"HTML"\`.

- custom_theme:

  A bslib::bs_theme() to replace the default United4Surveillance theme.
  This is mainly used to change colors. See the bslib documentation for
  all parameters. Use version = "3" to keep the navbar intact. Only used
  when \`report_format\` is \`"HTML"\`.

- min_cases_signals:

  integer, minimum number of cases a signal must have. All signals with
  case counts smaller than this will be filtered in a post-processing
  step

- title:

  NULL or a character string. Specifies the title of the report that is
  to be created

## Value

the compiled document is written into the output file, and the path of
the output file is returned; see
[render](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Details

If executed as a standalone function, all filtering must be performed
beforehand. This function is also invoked within the app.

## See also

\[names(available_algorithms())\]

## Examples
