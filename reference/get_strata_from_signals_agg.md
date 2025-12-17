# Extract strata from precomputed signals_agg

This helper function returns the list of stratification variables used
to generate a \`signals_agg\` object. If no stratification was applied
(i.e., all values in the \`category\` column are \`NA\`), it returns
\`NULL\`.

## Usage

``` r
get_strata_from_signals_agg(signals_agg)
```

## Arguments

- signals_agg:

  A data frame containing precomputed signal aggregations, produced by
  \[aggregate_signals()\]. Must include a \`category\` column that
  encodes the stratification variable(s).

## Value

A character vector of stratification values (e.g., \`"age_group"\`,
\`"county"\`), or \`NULL\` if no stratification was applied.
