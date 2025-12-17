# Plot age-groups grouped by another variable

Plot age-groups grouped by another variable

## Usage

``` r
plot_agegroup_by(
  df,
  age_group_col = "age_group",
  by_col = NULL,
  interactive = FALSE
)
```

## Arguments

- df:

  case data

- age_group_col:

  name of the age-group column

- by_col:

  name of the grouping column

- interactive:

  if TRUE, interactive plot is returned

## Value

either a gg or plotly object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_agegroup_by(
  df = test_data,
  age_group_col = "age_group",
  by_col = "county"
)
} # }
```
