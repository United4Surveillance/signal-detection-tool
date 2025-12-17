# Create age grouping variable for a given data set

If \`df\$age_group\` already exists, it is only reformatted and recoded
as a factor with complete, ordered levels. Otherwise a new \`age_group\`
variable is created based on \`df\$age\`.

## Usage

``` r
age_groups(df, break_at = NULL)
```

## Arguments

- df:

  A data frame containing at least one of the columns \`age\` or
  \`age_group\`. If \`age_group\` already exists, it is not recomputed
  but only reformatted and its levels are completed.

- break_at:

  An integer vector specifying additional lower bounds of age groups
  (excluding 0, which is always used as the first lower bound). If
  \`NULL\` (the default), 5-year age groups 00-04, 05-09, ..., 125+ are
  used.

## Value

The input data frame \`df\` with a factor column \`age_group\` added (or
reformatted) and levels stored in \`app_cache_env\$age_group_levels\`.

## Details

Ages are grouped into intervals of the form \`"LL-UU"\` or \`"LL+"\`,
with lower and upper bounds zero-padded to two digits.

## Examples

``` r
if (FALSE) { # \dontrun{
input_path <- "data/input/input_sample.csv"
data <- read.csv(input_path, header = TRUE, sep = ",")
data$age <- sample(1:125, 10, replace = TRUE)
age_groups(data) # default age groups
age_groups(data, c(15L, 35L, 65L, 100L)) # custom age groups (lower bounds)
} # }
```
