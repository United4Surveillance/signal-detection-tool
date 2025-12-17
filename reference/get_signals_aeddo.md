# Automated and Early Detection of Disease Outbreaks

This function retrieves outbreak signals using the aeddo algorithm. It
processes surveillance data, aggregates it based on specified date
variables, and applies the aeddo algorithm to detect potential
outbreaks.

## Usage

``` r
get_signals_aeddo(
  data_aggregated,
  number_of_weeks = 52,
  population_size = 1,
  sig_level = 0.95,
  exclude_past_outbreaks = TRUE,
  k = 52 * 3,
  init_theta = c(rep(0, 4), 1),
  lower = c(-1, -0.01, -0.8, -0.8, -6),
  upper = c(100, 0.5, 1, 1, 10),
  method = "L-BFGS-B"
)
```

## Arguments

- data_aggregated:

  data.frame, aggregated data with case counts

- number_of_weeks:

  integer, specifying the number of weeks to generate signals for

- population_size:

  The population size for the aeddo algorithm. Default is 1.

- sig_level:

  The quantile from the random effects distribution used for defining
  the for outbreak detection threshold, a numeric value between 0 and 1.

- exclude_past_outbreaks:

  logical value indicating whether past outbreak related observations
  should be excluded from future parameter estimation.

- k:

  An integer specifying the rolling window size employed for parameter
  estimation.

- init_theta:

  Initial values for model parameters in optimization.

- lower:

  Lower bounds for optimization parameters.

- upper:

  Upper bounds for optimization parameters.

- method:

  The optimization method to use, either "BFGS" (default) or "L-BFGS-B".

## Value

An object containing outbreak signals detected by the aeddo algorithm.

## References

For information on the aeddo algorithm, refer to the package
documentation.

## See also

[`aeddo`](https://ssi-dk.github.io/aeddo/reference/aeddo.html) for
details on the aeddo algorithm.

## Examples

``` r
if (FALSE) { # \dontrun{
data_aggregated <- input_example %>%
  preprocess_data() %>%
  aggregate_data() %>%
  add_rows_missing_dates()
results <- get_signals_aeddo(data_aggregated)
} # }
```
