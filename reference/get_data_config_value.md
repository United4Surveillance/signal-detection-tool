# Retrieve a Configuration Value from DATA_CONFIG

This function retrieves a configuration value from the global
\`DATA_CONFIG\` list using a colon-separated parameter name. If the
parameter is not found, a default value is returned. Optionally, the
retrieved value can be checked against a set of acceptable values.

## Usage

``` r
get_data_config_value(
  parameter_name,
  default_value = NULL,
  acceptable_values = NULL
)
```

## Arguments

- parameter_name:

  A character string specifying the configuration parameter. The
  parameter can be a colon-separated path to a nested value.

- default_value:

  The value to return if the parameter is not found in \`DATA_CONFIG\`.
  Defaults to \`NULL\`.

- acceptable_values:

  An optional vector of acceptable values. If provided, the function
  returns the intersection of the retrieved value and this set. If no
  intersection is found, the \`default_value\` is returned.

## Value

The retrieved configuration value if found. If \`acceptable_values\` is
provided, the function returns the intersection of the retrieved value
and \`acceptable_values\`, or \`default_value\` if there is no
intersection. If the parameter is not found, \`default_value\` is
returned.

## Examples

``` r
# Assuming DATA_CONFIG is defined as:
DATA_CONFIG <- list(api = list(endpoint = "https://example.com", timeout = 30))

# Retrieve a nested value
get_data_config_value("api:endpoint", default_value = "https://fallback.com")
#> [1] "https://fallback.com"

# Retrieve with acceptable values
get_data_config_value("api:timeout", default_value = 10, acceptable_values = c(10, 20, 30))
#> [1] 10
```
