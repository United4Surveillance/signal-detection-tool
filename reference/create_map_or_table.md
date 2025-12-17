# Decider for creating a map or a table based on whether all NUTS_ids are found in the shapefile

Decider for creating a map or a table based on whether all NUTS_ids are
found in the shapefile

## Usage

``` r
create_map_or_table(
  signals_agg,
  data_surveillance,
  region,
  shape = get_shp_config_or_internal(),
  interactive = TRUE,
  toggle_alarms = FALSE,
  partial = FALSE
)
```

## Arguments

- signals_agg:

  tibble, aggregated signals over n weeks with columns number of cases,
  any_alarms and n_alarms
  [`aggregate_signals`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md).
  This tibble can contain the aggregated signals for multiple categories
  i.e. state and county.

- data_surveillance:

  data.frame, surveillance linelist

- region:

  character, specifying the variable for the region to be shown should
  be one of ("country","state", "county","community","region_level1",
  "region_level2","region_level3")

- shape:

  sf. The shapefile used for mapping. - If an external shapefile path is
  provided in the configuration file, it is used. - Otherwise, the
  function defaults to the internal European shapefile (\`nuts_shp\`). -
  By default, this parameter is set to \`get_shp_config_or_internal()\`,
  which handles this logic dynamically.

- interactive:

  boolean identifying whether the plot should be static or interactive

- toggle_alarms:

  boolean identifying whether the plot should showing number of signals
  explicitly or only when hovering

- partial:

  logical, add partial bundle to plotly

## Value

a table or a plot depending on whether the matching of the NUTS IDs was
fully possible, the table and plots can be interactive or not depening
on the interactive parameter, can be class "ggplot" or "plotly" for plot
and class "gt_tbl" or "datatables" for table

## Examples

``` r
if (FALSE) { # \dontrun{
signals <- input_example %>%
  preprocess_data() %>%
  get_signals(stratification = c("sex", "county"))
signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
create_map_or_table(signals_agg, input_example, "county", nuts_shp)
} # }
```
