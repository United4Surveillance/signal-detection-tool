# Get Shapefile: Read from Config or Use Internal Dataset

This function retrieves a shapefile based on a path specified in a
configuration file. - If a valid path exists in the configuration, the
shapefile is read from that location. - If no path is provided, the
function defaults to using an internal shapefile (\`nuts_shp\`). - To
improve performance, the shapefile is cached in \`app_cache_env\$shp\`,
preventing multiple unnecessary reads during an application session
(e.g., when plotting maps).

## Usage

``` r
get_shp_config_or_internal()
```

## Value

An \`sf\` object representing the geographic data (either from the
config path or \`nuts_shp\`).
