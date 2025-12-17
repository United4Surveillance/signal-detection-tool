# check that shapefile contains NUTS_ID, LEVL_CODE, and CNTR_CODE

check that shapefile contains NUTS_ID, LEVL_CODE, and CNTR_CODE

## Usage

``` r
check_columns_shapefile(shape)
```

## Arguments

- shape:

  shapefile object loaded using sf::st_read()

## Value

if check passess, returns TRUE invisibly. If not, throws an error
message
