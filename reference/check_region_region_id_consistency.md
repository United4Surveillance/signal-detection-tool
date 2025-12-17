# Check whether the region and corresponding region_id columns only have one region name per ID

Check whether the region and corresponding region_id columns only have
one region name per ID

## Usage

``` r
check_region_region_id_consistency(data, regions)
```

## Arguments

- data:

  data.frame, raw linelist of surveillance cases

- regions:

  vector of strings, specifying the region variable names, i.e.
  c("county","community")

## Value

list, empty when no errors occured or filled with error messages
