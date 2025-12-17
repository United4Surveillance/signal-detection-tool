# Create a data.frame with sine and cosine components for harmonic modeling.

This function generates a data.frame with sine and cosine values based
on the provided time series length and frequency. It is primarily used
to create harmonic models that describe periodic patterns in time series
data. The dataframe will be used to fit parameters of a glm with sin and
cos elements.

## Usage

``` r
create_sincos_data(ts_len, freq = 52, S = 1)
```

## Arguments

- ts_len:

  integer, specifying the length of the time series.

- freq:

  integer, default 52, specifying the frequency of the sine and cosine
  waves. When the timeseries is aggregated weekly then freq = 52.

- S:

  integer, default 1, specifying the number of cycles per freq. When
  freq = 52 this specifies the number of cycles to have per year

## Value

A data frame with columns for the sine and cosine values over time.
