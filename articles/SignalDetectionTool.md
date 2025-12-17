# SignalDetectionTool: R Package for signal detection on infectious disease surveillance data

The **SignalDetectionTool** (SDT) is an R package which contains an R
shiny app to perform signal detection on infectious disease surveillance
data and visualise results. Furthermore the functions of this R package
can also be used standalone without running the shiny application. This
vignette gives guidance on how to use the functions within the R
package.

## Running the App

This section is extensively described in the README of the package thus
we give minimal instructions here. First install and load the SDT
package.

``` r
library(SignalDetectionTool)
```

Run the app entering the following command in the console.

``` r
run_app()
```

It is possible to run the app with customised settings using a yaml
file. Please have a look at the README.

## Using functions from the SDT

### Data

The input to the SDT is a linelist of infectious disease cases
structured according to the format defined in `input_metadata`. A sample
linelist (`input_example`) and its corresponding metadata specification
(`input_metadata`) is available in the package as internal datasets and
can be accessed using:

``` r
data("input_example")
data("input_metadata")
```

### Applying signal detection

To apply signal detection to your data for the most recent 6 weeks you
first need to preprocess your linelist and then apply
[`get_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md).
Inside
[`get_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)
the linelist is aggregated to a timeseries containing weekly counts of
cases.  
To apply different signal detection methods use the parameter method. To
generate stratified signals use the parameter stratification. Please
have a look at the function documentation for more details. This is an
example for signals generated using the default paramters with
FarringtonFlexible and no stratification:

``` r
data_prepro <- input_example %>% preprocess_data()
signals <- data_prepro %>% get_signals()
```

The generated `signals` output is a data.frame containing the aggregated
number of cases per week with additional columns `cases_in_outbreak`,
`alarms`, `upperbound`, `expected`, `category`, `stratum`, `method` and
`number_of_weeks`.

- `cases_in_outbreak` is only generated if the column `outbreak_status`
  was available in your provided linelist. It counts how many cases in
  this week were already part of a known outbreak.  
- `alarms` is a column with booleans indicating for which weeks a signal
  has been generated. Of course the majority of weeks in the aggregated
  timeseries contain `NA` in this column as only the most recent
  selected `number_of_weeks` are filled with `TRUE` of `FALSE`.  
- `upperbound` contains a numeric threshold calculated with the signal
  detection methods. As for `alarms` the majority of weeks in the
  aggregated timeseries contain `NA` as this is only calculated for the
  `number_of_weeks` for which signals are generated. Furthermore some
  algorithms such as EARS do not calculate a threshold.  
- `expected` is a numeric value giving the expected number of cases by
  the signal detection algorithm. Only filled for the weeks for which
  signals are generated.  
- `category` Character string specifying to which stratum timeseries
  belongs to when having added stratification parameters (more details
  in the next example). For unstratified timeseries it is `NA`.  
- `stratum` Character string specifying to which category of the stratum
  the timeseries belongs to when having added stratification parameters
  (more details in the next example). For unstratified timeseries it is
  `NA`.  
- `method` Character string specifying the signal detection method which
  was used.
- `number_of_weeks` Integer specifying the number of weeks for which
  signals were generated.

Generating stratified signals using EARS:

``` r
data_prepro <- input_example %>% preprocess_data()
signals_ears_stratified <- data_prepro %>% get_signals(
  method = "ears",
  stratification = c("age_group", "county")
)
```

The `signals_ears_stratified` output is a data frame in long format,
containing multiple time series of weekly aggregated case counts. Each
time series corresponds to a unique combination of the specified
stratification variables, with all strata stacked vertically.

The columns `category` and `stratum` identify the individual timeseries.
In this example we obtain the following values:  
\* `category` is filled with the column names provided in the
`stratification` parameter.  
\* `stratum` with the values of these variables.

In this example we obtain:  
\* `category` is filled with “age_group” and “county”  
\* `stratum` contains “00–04”, “05–09”, “10–14”, “15–19”, “20–24”,
“25–29”, “30–34”, “35–39”, “40–44”, “45–49”, “50–54”, “55–59”, “60–64”,
“65–69”, “70–74”, “75–79”, “80–84”, “85–89”, “90–94”, “95–99”,
“100–104”, “105–109” for rows with `category == "age_group"`. For rows
with `category == "county"` the values of `stratum` are “Burgenland”,
“Kärnten”, “Niederösterreich”, “Oberösterreich”, “Salzburg”,
“Steiermark”, “Tirol”, “Vorarlberg”, “Wien”.

### Create visualisations and tables

Coming soon…

### Run the report

#### Single-pathogen reporting

You can directly generate an HTML or Word report containing signal
results and visualizations using
[`run_report()`](https://united4surveillance.github.io/signal-detection-tool/reference/run_report.md).
Simply pass your surveillance linelist—structured according to the
format defined in `input_metadata`—to
[`run_report()`](https://united4surveillance.github.io/signal-detection-tool/reference/run_report.md).
The data will be preprocessed automatically within the function. The
following code chunk generates a Word report using EARS without any
stratification (default) for the past six weeks (default):

``` r
run_report(input_example,
  report_format = "DOCX",
  method = "EARS"
)
```

This generates an HTML report (default) with stratification by age
group, county and sex for the last 2 weeks using FarringtonFlexible
(default):

``` r
run_report(input_example,
  strata = c("age_group", "county", "sex"),
  number_of_weeks = 2
)
```

#### Multi-pathogen reporting (HTML)

The following functionalities are currently only available for HTML
reports. They are not supported in Word (DOCX) format. The package also
supports generating a single HTML report for multiple pathogens.  
To use this functionality, you must provide a line list that includes
cases from different pathogens.

An example line list is available in the package as an internal dataset:
`input_example_multipathogen`.

You can access it with:

``` r
data("input_example_multipathogen")
```

To run an HTML report with the EARS algorithm and stratification by
age_group and county for all pathogens in the line list you can run:

``` r
run_report(input_example_multipathogen,
  strata = c("age_group", "county"),
  method = "EARS"
)
```

If you want to generate a report for a subset of pathogens in your
linelist you can specify these using the pathogens parameter:

``` r
run_report(input_example_multipathogen,
  pathogens = c("Enterobacter", "Salmonella"),
  strata = c("age_group", "county"),
  method = "EARS"
)
```

In addition you can customise the HTML report by replacing the default
United4Surveillance logo in the top right corner with your own logo. To
do this, provide the path to a .png or .svg file using the custom_logo
parameter.

``` r
run_report(input_example_multipathogen,
  pathogens = c("Enterobacter", "Salmonella"),
  strata = c("age_group", "county"),
  method = "EARS",
  custom_logo = "path/to/my/custom_logo.png"
)
```
