# SignalDetectionTool 0.5.1

* Bug fix in completing aggregated case data with 0 for cases for 53th isoweek.
* Change function name add_rows_missing_dates() to add_missing_isoweeks()
* Only perform completion of aggregated data not in add_missing_isoweeks() and not filtering by dates as it was done before in add_rows_missing_dates()
* Implement a seperate function to filter linelist given date_start and date_end
* Integrate the add_missing_isoweeks() into the aggregate_data() function such that the aggregated data is complete after having applied aggregate_data()


# SignalDetectionTool 0.5.0

* Adding the possibility to filter the data by age.
* Adding column "cases in outbreak" to the signal detection results table. This is based on an optional variable outbreak_status in the dataset.
* The value unknown can be chosen as filter value when present in the selected filter variables. 
* Allow `run_report()` to work as a standalone function.
* The output directory can be specified when using `run_report()` function 

# SignalDetectionTool 0.4.0

* Allowing the `plot_time_series()` to work without exected_pad and upperbound_pad, i.e. directly with the signal detection results
* Adding six outbreak detection methods which are all GLM based. Simple Mean, Timetrend, Harmonic, Harmonic with timetrend, Farrington like GLM and Farrington like GLM with timetrend to model the expected number of cases.
* Adding the possibility to do a pandemic correction based on these GLM methods. Pandemic correction fits a seperate intercept and possibly timetrend depending on the model chosen.
* Bug fix inside `preprocess_data()` so that the tool works again when the age variable is not included in the data.
* Fixing bug so the legend in the interactive version of the plot_regional function only shows the border-color


# SignalDetectionTool 0.3.0

* Losening restriction on which region information can be used for creating maps. Now all region variables including region_level variables can be used which is necessary for users who want to use their own shapefiles for their defined regions.
* Bug fix inside `format_table()` so that signals tab is working again when no stratification is chosen.
* Changed the title of the tool to "Signal Detection Tool".
* Implemented cards and boxes by using `bslib` package.
* Fixing bug so that plot_time_series() works with upperbond_pad and expected_pad being all NA
* Added notification about negative values in age.
* Improvements in the documentation of `build_signals_table()`, `prepare_signals_agg_table()`

# SignalDetectionTool 0.2.0

* Several bug fixes concerning `age_groups()` function. Fixed that NA in age column is dealt with correctly in age_group, fixed that when no age column is provided in the dataset function still works and further details making the function more robust. 
* Added EU emblem
* Switched off warnings shown in the console
* Updates and bug fixes to `plot_time_series()` 
  - show the blue background, signifying signal detection period, correctly, when using `facet_grid` for strata in report. 
  - Improved general readability of `timeseries` plot, when used in relation to `facet`.
  - Improved positioning of bars for case numbers and blue background coverage.
  - Interactive plot in app and HTML report now adapts y-range according to x-zoom and the x-axis do not anymore extend into the future.
* Correcting text specifying the chosen signal detection period in Input tab.
* Changes appearance of all tables, removes rownames and alarm column, more concise grouping
* Adds filter and export functionality to all tables
* Adds highlighting and sorting to strata tables
* Added additional data checks for age_group column and provided more detailed format requirement in description.md. Implemented `is_age_group_format()`, `is_last_age_group_format()` and `check_type_and_value_age_group()`.
* Added spinner indicating that the app is busy to all tabs.
* Interactive HTML option is now only available if format HTML is selected.
* Stripping leading and trailing whitespaces from all columns of the input data in `preprocess_data()`.
* Unified wording of "Signals"; changed all instances of "Alarms" to "Signals".
* Fixing that the Help Tab works when installing package from binary.
* Seperating the formating of signal detection results tables from preprocessing in seperate functions.
* Renaming the functions such that the functionality provided by former `create_results_table()` is now `build_signals_table()` and `create_stratified_table()` is now `build_signals_agg_table()` with the additional parameter format to obtain a data.frame, Datatable or Flextable. This makes it easier for users to use this function from outside.
* Allow selection of all character and factor variables in the dataset, except for the variables ending with '_id', to use for filters and stratifications.
* Adding a function `pad_signals()` performing the padding of signals timeseries (extension of threshold and expecation to the past for visualisation) which was previously done in the signals tab server.
* adding method and number_of_weeks to the resulting tibble of `get_signals()` to reduce number of parameters needed to be passed forward.
* Improved wording of objects in signals and reports tab
* Made table appearance for interactive and static tables consistent using new dependency `flextable`
* Recodes NAs in signal tables to 'unknown' for all stratifications but not for the unstratified level

# SignalDetectionTool 0.1.0

* First version of the tool provided to piloting countries.
* Data Check, Input parameter, Signal and Report Tab are implemented.
* SignalDetection Algorithms FarringtonFlexible, EARS and CUSUM with reset are implemented.
* Interactivity in the Signal tab is realised using plotly and additional shiny buttons.
* User can download a doxc or html report in the Report tab.

# SignalDetectionTool 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
