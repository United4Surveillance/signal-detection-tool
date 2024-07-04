# SignalDetectionTool 0.2.1

* Losening restriction on which region information can be used for creating maps. Now all region variables including region_level variables can be used which is necessary for users who want to use their own shapefiles for their defined regions.
* Changed the title of the tool to "Signal Detection Tool".
* Implemented cards and boxes by using `bslib` package.

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
