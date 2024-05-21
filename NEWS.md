# SignalDetectionTool 0.1.1

* Several bug fixes concerning `age_groups()` function. Fixed that NA in age column is dealt with correctly in age_group, fixed that when no age column is provided in the dataset function still works and further details making the function more robust. 
* Updated `timeseries()` to show the blue background, signifying signal detection period, correctly, when using `facet_grid` for strata in report. 
* Improved general readability of `timeseries` plot, when used in relation to `facet`.
* Correcting text specifying the chosen signal detection period in Input tab.
* Added additional data checks for age_group column and provided more detailed format requirement in description.md. Implemented `is_age_group_format()`, `is_last_age_group_format()` and `check_type_and_value_age_group()`.
* Added spinner indicating that the app is busy to all tabs.
* Interactive HTML option is now only available if format HTML is selected.
* Stripping leading and trailing whitespaces from all columns of the input data in `preprocess_data()`.

# SignalDetectionTool 0.1.0

* First version of the tool provided to piloting countries.
* Data Check, Input parameter, Signal and Report Tab are implemented.
* SignalDetection Algorithms FarringtonFlexible, EARS and CUSUM with reset are implemented.
* Interactivity in the Signal tab is realised using plotly and additional shiny buttons.
* User can download a doxc or html report in the Report tab.

# SignalDetectionTool 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
