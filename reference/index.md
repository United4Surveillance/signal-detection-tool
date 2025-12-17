# Package index

## All functions

- [`add_cw_iso()`](https://united4surveillance.github.io/signal-detection-tool/reference/add_cw_iso.md)
  : Adds a column \`cw_iso\` to the data.frame \`data\`.
- [`age_format_check()`](https://united4surveillance.github.io/signal-detection-tool/reference/age_format_check.md)
  : Age Group Format Check
- [`age_groups()`](https://united4surveillance.github.io/signal-detection-tool/reference/age_groups.md)
  : Create age grouping variable for a given data set
- [`aggregate_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_data.md)
  : Aggregates case data (linelist, i.e. one row per case) by isoyear
  and isoweek and adds missing isoweeks to the aggregated dataset.
  Additionally number of cases part of a known outbreak is added if the
  variable outbreak_status exists in the data.
- [`aggregate_pad_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_pad_signals.md)
  : Aggregate and Pad Signals for Reporting
- [`aggregate_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/aggregate_signals.md)
  : Aggregate cases and signals over the number of weeks. First the
  signals are filtered to obtain the signals for the last n weeks
  aggregating the number of cases observed, create variable any signal
  generated and the aggregate the number of signals
- [`algo.cusum_with_reset()`](https://united4surveillance.github.io/signal-detection-tool/reference/algo.cusum_with_reset.md)
  : Implementation of the CUSUM algorithm retrieved from the
  surveillance package and adapted so that after a signal was triggered
  the cusum is set to 0 Parameters are inherited from surveillance
  algo.cusum
- [`available_algorithms()`](https://united4surveillance.github.io/signal-detection-tool/reference/available_algorithms.md)
  : List Available Signal Detection Algorithms
- [`build_empty_datatable()`](https://united4surveillance.github.io/signal-detection-tool/reference/build_empty_datatable.md)
  : Create an Empty DataTable with a Custom Message
- [`build_signals_agg_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/build_signals_agg_table.md)
  : Builds the aggregated signal detection results table with different
  formating options.
- [`build_signals_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/build_signals_table.md)
  : Builds the signal detection results table with different formating
  options. To get the raw data.frame containing method ald
  number_of_weeks as well use format = "data.frame", to obtain nicely
  formated tables in an interactive DataTable or as Flextable use format
  = "DataTable" or format = "Flextable".
- [`check_any_age()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_any_age.md)
  : Helper function to check for presence of age variable or instead
  age_group
- [`check_character_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_character_levels.md)
  : Helper to check that values of a character variable are in given
  levels
- [`check_columns_shapefile()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_columns_shapefile.md)
  : check that shapefile contains NUTS_ID, LEVL_CODE, and CNTR_CODE
- [`check_empty_rows()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_empty_rows.md)
  : check whether there is a completely empty row in provided
  surveillance data
- [`check_for_missing_values()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_for_missing_values.md)
  : Varible names which should be checked for missing values
- [`check_mandatory_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_mandatory_variables.md)
  : checking mandatory variables in the surveillance data check if
  mandatory variables are present in the data check if they have the
  correct type and correct values
- [`check_presence_mandatory_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_presence_mandatory_variables.md)
  : checking presence of mandatory variables in surveillance data
- [`check_raw_surveillance_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_raw_surveillance_data.md)
  : Checking whether raw surveillance linelist fulfills requirements to
  the data specified in the SOP Checking presence and correct type of
  mandatory variables Checking type and values of optional variables
- [`check_region_region_id_consistency()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_region_region_id_consistency.md)
  : Check whether the region and corresponding region_id columns only
  have one region name per ID
- [`check_type_and_value_age_group()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_age_group.md)
  : Checking type and values of the age_group column
- [`check_type_and_value_case_id()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_case_id.md)
  : Checking type of case_id, duplication or missing case_id
- [`check_type_and_value_date()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_date.md)
  : Checking type and values of date variables date variable can be of
  type character or date
- [`check_type_and_value_mandatory_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_mandatory_variables.md)
  : Checking those mandatory variables which are present in the data for
  their type
- [`check_type_and_value_optional_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_optional_variables.md)
  : Checking correct type and value of optional variables which are
  present in the data
- [`check_type_and_value_yes_no_unknown()`](https://united4surveillance.github.io/signal-detection-tool/reference/check_type_and_value_yes_no_unknown.md)
  : Checking type and values of variables which should have
  yes,no,unknown values
- [`complete_agegrp_arr()`](https://united4surveillance.github.io/signal-detection-tool/reference/complete_agegrp_arr.md)
  : Complete Age Group Array
- [`conjure_filename()`](https://united4surveillance.github.io/signal-detection-tool/reference/conjure_filename.md)
  : Conjure Filename
- [`convert_columns_integer()`](https://united4surveillance.github.io/signal-detection-tool/reference/convert_columns_integer.md)
  : Convert specified columns to integer type
- [`convert_to_sts()`](https://united4surveillance.github.io/signal-detection-tool/reference/convert_to_sts.md)
  : Turns aggregated data into surveillance's sts format
- [`create_age_group_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_age_group_levels.md)
  : Creation of age_group levels from different formats of the age_group
  column
- [`create_barplot_or_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_barplot_or_table.md)
  : Decider function to create barplot or table of aggregated cases with
  signals Depending on the number of unique levels to visualise it is
  decided whether a barplot or a table is shown. The aggregated number
  of cases for each stratum and whether any signal are shown.
- [`create_baseline()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_baseline.md)
  : Create a data.frame with a constant baseline for an intercept only
  regression model.
- [`create_factor_with_unknown()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_factor_with_unknown.md)
  : Create a factor out of the stratum column with transforming NA to
  unknown
- [`create_fn_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_fn_data.md)
  : Create a data.frame with 10 seasgroups components for harmonic
  modeling.
- [`create_formula()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_formula.md)
  : Create a model formula based on the columns in the model_data
  dataframe.
- [`create_map_or_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_map_or_table.md)
  : Decider for creating a map or a table based on whether all NUTS_ids
  are found in the shapefile
- [`create_model_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_model_data.md)
  : Create Model Data for Generalized Linear Modeling
- [`create_sincos_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_sincos_data.md)
  : Create a data.frame with sine and cosine components for harmonic
  modeling.
- [`create_time_trend()`](https://united4surveillance.github.io/signal-detection-tool/reference/create_time_trend.md)
  : Create a data.frame with variable time trend for regression
  modeling.
- [`cusum_with_reset()`](https://united4surveillance.github.io/signal-detection-tool/reference/cusum_with_reset.md)
  : Wrapper around the algo.cusum_with_reset Copied from the code of the
  surveillance package and adapted for algo.cusum_with_reset
- [`decider_barplot_map_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/decider_barplot_map_table.md)
  : Decider function whether create_map_or_table or
  create_barplot_or_table is used
- [`filter_by_date()`](https://united4surveillance.github.io/signal-detection-tool/reference/filter_by_date.md)
  : Filter Data Frame by Date Range
- [`filter_data_last_n_weeks()`](https://united4surveillance.github.io/signal-detection-tool/reference/filter_data_last_n_weeks.md)
  : Filter the data so that only the data of the last n weeks are
  returned This function can be used to filter for those last n weeks
  where signals were generated.
- [`format_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/format_table.md)
  : Format the signal results to an interactive or static table
- [`get_case_id_duplicates()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_case_id_duplicates.md)
  : Checking for duplicates in case_id
- [`get_data_config_value()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_data_config_value.md)
  : Retrieve a Configuration Value from DATA_CONFIG
- [`get_database_connection()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_database_connection.md)
  : Establish a Database Connection (Example Implementation)
- [`get_empty_columns()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_empty_columns.md)
  : Retrieveing which columns in the dataset only contain missing values
- [`get_float_columns()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_float_columns.md)
  : Get the numeric columns that are not integer columns
- [`get_intervention_timepoint()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_intervention_timepoint.md)
  : Get row number of aggregated data which is the isoweek and isoyear
  corresponding to the date given
- [`get_iso_week_year()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_iso_week_year.md)
  : Get isoweek and isoyear from a given date
- [`get_min_max_date()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_min_max_date.md)
  : Get minimum and maximum date in the linelist
- [`get_missing_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_missing_data.md)
  : retrieve 'case_id's which have missing values in computationally
  crucial variables
- [`get_name_by_value()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_name_by_value.md)
  : Function to retrieve name from named vector given its value Can be
  used to retrieve the "pretty" names to show to the user and in the
  background work with the values
- [`get_possible_methods()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_possible_methods.md)
  : Determine Possible Outbreak Detection Methods Based on Available
  Historic Data
- [`get_region_from_region_id()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_region_from_region_id.md)
  : Function to extract corresponding region to the region_id variable
- [`get_region_id_from_region()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_region_id_from_region.md)
  : Function to get the region_id variable names from the region
  variables
- [`get_shp_config_or_internal()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_shp_config_or_internal.md)
  : Get Shapefile: Read from Config or Use Internal Dataset
- [`get_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals.md)
  : Get Signals
- [`get_signals_aeddo()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_aeddo.md)
  : Automated and Early Detection of Disease Outbreaks
- [`get_signals_all()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_all.md)
  : Get Signals for All Strata Including Unstratified
- [`get_signals_cusum()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_cusum.md)
  : Get signals of CUSUM algorithm with reset
- [`get_signals_ears()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_ears.md)
  : Get signals of surveillance's EARS algorithm
- [`get_signals_farringtonflexible()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_farringtonflexible.md)
  : Get signals of surveillance's farringtonFlexible algorithm
- [`get_signals_glm()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_glm.md)
  : Get signals based on a weigthed GLM quasipoisson regression model
  for the expected case counts The GLM is flexible being able to just
  fit a mean, add a time trend, fit a harmonic sin/cos model or the
  seasons from the farringtonflexible.
- [`get_signals_stratified()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_signals_stratified.md)
  : Get Signals Stratified
- [`get_strata_from_signals_agg()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_strata_from_signals_agg.md)
  : Extract strata from precomputed signals_agg
- [`get_unused_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_unused_variables.md)
  : retrieve variables which are in provided in surveillance linelist
  but are not used in the tool
- [`get_valid_dates_intervention_start()`](https://united4surveillance.github.io/signal-detection-tool/reference/get_valid_dates_intervention_start.md)
  : Get a default and minimum and maximum date for the intervention time
  point for the glm algorithms with pandemic correction. This is based
  on the data provided and the settings for the delays.
- [`input_example`](https://united4surveillance.github.io/signal-detection-tool/reference/input_example.md)
  : Example input data
- [`input_example_multipathogen`](https://united4surveillance.github.io/signal-detection-tool/reference/input_example_multipathogen.md)
  : Multipathogen input data
- [`input_metadata`](https://united4surveillance.github.io/signal-detection-tool/reference/input_metadata.md)
  : Input metadata
- [`is_ISO8601()`](https://united4surveillance.github.io/signal-detection-tool/reference/is_ISO8601.md)
  : checking YYYYY-mm-dd format of date variables
- [`is_ISO8601_detailed()`](https://united4surveillance.github.io/signal-detection-tool/reference/is_ISO8601_detailed.md)
  : detailed check of date variables check that months are numbers
  between 01-12 and days are from 01-31
- [`is_age_group_format()`](https://united4surveillance.github.io/signal-detection-tool/reference/is_age_group_format.md)
  : checking age_group column only containing digits, separators and
  \<,\>,+ For the first age_group using seperator, i.e. 00-05, \<5 is
  allowed. For the last age group using seperators, i.e. 95-100, \>100
  and 100+ is allowed. Separators like - (dash), \_ (underscore), and —
  (em dash) are also allowed for intermediate age ranges.
- [`is_last_age_group_format()`](https://united4surveillance.github.io/signal-detection-tool/reference/is_last_age_group_format.md)
  : checking the format of the last/biggest age group to follow the
  format digit separator digit, digit+ or \>digit
- [`isoweek_to_date()`](https://united4surveillance.github.io/signal-detection-tool/reference/isoweek_to_date.md)
  : Create a Date from ISO Year and Week
- [`load_data_db()`](https://united4surveillance.github.io/signal-detection-tool/reference/load_data_db.md)
  : Load Data from Database (Example Implementation)
- [`nuts_shp`](https://united4surveillance.github.io/signal-detection-tool/reference/nuts_shp.md)
  : NUTS-Regions
- [`pad_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/pad_signals.md)
  : Extend the computed threshold and expectation of the signal
  detection method to the past for visualisation purposes but not for
  signal generation Inside the function it is computed what the maximum
  number of timepoints is the signal detection algorithms can be applied
  for. This depends on the algorithm and the amount of historic data.
  The already generated signals dataframe is then extended with the
  expectation and threshold into the past
- [`plot_agegroup_by()`](https://united4surveillance.github.io/signal-detection-tool/reference/plot_agegroup_by.md)
  : Plot age-groups grouped by another variable
- [`plot_barchart()`](https://united4surveillance.github.io/signal-detection-tool/reference/plot_barchart.md)
  : Barplot visualising the number of cases and information about any
  signals
- [`plot_regional()`](https://united4surveillance.github.io/signal-detection-tool/reference/plot_regional.md)
  : Plot number of cases with number of signals by region
- [`plot_signals_per_week()`](https://united4surveillance.github.io/signal-detection-tool/reference/plot_signals_per_week.md)
  : Plot in how many strata an signal was detected under the detection
  period
- [`plot_time_series()`](https://united4surveillance.github.io/signal-detection-tool/reference/plot_time_series.md)
  : Plot time-series based on the results of a signal detection
  algorithm, being alarms, threshold and expectation
- [`prepare_signals_agg_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/prepare_signals_agg_table.md)
  : Prepares aggregated signals of one category for producing a table.
- [`prepare_signals_table()`](https://united4surveillance.github.io/signal-detection-tool/reference/prepare_signals_table.md)
  : Prepare the signal detection results for creation of table with
  results
- [`preprocess_data()`](https://united4surveillance.github.io/signal-detection-tool/reference/preprocess_data.md)
  : Preprocessing of linelist surveillance data with or without
  outbreak_ids
- [`query_database()`](https://united4surveillance.github.io/signal-detection-tool/reference/query_database.md)
  : Query Database (Example Implementation)
- [`read_csv_both_sep()`](https://united4surveillance.github.io/signal-detection-tool/reference/read_csv_both_sep.md)
  : Read csv files which can have seperators ; or ,
- [`read_csv_or_excel()`](https://united4surveillance.github.io/signal-detection-tool/reference/read_csv_or_excel.md)
  : Read csv or excel files Checks the input file for its type and then
  reads the file
- [`region_id_variable_names()`](https://united4surveillance.github.io/signal-detection-tool/reference/region_id_variable_names.md)
  : Variable names of the region_id variables including those which do
  not necessarily follow NUTS format
- [`region_variable_names()`](https://united4surveillance.github.io/signal-detection-tool/reference/region_variable_names.md)
  : Variable names of the region_id variables including those which do
  not necessarily follow NUTS format
- [`remove_empty_columns()`](https://united4surveillance.github.io/signal-detection-tool/reference/remove_empty_columns.md)
  : Removing columns from data which only contain missing values
- [`run_app()`](https://united4surveillance.github.io/signal-detection-tool/reference/run_app.md)
  : Run the Shiny Application
- [`run_report()`](https://united4surveillance.github.io/signal-detection-tool/reference/run_report.md)
  : Renders signal detection report
- [`save_signals()`](https://united4surveillance.github.io/signal-detection-tool/reference/save_signals.md)
  : Save signals
- [`sex_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/sex_levels.md)
  : Allowed levels for sex in preprocessed surveillance data used for
  all calculations
- [`sex_raw_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/sex_raw_levels.md)
  : Allowed levels for sex in raw surveillance data
- [`transform_required_format()`](https://united4surveillance.github.io/signal-detection-tool/reference/transform_required_format.md)
  : Transform Data to Required Format (Example Implementation)
- [`yes_no_unknown_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/yes_no_unknown_levels.md)
  : Allowed levels for variables with yes, no, unknown levels in
  preprocessed surveillance data used for calculations
- [`yes_no_unknown_raw_levels()`](https://united4surveillance.github.io/signal-detection-tool/reference/yes_no_unknown_raw_levels.md)
  : Allowed levels for variables with yes, no, unknown levels in raw
  surveillance data
- [`yes_no_unknown_variables()`](https://united4surveillance.github.io/signal-detection-tool/reference/yes_no_unknown_variables.md)
  : Variable names of the variables which have yes, no , unknown levels
