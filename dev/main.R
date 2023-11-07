# This file should include the main R script of the package
# All other necessary code files, data etc. should be sourced in this script
# Feel free to complete the file

library(surveillance)
library(tidyverse)
library(ggiraph)
library(gt)
library(checkmate)

# Sources to scripts and data could be replaced by devtools::load_all() but then this main script cannot be located in the R directory
devtools::load_all()

# preprocess
data <- input_example %>%
  dplyr::mutate(date_onset = ifelse(is.na(date_onset) | date_onset == "", date_report, date_onset))
data$age_group <- factor(data$age_group, levels = stringr::str_sort(unique(data$age_group), numeric = TRUE))
data$sex <- factor(data$sex)

# check data
check_input_data(data)

# plot timeseries
results <- get_signals_farringtonflexible(data)
plot_time_series(results, interactive = T)
plot_time_series(results, number_of_weeks = 10)

# plot age group
plot_agegroup_by(data)
ggiraph::girafe(ggobj = plot_agegroup_by(data))

# run signal detection on data
# results <- get_signals(data)  # no stratification
results <- get_signals(data, stratification = c("county", "sex", "age_group"))

create_results_table(results, interactive = FALSE)
create_results_table(results, interactive = TRUE)

# run signal detection stratified by region
country <- unique(data$country_id)
has_county <- length(unique(data$county_id)) > 1
has_community <- length(unique(data$community_id)) > 1

if (has_county) {
  signals_county <- get_signals(data, stratification = c("county_id"))
}
if (has_community) {
  signals_community <- get_signals(data, stratification = c("community_id"))
}
# plot surveillance data and signal detection results as a map

if (has_county) {
  plot_regional(data, signals_county, nuts_shp, country_id = country, regional_level = "county")
}
if (has_community) {
  plot_regional(data, signals_community, nuts_shp, country_id = country, regional_level = "community", interactive = TRUE)
}

