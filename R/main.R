# This file should include the main R script of the package
# All other necessary code files, data etc. should be sourced in this script
# Feel free to complete the file

library(surveillance)
library(tidyverse)
library(ggiraph)
library(gt)
library(checkmate)

# Sources to scripts and data could be replaced by devtools::load_all() but then this main script cannot be located in the R directory
source("R/tool_functions.R")
source("R/farrington_flexible.R")
source("R/results_table.R")
source("R/plot_regional.R")
source("R/plot_time_series.R")
source("R/plot_agegroup_by.R")

#' Get Signals
#'
#' This function analyzes surveillance data to detect signals using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param method The method to use for signal detection (currently supports "farrington").
#' @param stratification A character vector specifying the columns to stratify the analysis. Default is NULL.
#'
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' data <- read.csv("../data/input/input.csv")
#' # results <- get_signals(data)
#' results <- get_signals(data, method = "farrington", stratification = c("county", "sex"))
get_signals <- function(data, method = "farrington", stratification = NULL) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = c("farrington"))
  )
  checkmate::assert(
    checkmate::check_null(stratification),
    checkmate::check_vector(stratification),
    combine = "or"
  )

  if (method == "farrington") {
    fun <- get_signals_farringtonflexible
  }

  if (is.null(stratification)) {
    results <- fun(data) %>% dplyr::mutate(category = NA, stratum = NA)
  } else {
    results <- get_signals_stratified(data, fun, stratification)
  }

  return(results)
}


# load example data
input_path <- "data/input/input.csv"
header <- readLines(input_path, n=1)
if(grepl(";",header)){
  data <- read.csv(input_path, header = TRUE, sep = ";", encoding = "UTF-8")
} else {
  data <- read.csv(input_path, header = TRUE, sep = ",", encoding = "UTF-8")
}
shape <- sf::st_read("data/shp/NUTS_RG_03M_2021_3035.shp")

# preprocess
data <- data %>% mutate(date_onset = ifelse(is.na(date_onset) | date_onset=="", date_report, date_onset))
data$age_group <- factor(data$age_group)
data$sex <- factor(data$sex)

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
has_county <- length(unique(data$county_id))>1
has_community <- length(unique(data$community_id))>1

if(has_county)
  signals_county <- get_signals(data, stratification = c("county_id"))

if(has_community)
  signals_community <- get_signals(data, stratification = c("community_id"))

# plot surveillance data and signal detection results as a map

if(has_county)
  plot_regional(data, signals_county, shape, country_id = country, regional_level = "county")
if(has_community)
  plot_regional(data, signals_community, shape, country_id = country, regional_level = "community", interactive = TRUE)
