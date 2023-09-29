# This file should include the main R script of the package
# All other necessary code files, data etc. should be sourced in this script
# Feel free to complete the file

library(surveillance)
library(tidyverse)

# Sources to scripts and data could be replaced by devtools::load_all() but then this main script cannot be located in the R directory
source("R/tool_functions.R")
source("R/farrington_flexible.R")
source("R/results_table.R")

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
data <- read.csv(input_path, header = TRUE, sep = ",", encoding = "UTF-8")


# run signal detection on data
# results <- get_signals(data)  # no stratification
results <- get_signals(data, stratification = c("county", "sex", "age_group"))

tabl <- create_results_table(results, interactive = FALSE)
tabl
