# This file should include the main R script of the package
# All other necessary code files, data etc. should be sourced in this script
# Feel free to complete the file

library(surveillance)
library(tidyverse)

# Sources to scripts and data could be replaced by devtools::load_all() but then this main script cannot be located in the R directory
source("R/tool_functions.R")

# load example data
input_path <- "data/input/input.csv"
data <- read.csv(input_path, header = TRUE, sep = ",")

sts_cases <- convert_to_sts(data)

# run Farrington Flexible on data
results <- surveillance::farringtonFlexible(sts_cases)

print(results)
