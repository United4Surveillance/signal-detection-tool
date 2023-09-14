library(surveillance)
library(tidyverse)
source("farrington_flexible.R")

# load example data
input_path <- "data/input/input.csv"
data <- read.csv(input_path, header = TRUE, sep = ",")

# run Farrington Flexible on data
results <- get_signals_farringtonflexible(data)

print(results)
