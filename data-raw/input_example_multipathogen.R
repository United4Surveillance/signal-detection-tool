## code to prepare `input_example_multipathogen` dataset goes here
input_example_multipathogen <- SignalDetectionTool::input_example

# Changing manually the pathogen of some linelist entries
input_example$pathogen[3000:3999] <- "Enterobacter"
input_example$pathogen[4000:4999] <- "Salmonella"

usethis::use_data(input_example_multipathogen, overwrite = TRUE)
