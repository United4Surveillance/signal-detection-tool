## code to prepare `input_example` dataset goes here
input_example <- read.csv("dev/data/input/input.csv") %>%
  dplyr::mutate_if(is.character, function(x) iconv(x, to = "UTF-8"))
usethis::use_data(input_example, overwrite = TRUE)
