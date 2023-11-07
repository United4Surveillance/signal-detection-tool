## code to prepare `input_metadata` dataset goes here
input_metadata <- read.delim("dev/data/input/description.md", sep = "|") %>%
  dplyr::select("Variable", "Mandatory", "Description") %>%
  dplyr::mutate_all(trimws) %>%
  dplyr::filter(!grepl("-{1,}", Variable))

usethis::use_data(input_metadata)

