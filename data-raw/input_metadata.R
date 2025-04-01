## code to prepare `input_metadata` dataset goes here
input_metadata <- read.delim("dev/data/input/description.md", header = TRUE, sep = "|") %>%
  dplyr::select("Variable", "Mandatory", "Type", "Description") %>%
  dplyr::mutate_all(trimws) %>%
  dplyr::filter(!grepl("-{1,}", Variable))

usethis::use_data(input_metadata, overwrite = TRUE)
