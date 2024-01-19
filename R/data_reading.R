#' Read csv files which can have seperators ; or ,
#' @param filepath character, filepath
#' @returns data which was read ins
read_csv_both_sep <- function(filepath) {
  header <- readLines(filepath, n = 1)
  if (grepl(";", header)) {
    data <- read.csv(filepath, header = TRUE, sep = ";", encoding = "UTF-8", stringsAsFactors = F)
  } else {
    data <- read.csv(filepath, header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = F)
  }

  data
}

#' Read csv or excel files
#' Checks the input file for its type and then reads the file
#' @param filename character, filename
#' @param filepath character, filepath
#' @returns data.frame, loaded dataset provided by the user which is a linelist of surveillance data
read_csv_or_excel <- function(filename, filepath) {
  filetype <- tools::file_ext(filename)

  if (filetype == "csv") {
    data <- read_csv_both_sep(filepath)
  } else if (filetype == "xlsx" | filetype == "xls") {
    data <- readxl::read_excel(filepath, guess_max = 5000)
    data <- data.frame(data)
  }
  data
}
