read_csv_both_sep <- function(filepath){

  header <- readLines(filepath, n=1)
  if(grepl(";",header)){
    data <- read.csv(filepath, header = TRUE, sep = ";", encoding = "UTF-8", stringsAsFactors = F)
  } else {
    data <- read.csv(filepath, header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = F)
  }

  data
}

read_csv_or_excel <- function(filename, filepath){

  filetype <- tools::file_ext(filename)

  if(filetype == "csv"){
    data <- read_csv_both_sep(filepath)
  }
  else if(filetype == "xlsx"| filetype == "xls"){
    data <- readxl::read_excel(filepath, guess_max = 5000)
  }
  data

}
