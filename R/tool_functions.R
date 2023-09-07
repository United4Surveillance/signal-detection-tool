# This file should include all relevant helper functions required to run the actual signal detection tool
# Feel free to complete the file

#' Creates age grouping variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
#' 
#' @example 
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data$age <- sample(1:125, 10, replace = TRUE)
#' age_groups(data) # default age groups
#' age_groups(data, c(15L,35L,65L,100L)) # custom age groups
age_groups <- function(df, break_at = NULL) {
  # error checking ----------------------------------------------------------
  
  if (!is.null(break_at)) { # check for non integer values
    if (!(is.integer(break_at))) {
      return("Input of integer type is only allowed")
    }
    
    var <- length(break_at) # helper vector
    for (i in 1:(var - 1)) { # check if break points are ordered
      if (break_at[i + 1] < break_at[i]) {
        return("Invalid break points")
      }
    }
  }
  
  # setting up age groups ---------------------------------------------------
  
  default_break_at <- seq(5, 125, 5)
  
  if (is.null(break_at)) { # use default age groups
    set <- c(0, default_break_at) # helper vector
  } else { # use custom age groups
    set <- c(0, break_at)
  }
  
  # function for categorizing age -------------------------------------------
  
  find_age_group <- function(age, x = set) { # define function for placing age in an age interval
    intervals <- length(x) # number of age groups
    
    for (i in 1:intervals) { # finding interval in which age lies
      if (i == intervals) { # check if last age group
        group <- paste0(x[i], "+")
        return(group)
      }
      if ((x[i] <= age) & (age < x[i + 1])) {
        if (age < 10) { # zero padding
          group <- paste(paste0(0, x[i]),
                         ifelse(x[i + 1] - 1 < 10,
                                paste0(0, x[i + 1] - 1),
                                x[i + 1] - 1
                         ),
                         sep = "-"
          )
          return(group)
        } else {
          group <- paste(x[i], x[i + 1] - 1, sep = "-")
          return(group)
        }
      }
    }
  }
  
  # assigning age group  ----------------------------------------------------
  
  for (i in 1:nrow(df)) { # assign age group to every age in data frame
    
    if (is.integer(df$age) != TRUE) { # check for integer
      return("Type of age is not integer")
    }
    df$age_group[i] <- find_age_group(df$age[i])
  }
  
  # converting age_group to factor ------------------------------------------
  
  # extract the lower values of age groups
  lower_values <- (gsub("-.*", "", df$age_group))
  
  # order the age groups based on the lower values
  ordered_groups <- df$age_group[order(lower_values)]
  
  df$age_group <- factor(df$age_group, levels = unique(ordered_groups))
  return(df)
}

#' Turn data into sts object
#' @param data data frame to be converged
#' 
#' @examples 
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' sts_cases <- convert_to_sts(data)
convert_to_sts <- function(data) {
  # Convert the 'date_onset' column to a date format
  data <- data %>%
    dplyr::mutate(date_onset = as.Date(date_onset)) %>%
    dplyr::arrange(date_onset)
  
  year_week <- surveillance::isoWeekYear(data$date_onset)
  
  # aggregate case counts per week
  case_counts <- data %>%
    dplyr::mutate(isoyear = year_week$ISOYear) %>%
    dplyr::mutate(isoweek = year_week$ISOWeek) %>%
    dplyr::group_by(isoyear, isoweek) %>%
    dplyr::summarize(cases = n())
  
  # create sts object
  return(surveillance::sts(case_counts$cases,
                           start = c(
                             case_counts$isoyear[1],
                             case_counts$isoweek[1]
                           ),
                           frequency = 52
  ))
}
