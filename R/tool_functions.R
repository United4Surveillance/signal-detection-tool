# This file should include all relevant helper functions required to run the actual signal detection tool
# Feel free to complete the file

#' Finds correct age interval for given age
#' @param age integer age in years
#' @param x vector of age group break points
#'
#' @examples
#' find_age_group(5, c(0, 5, 10, 99))  # would result in "05-09"
#' find_age_group(12, c(0, 5, 15, 99)) # would result in "05-14"
find_age_group <- function(age, x) {
  intervals <- length(x) # number of age groups

  for (i in 1:intervals) { # finding interval in which age lies
    if (i == intervals) { # check if last age group
      group <- paste0(x[i], "+")
      return(group)
    }
    if ((x[i] <= age) & (age < x[i + 1])) {
      if (age < 10 | x[i] < 10) { # zero padding
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

#' Creates age grouping variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
#'
#' @examples
#' input_path <- "data/input/input_sample.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data$age <- sample(1:125, 10, replace = TRUE)
#' age_groups(data) # default age groups
#' age_groups(data, c(15L,35L,65L,100L)) # custom age groups
age_groups <- function(df, break_at = NULL) {
  # error checking ----------------------------------------------------------

  if (!is.null(break_at)) { # check for non integer values
    if (!(is.integer(break_at))) {
      stop("Input of integer type is only allowed")
    }

    var <- length(break_at) # helper vector
    for (i in 1:(var - 1)) { # check if break points are ordered
      if (break_at[i + 1] < break_at[i]) {
        stop("Invalid break points")
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

  # assigning age group  ----------------------------------------------------

  for (i in 1:nrow(df)) { # assign age group to every age in data frame

    if (is.integer(df$age) != TRUE) { # check for integer
      stop("Type of age is not integer")
    }
    df$age_group[i] <- find_age_group(df$age[i], set)
  }

  # converting age_group to factor ------------------------------------------

  # extract the lower values of age groups
  lower_values <- gsub("-.*", "", df$age_group)
  lower_values <- as.numeric(lower_values)

  # order the age groups based on the lower values
  ordered_groups <- df$age_group[order(lower_values)]

  df$age_group <- factor(df$age_group, levels = unique(ordered_groups))
  return(df)
}

#' Get Signals Stratified
#'
#' This function stratifies surveillance data by specified columns and analyzes each stratum separately using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param fun The signal detection function to apply to each stratum.
#' @param stratification_columns A character vector specifying the columns to stratify the data by.
#'
#' @return A tibble containing the results of the signal detection analysis stratified by the specified columns.
#' @export
#'
#' @examples
#' data <- read.csv("../data/input/input.csv")
#' categories <- c("county", "sex", "age_group") # Replace with actual column names
#' results <- get_signals_stratified(data, fun = get_signals_farringtonflexible, stratification_columns = categories)
#' print(results)
get_signals_stratified <- function(data, fun, stratification_columns) {
  # check that all columns are present in the data
  for (col in stratification_columns) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }
  start_date <- min(data$date_onset)
  end_date <- max(data$date_onset)

  # Initialize an empty list to store results per category
  category_results <- list()

  # Loop through each category
  for (category in stratification_columns) {
    # Group the data by the current category
    grouped_data <- data %>%
      dplyr::group_by_at(category) %>%
      dplyr::group_data()

    # iterate over all strata and
    for (i in 1:nrow(grouped_data)) {
      stratum <- grouped_data[i, category][[1]]
      sub_data <- data %>%
        dplyr::slice(grouped_data[i, ".rows"][[1]][[1]])


      # run selected algorithm here specifying start and end dates
      results <- fun(sub_data, start_date, end_date)

      if (is.null(results)) {
        warning(paste0(
          "The stratum ", category, ":", stratum,
          " will be neglected due to lack of data."
        ))
      } else {
        # add information on stratification to results
        results <- results %>% dplyr::mutate(
          category = category, stratum = stratum
        )
      }
      # Store the results in the list
      category_results[[stratum]] <- results
    }
  }

  return(dplyr::bind_rows(category_results))
}
