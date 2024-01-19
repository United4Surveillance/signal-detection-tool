# This file should include all relevant helper functions required to run the actual signal detection tool
# Feel free to complete the file

#' Finds correct age interval for given age
#' @param age integer age in years
#' @param x vector of age group break points
#'
#' @examples
#' \dontrun{
#' find_age_group(5, c(0, 5, 10, 99)) # would result in "05-09"
#' find_age_group(12, c(0, 5, 15, 99)) # would result in "05-14"
#' }
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

#' Age Group Format Check and Adjustment
#'
#' This function checks whether the 'age_group' column in the given data frame
#' follows the format "xx-xx" and adjusts it if necessary.
#' If the format is not as desired, it modifies the entries to comply with the specified format.
#'
#' @param df A data frame containing an 'age_group' column.
#'
#' @return A modified data frame with the 'age_group' column adjusted for the desired format.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age_group = c("01-05", "6-10", "11-15", "16-20"))
#' adjusted_data <- agegrp_format_check(data_frame)
#' print(adjusted_data)
#' }
#'
#' @importFrom stringr str_starts str_split_fixed
agegrp_format_check <- function(df) {
  # checking whether format is as wanted as xx-xx
  # if not okay, change to comply with format
  # regex checking if there are any with 2 or more digits, followed by a hyphen or a '+'
  if (any(stringr::str_starts(df$age_group, "[:digit:]{2,}[\\-\\+]") == FALSE)) {
    splits <- stringr::str_split_fixed(as.character(df$age_group),"[\\+\\-]", 2)

    for (i in 1:length(df$age_group)) {
      # adding leading zeros on <10 ages
      if (as.numeric(splits[i,1]) < 10 & as.numeric(splits[i,2]) < 10) {
        df$age_group[i] <- paste0(sprintf("%02d",as.numeric(splits[i,1])),
                                  "-",
                                  sprintf("%02d",as.numeric(splits[i,2]))
        )
      }
    }
  }

  return(df)
}

#' Complete Age Group List
#'
#' This function takes a data frame containing an 'age_group' column,
#' finds a non-NULL example age group, and generates a complete list of age
#' groups based on the gap between the lower and upper bounds.
#' It is particularly useful for completing missing age groups in datasets.
#'
#' @param df A data frame containing an 'age_group' column.
#'
#' @return A character vector representing the complete age group array.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age_group = c("01-05", "06-10", "11-15", "16-20"))
#' complete_list <- complete_agegrp_list(data_frame)
#' print(complete_list)
#' }
#'
#' @importFrom stringr str_split_1
#' @importFrom plyr round_any
complete_agegrp_list <- function(df) {
  # setting binary variable to false
  symbols_bin <- FALSE

  # find an example of an age group that is not NULL
  tmp_agegrp <- df$age_group[!is.null(df$age_group) & !grepl("[\\+]",df$age_group)][1]

  # get the lower and upper of the age group gap and find the range
  lower_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp),"-")[1])
  upper_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp),"-")[2])
  split_range <- plyr::round_any(upper_split - lower_split, accuracy = 5)

  # find the maximum age in relation to the age group gap
  max_data_rounded <- plyr::round_any(x = max(df$age),
                                      accuracy = split_range,
                                      f = ceiling)

  # make sequences
  seq_lower <- seq(0,max_data_rounded,split_range)
  seq_upper <- (seq_lower-1)[-1]

  # ensuring whether + has been used, e.g. 80-89, 90+
  if (any(grepl("[\\+]", df$age_group)==TRUE)) {
    # set binary variable that symbol is in use
    symbols_bin <- TRUE

    # find the maximum agegroup
    splitted_agegroup <- stringr::str_split_fixed(string = unique(df$age_group),
                                                  pattern = "[\\+\\-]",
                                                  n = 2)
    max_number <- as.numeric(splitted_agegroup[which(splitted_agegroup[,2] == "")][1])
  }

  # combine to create the complete age group array
  # adding leading zeros on <10 ages
  all_agegrps <- paste0(sprintf("%02d",seq_lower[-length(seq_lower)]),
                        "-",
                        sprintf("%02d",seq_upper))

  if (symbols_bin) {
    # locate max_number in agegrps and replace it with "max_number+"
    all_agegrps[grep(x = all_agegrps, pattern = max_number)] <- paste0(max_number,"+")

    # remove any items after +-symbol
    all_agegrps <- all_agegrps[1:grep(x = all_agegrps, pattern = "[\\+]")]
  }

  return(all_agegrps)
}

#' Creates age grouping factorized variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
#'
#' @export
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input_sample.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data$age <- sample(1:125, 10, replace = TRUE)
#' age_groups(data) # default age groups
#' age_groups(data, c(15L, 35L, 65L, 100L)) # custom age groups
#' }
age_groups <- function(df, break_at = NULL) {
  # check whether age_groups already exist
  if (!("age_group" %in% colnames(df))) {
    # if age_group doesn't exist, create it from age
    # error checking ---------------------------------------------------------

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

    # setting up age groups ----------------------------------------------------

    default_break_at <- seq(5, 125, 5)

    if (is.null(break_at)) { # use default age groups
      set <- c(0, default_break_at) # helper vector
    } else { # use custom age groups
      set <- c(0, break_at)
    }

    # assigning age group  -----------------------------------------------------

    for (i in 1:nrow(df)) { # assign age group to every age in data frame

      if (is.integer(df$age) != TRUE) { # check for integer
        stop("Type of age is not integer")
      }
      df$age_group[i] <- find_age_group(df$age[i], set)
    }

    # move age_group to correct position
    df <- df %>% dplyr::relocate(age_group, .after = age)
  }

  # # ensure age_group format with xx-xx format demand
  df <- agegrp_format_check(df)

  # get the complete list of agegroups
  all_agegroups <- complete_agegrp_list(df)

  # converting age_group to factor ---------------------------------------------
  df$age_group <- factor(df$age_group,
                         levels = stringr::str_sort(all_agegroups, numeric = TRUE)
  )

  return(df)
}



#' Get Signals Stratified
#'
#' This function stratifies and aggregates surveillance data by specified columns and analyzes
#' each stratum separately using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param fun The signal detection function to apply to each stratum.
#' @param stratification_columns A character vector specifying the columns to
#'   stratify the data by.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for.
#' @return A tibble containing the results of the signal detection analysis
#'   stratified by the specified columns.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("../data/input/input.csv")
#' categories <- c("county", "sex", "age_group") # Replace with actual column names
#' results <- get_signals_stratified(
#'   data,
#'   fun = get_signals_farringtonflexible,
#'   stratification_columns = categories
#' )
#' print(results)
#' }
get_signals_stratified <- function(data,
                                   fun,
                                   stratification_columns,
                                   date_start = NULL,
                                   date_end = NULL,
                                   date_var = "date_report",
                                   number_of_weeks = 52) {
  # check that all columns are present in the data
  for (col in stratification_columns) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  # Initialize an empty list to store results per category
  category_results <- list()

  # get min and max date of the whole dataset before stratification
  # stratified aggregated data can be filled up with 0s until min and max date
  # of the full dataset
  if(is.null(date_start)){
    date_start <- min(data[[date_var]], na.rm = TRUE)
  }
  if(is.null(date_end)){
    date_end <- max(data[[date_var]], na.rm = TRUE)
  }

  # Loop through each category
  for (category in stratification_columns) {

    if(is.factor(data[, category])){
      strata <- levels(data[, category])
    }
    else{
      strata <- unique(data[, category])  # character is supported as well
    }

    # iterate over all strata and run algorithm
    for (stratum in strata) {
      sub_data <- data %>% dplyr::filter(.data[[category]] == stratum)

      # aggregate data
      sub_data_agg <- sub_data %>%
        aggregate_data(date_var = date_var) %>%
        add_rows_missing_dates(date_start, date_end)


      # run selected algorithm
      results <- fun(sub_data_agg, number_of_weeks)

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

#' Get Signals
#'
#' This function analyzes surveillance data to detect signals using the
#' specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param method The method to use for signal detection (currently supports
#'   "farrington").
#' @param stratification A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for.
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read.csv("data/input/input.csv")
#' results <- get_signals(data,
#'   method = "farrington",
#'   stratification = c("county", "sex")
#' )
#' }
get_signals <- function(data,
                        method = "farrington",
                        stratification = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        date_var = "date_report",
                        number_of_weeks = 52) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = c("farrington","aeddo","ears","cusum")))

  checkmate::assert(
    checkmate::check_null(stratification),
    checkmate::check_vector(stratification),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  if (method == "farrington") {
    fun <- get_signals_farringtonflexible
  } else if (method == "aeddo") {
    fun <- get_signals_aeddo
  } else if (method == "ears"){
    fun <- get_signals_ears
  } else if (method == "cusum"){
    fun <- get_signals_cusum
  }

  if (is.null(stratification)) {
    # preprocess and aggregated data
    data_agg <- data %>%
      aggregate_data(date_var = date_var) %>%
      add_rows_missing_dates(date_start, date_end)

    results <- fun(data_agg, number_of_weeks)

    if(!is.null(results)){
      results %>%
        dplyr::mutate(category = NA, stratum = NA) %>%
        dplyr::mutate(method = method)
    }
  } else {
    results <- get_signals_stratified(
      data,
      fun,
      stratification,
      date_start,
      date_end,
      date_var,
      number_of_weeks
    )
  }


  return(results)
}

#' Save signals
#'
#' Save the processed results to a CSV file and return codes and messages for application use.
#'
#' @param signals The processed results data frame.
#' @param original_input_data The original input data used for analysis.
#' @param filepath The optional filepath to save the results. If not provided, a filename is generated.
#'
#' @return A list containing success status (TRUE or FALSE) and a message (NULL for success, a warning, or an error).
#'
#' @examples
#'
#' # Save signals with default or custom filepath
#' # signals <- SignalDetectionTool::get_signals(SignalDetectionTool::input_example)
#' signals <- SignalDetectionTool::get_signals(SignalDetectionTool::input_example,
#'   stratification = c("county", "sex", "age_group")
#' )
#' save_signals(signals, SignalDetectionTool::input_example)
#'
#' @export
save_signals <- function(signals, original_input_data, filepath = "") {
  # get last day of week
  # renaming and reorganization of output is kept here for future use, once we change signal output
  to_save <- signals %>%
    dplyr::mutate(date = lubridate::ceiling_date(lubridate::ymd(paste0(signals$year, "-01-01")) + lubridate::weeks(signals$week), "week") - 1) %>%
    dplyr::filter(!is.na(alarms)) # %>%
  #   dplyr::rename(
  #     outbreak_status = "alarms",
  #     count_cases = "cases"
  #   ) %>%
  #   dplyr::mutate(expected = ifelse(is.na(expected), 0, expected)) %>%
  #   dplyr::mutate(count_outbreak_cases = outbreak_status * (count_cases - floor(upperbound))) # %>%
  #
  # # list of columns to reconstruct
  # reconstruct_columns <- SignalDetectionTool::input_metadata[SignalDetectionTool::input_metadata$Mandatory == "YES", "Variable"]
  # # remove columns that are not applicable for aggregated data
  # reconstruct_columns <- reconstruct_columns[-which(reconstruct_columns %in% c("case_id", "date_report"))]
  #
  # # if stratification was applied, get the stratification column and append
  # if ("stratum" %in% colnames(to_save)) {
  #   strat_columns <- unique(signals$category)
  #   reconstruct_columns <- union(reconstruct_columns, strat_columns)
  # }
  # # TODO might want to add column `<column>_id` if available
  #
  #
  # # retrieve original value if all identical, set to "all" otherwise
  # for (col in reconstruct_columns) {
  #   to_save[col] <- original_input_data[1, col] # set initial value
  #   if (length(unique(original_input_data[col])[[1]]) > 1) {
  #     to_save <- to_save %>% dplyr::mutate({{ col }} := "all")
  #   }
  # }
  #
  # # iterate over categories, write values into appropriate column
  # for (cat in unique(to_save[["category"]])) {
  #   rows_stratified <- !is.na(to_save[["category"]]) & to_save["category"] == cat
  #   to_save[rows_stratified, cat] <- as.character(to_save[rows_stratified, "stratum"])
  # }
  # # drop some output columns
  # to_save <- to_save %>% dplyr::select(-upperbound, -expected)
  #
  # # check if analysis was stratified, if so drop respective columns
  # if ("stratum" %in% colnames(to_save)) {
  #   to_save <- to_save %>% dplyr::select(-category, -stratum)
  # }

  if (filepath == "") {
    filepath <- conjure_filename(to_save)
  }

  # actually save signals to file an return codes, messages for app to utilize
  tryCatch(
    {
      write.csv(to_save, filepath, row.names = FALSE)

      # Return TRUE and NULL if there are no warnings or errors
      return(list(success = TRUE, message = NULL))
    },
    warning = function(warning_message) {
      # Return TRUE and the warning message if there are warnings
      return(list(success = TRUE, message = warning_message))
    },
    error = function(error_message) {
      # Return FALSE and the error message if there are errors
      return(list(success = FALSE, message = error_message))
    }
  )
}


#' Conjure Filename
#'
#' Generate an output filename based on the results from get_signals.
#'
#' @param data The data frame containing information for filename generation.
#'
#' @return The generated filename with the directory path, i.e. signals_farrington_Austria_Pertussis_2022-01-01_2023-07-08_.csv
#'
#' @examples
#' # Generate a filename based on results data
#' conjure_filename(SignalDetectionTool::get_signals(SignalDetectionTool::input_example))
#'
#' @export
conjure_filename <- function(data) {
  directory <- "outputs"

  # resulting filename will be
  # signals_method_country_pathogen_date-start_date-end.csv
  filename <- paste("signals", data[1, "method"], data[1, "country"],
                    data[1, "pathogen"],
                    min(data$date), max(data$date), ".csv",
                    sep = "_"
  )

  return(paste(directory, filename, sep = "/"))
}
