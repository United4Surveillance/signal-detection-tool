#' Checking whether raw surveillance linelist fulfills requirements to the data specified in the SOP
#' Checking presence and correct type of mandatory variables
#' Checking type and values of optional variables
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_raw_surveillance_data <- function(data) {
  errors <- list()

  # check that reading in worked
  # if it did not work do not continue with other checks directly return
  if (nrow(data) == 0) {
    errors <- append(errors, "Loaded data is empty")

    return(errors)
  }

  # check that there are no empty lines
  if (check_empty_rows(data)) {
    errors <- append(errors, "Empty rows in the data")
  }

  # removing completely empty columns from before checking
  data <- remove_empty_columns(data)

  # check mandatory and optional variabless
  errors_mandatory <- check_mandatory_variables(data)
  errors_optional <- check_type_and_value_optional_variables(data)

  # checking consistency of all region and corresponding id variables provided
  regions_available <- intersect(colnames(data), region_variable_names())
  errors_consistency_region <- check_region_region_id_consistency(data, regions_available)

  errors <- c(errors, errors_mandatory, errors_optional, errors_consistency_region)

  # return potential error messages
  if (length(errors) != 0) {
    # remove empty slots
    errors <- errors[sapply(errors, function(element) !is.null(element))]
  }
  errors
}

#' checking mandatory variables in the surveillance data
#' check if mandatory variables are present in the data
#' check if they have the correct type and correct values
#' @param data data.frame, raw surveillance linelist
#' @returns list, empty or filled with errors
check_mandatory_variables <- function(data) {
  errors_presence <- check_presence_mandatory_variables(data)
  errors_types <- check_type_and_value_mandatory_variables(data)

  errors <- c(errors_presence, errors_types)

  errors
}

#' checking presence of mandatory variables in surveillance data
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_presence_mandatory_variables <- function(data) {
  errors <- list()

  # age is not mandatory as age or age_group can be given and thus
  # checked seperately
  mandatory_columns <- SignalDetectionTool::input_metadata %>%
    dplyr::filter(Mandatory == "YES") %>%
    dplyr::pull("Variable")

  data_columns <- colnames(data)
  missing_columns <- setdiff(mandatory_columns, data_columns)

  # check whether data contains one of age or agegroup
  if (check_any_age(data_columns)) {
    missing_columns <- c(missing_columns, "age or age_group")
  }

  if (length(missing_columns) != 0) {
    errors <- lapply(missing_columns, function(col) paste0("Missing or empty mandatory column '", col, "' in the data"))
  }

  errors
}

#' Checking those mandatory variables which are present in the data for their type
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_mandatory_variables <- function(data) {
  errors <- list()
  data_columns <- colnames(data)

  if ("case_id" %in% data_columns) {
    errors <- append(errors, check_type_and_value_case_id(data))
  }
  if ("date_report" %in% data_columns) {
    errors <- append(errors, check_type_and_value_date(data, "date_report"))
  }

  if ("age" %in% data_columns) {
    if (!checkmate::test_integerish(data$age)) {
      errors <- append(errors, "age is not an integer")
    }
  }
  if ("age_group" %in% data_columns) {
    errors <- append(errors, check_type_and_value_age_group(data))
  }

  if ("country" %in% data_columns) {
    if (!checkmate::test_character(data$country)) {
      errors <- append(errors, "country is not a character")
    }
  }
  if ("country_id" %in% data_columns) {
    if (!checkmate::qtest(data$case_id, c("s", "n"))) {
      errors <- append(errors, "country_id is not a character or a numeric")
    }
  }
  if ("pathogen" %in% data_columns) {
    if (!checkmate::test_character(data$pathogen)) {
      errors <- append(errors, "pathogen is not a character")
    }
  }
  errors
}

#' Checking correct type and value of optional variables which are present in the data
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_optional_variables <- function(data) {
  errors <- list()
  data_columns <- colnames(data)

  if ("sex" %in% data_columns) {
    if (!checkmate::test_character(data$sex)) {
      errors <- append(errors, "sex is not a character")
    } else {
      if (!check_character_levels(tolower(data$sex), sex_raw_levels())) {
        errors <- append(errors, paste0("Sex does not have the required levels ", paste(unlist(setdiff(sex_raw_levels(), c("", NA_character_))), collapse = ", ")))
      }
    }
  }

  # checks on the dates
  date_vars <- colnames(data %>% dplyr::select(dplyr::starts_with("date_")))
  date_vars <- setdiff(date_vars, "date_report")

  for (date_var in date_vars) {
    errors <- append(errors, check_type_and_value_date(data, date_var))
  }

  # checks on the _id variables
  id_vars <- colnames(data %>% dplyr::select(dplyr::ends_with("_id")))
  id_vars <- setdiff(id_vars, c("case_id", "country_id"))

  for (id_var in id_vars) {
    if (!checkmate::qtest(data[[id_var]], c("s", "n"))) {
      errors <- append(errors, paste0(id_var, " is not a character or a numeric"))
    }
  }

  # variables with yes,no,unknown values
  yes_no_unknown_vars <- intersect(data_columns, yes_no_unknown_variables())

  for (yes_no_unknown_var in yes_no_unknown_vars) {
    errors <- append(errors, check_type_and_value_yes_no_unknown(data, yes_no_unknown_var))
  }

  # check remaining character variables
  remaining_char_vars <- c(
    "state",
    "region_level1",
    "county",
    "region_level2",
    "community",
    "region_level3",
    "age_group",
    "occupation",
    "place_of_infection",
    "subtype"
  )
  remaining_char_vars <- intersect(data_columns, remaining_char_vars)

  for (remaining_char_var in remaining_char_vars) {
    if (!checkmate::test_character(data[[remaining_char_var]])) {
      errors <- append(errors, paste0(remaining_char_var, " is not a character"))
    }
  }

  errors
}

#' Checking type and values of date variables
#' date variable can be of type character or date
#' @param data data.frame, raw linelist of surveillance cases
#' @param date_var character, date variable to check
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_date <- function(data, date_var) {
  errors <- list()

  is_date <- lubridate::is.Date(data[[date_var]])
  is_character <- checkmate::test_character(data[[date_var]])

  if (!is_character & !is_date) {
    errors <- append(errors, paste0(date_var, " is not a character nor of type Date."))
  }
  if (is_character) {
    if (!is_ISO8601(data[[date_var]])) {
      errors <- append(errors, paste0(date_var, " is not in ISO 8601 format YYYY-MM-DD"))
    } else if (!is_ISO8601_detailed(data[[date_var]])) {
      errors <- append(errors, paste0(date_var, " has values for month outside 01-12 or for days outside 01-31"))
    }
  }
  errors
}


#' Checking type of case_id, duplication or missing case_id
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_case_id <- function(data) {
  errors <- list()

  # allow case_id to be character and numeric
  if (!checkmate::qtest(data$case_id, c("s", "n"))) {
    errors <- append(errors, "case_id is not a character or a numeric")
  }
  # check of values
  else {
    # any NA or empty strings
    if (any(is.na(data$case_id)) | any(data$case_id == "")) {
      errors <- append(errors, "Missing/empty case_ids inside data")
    }
    # check duplicates for case_id
    if (nrow(get_case_id_duplicates(data)) != 0) {
      errors <- append(errors, "Duplicate case_id in data")
    }
  }
  errors
}

#' Checking type and values of variables which should have yes,no,unknown values
#' @param data data.frame, raw linelist of surveillance cases
#' @param var character, variable to check
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_yes_no_unknown <- function(data, var) {
  errors <- list()

  if (!checkmate::test_character(data[[var]])) {
    errors <- append(errors, paste0(var, " is not a character"))
  } else {
    if (!check_character_levels(tolower(data[[var]]), yes_no_unknown_raw_levels())) {
      error_message <- paste0(var, " does not have the required levels ", paste(unlist(setdiff(yes_no_unknown_raw_levels(), c("", NA_character_))), collapse = ", "))
      errors <- append(errors, error_message)
    }
  }
  errors
}

#' Checking type and values of the age_group column
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list, empty when no errors occured or filled with error messages
check_type_and_value_age_group <- function(data) {
  errors <- list()

  if (!checkmate::test_character(data$age_group)) {
    errors <- append(errors, "Age group is not a character.")
  } else {
    # check whether age_group contains only digits and allowed separators
    if (!is_age_group_format(data$age_group)) {
      errors <- append(errors, "Age group does not follow the required format containing only digits and special characters: <, >, -, \u2014, _, +.")
    }
    # check that last age_group follows the format either using xxsepxx (sep being the separator used) or xx+ but no other format
    else {
      age_format <- age_format_check(data)
      # check that only one type of 'main' separator is used, i.e. not allowed c("31-35",36_40")
      if (length(age_format$agegrp_div) > 1) {
        errors <- append(errors, paste0("More than one separator is used for age_group. Please decide for one of these ", paste0(age_format$agegrp_div, collapse = ","), " separators."))
      }

      if (!is_last_age_group_format(data$age_group)) {
        errors <- append(errors, "The last/oldest age group does not follow the required format. Allowed is either digit+, >digit or digit separator digit (e.g. 100+, >100, 90-100).")
      }
    }
  }
  errors
}


#' Helper function to check for presence of age variable or instead age_group
#' @param data_columns vector, column names of raw surveillance linelist
#' @returns boolean, TRUE when the required variables are present, FALSE if not present
check_any_age <- function(data_columns) {
  if (any(c("age", "age_group") %in% data_columns)) {
    FALSE
  } else {
    TRUE
  }
}

#' retrieve variables which are in provided in surveillance linelist but are not used in the tool
#' @param data data.frame, raw linelist of surveillance cases
#' @returns vector with all column names which are not part of the defined input data variables
get_unused_variables <- function(data) {
  setdiff(colnames(data), SignalDetectionTool::input_metadata$Variable)
}

#' retrieve 'case_id's which have missing values in computationally crucial
#' variables
#' @param data data.frame, raw linelist of surveillance cases
#' @returns list of column names, number of missing values and the first five
#'   'case_id's for which values are missing
get_missing_data <- function(data) {
  missing_values <- lapply(check_for_missing_values(), function(x) {
    if (x %in% names(data)) { # only check columns that are present in the data
      cases_with_missing_data <- data %>%
        dplyr::filter(is.na(!!rlang::sym(x))) %>%
        dplyr::pull(case_id)
    } else {
      cases_with_missing_data <- NULL
    }
    if (length(cases_with_missing_data) == 0) {
      missing_msg <- NULL
    } else {
      missing_msg <- shiny::tags$li(
        shiny::tags$span(
          class = "more",
          paste0(
            x, ": ", length(cases_with_missing_data),
            " cases with missing value (",
            paste0(cases_with_missing_data, collapse = ", "),
            ")"
          )
        )
      )
    }
    missing_msg
  })
  missing_values[!sapply(missing_values, is.null)]
}

#' check whether there is a completely empty row in provided surveillance data
#' @param data data.frame, raw linelist of surveillance cases
#' @returns boolean, TRUE when there was an empty row inside the data, FALSE when no empty rows
check_empty_rows <- function(data) {
  n <- nrow(data)
  if (n == 0L) return(FALSE)

  # POSIXct -> character (as in the original)
  is_posix <- vapply(data, lubridate::is.POSIXct, logical(1L))
  if (any(is_posix)) {
    data[is_posix] <- lapply(data[is_posix], as.character)
  }

  # For each column a logical vector: TRUE if the field is "empty" (NA or "")
  is_empty_col <- function(x) {
    # Treat factors and POSIXct (now char) as character
    if (is.factor(x)) x <- as.character(x)
    # For non-character types there are usually no "", only NA
    if (is.character(x)) {
      is.na(x) | x == ""
    } else {
      is.na(x)
    }
  }

  # Result: matrix nrow(data) x ncol(data)
  empty_mat <- vapply(data, is_empty_col, logical(n))

  # At least one row in which all columns are "empty"?
  any(rowSums(empty_mat) == ncol(data))
}

#' Check whether the region and corresponding region_id columns only have one region name per ID
#' @param data data.frame, raw linelist of surveillance cases
#' @param regions vector of strings, specifying the region variable names, i.e. c("county","community")
#' @returns list, empty when no errors occured or filled with error messages
check_region_region_id_consistency <- function(data, regions) {
  errors <- list()

  for (i in seq_along(regions)) {
    region <- regions[i]
    region_id <- get_region_id_from_region(region)

    if (region %in% colnames(data) & region_id %in% colnames(data)) {
      region_and_region_id <- data %>%
        dplyr::distinct(!!rlang::sym(region_id), !!rlang::sym(region))
      duplicated_ids <- any(duplicated(region_and_region_id %>%
        dplyr::select(region_id)), na.rm = T)
      if (duplicated_ids) {
        error_message <- paste0("There are several differing region names in the column ", region, " which have the same ", region_id, ". Each ", region_id, " should only match one ", region, " . Please check whether there are any typos in your ", region, " column")
        errors <- append(errors, error_message)
      }
    }
  }
  errors
}


#' checking age_group column only containing digits, separators and <,>,+
#' For the first age_group using seperator, i.e. 00-05, <5 is allowed. For the last age group using seperators, i.e. 95-100, >100 and 100+ is allowed. Separators like - (dash), _ (underscore), and — (em dash) are also allowed for intermediate age ranges.
#' @param col character vector, vector containing age groups to check
#' @returns boolean, when TRUE all values of col are in the required format, when FALSE at least one is not in required format
#' @examples
#' \dontrun{
#' is_age_group_format(c("<30", "30-35", "40-45", "45+")) # Should return TRUE
#' is_age_group_format(c("<30", "30-35", "40-45", ">45")) # Should return TRUE
#' is_age_group_format(c("below_one", "one_to_five")) # Should return FALSE
#' is_age_group_format(c("<30", "30-35", "40/45", "45+")) # Should return FALSE
#' is_age_group_format(c("<30", "30-35", "40+45", "45+")) # Should return FALSE
#' }
is_age_group_format <- function(col) {
  all(grepl("^<?\\d{1,}[-_\u2014]?\\d{0,}$|^\\d{1,}[+]$|^>\\d{1,}$", col) | is.na(col) | col == "" | col == "unknown" | col == "NA")
}

#' checking the format of the last/biggest age group to follow the format digit separator digit, digit+ or >digit
#' @param col character vector, vector containing age groups to check
#' @returns boolean, when TRUE the largest age group of col is in the required format, when FALSE it is not in required format
#' @examples
#' \dontrun{
#' is_last_age_group_format(c("<30", "30_35", "40_45", "45+")) # Should return TRUE
#' is_last_age_group_format(c("<30", "30-35", "40-45", "45-50")) # Should return TRUE
#' is_last_age_group_format(c("<30", "30-35", "40-45", ">45")) # Should return TRUE
#' is_last_age_group_format(c("<30", "30-35", "40-45", "45-")) # Should return FALSE
#' is_last_age_group_format(c("<30", "30-35", "40-45", "45")) # Should return FALSE
#' }
is_last_age_group_format <- function(col) {
  # get the last/oldest age_group from the data
  age <- unlist(stringr::str_extract_all(col, "\\d+"))
  age <- as.numeric(age)
  max_age <- max(age, na.rm = TRUE)
  max_age_str <- as.character(max_age)

  # Find all age groups that contain the maximum age, this could also be 70-80 and 80+ but both are fine
  final_age_groups <- col[grep(max_age_str, col)]
  # regex checking format being only allowed format
  # digit sep digit or digit+
  all(grepl("^\\d{1,}[-_\u2014]\\d{1,}$|^\\d{1,}[+]$|^>\\d{1,}$", final_age_groups))
}


#' checking YYYYY-mm-dd format of date variables
#' @param date_col character vector, vector containing dates to check
#' @returns boolean, when TRUE all values of date_col are in the required format, when FALSE at least one is not in required format
#' @examples
#' \dontrun{
#' is_ISO8601(c("2023-04-01", "2022-12-31"))
#' }
is_ISO8601 <- function(date_col) {
  # need to allow empty entries inside the data variable
  all(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_col) | is.na(date_col) | date_col == "" | date_col == "unknown" | date_col == "NA")
}

#' detailed check of date variables
#' check that months are numbers between 01-12 and days are from 01-31
#' @param date_col character vector, vector containing dates to check
#' @returns boolean, when TRUE all values of date_col are in the required format, when FALSE at least one is not in required format
#' @examples
#' \dontrun{
#' is_ISO8601_detailed(c("2023-04-01", "2022-12-32")) # Should return FALSE
#' is_ISO8601_detailed(c("2023-04-01", "2022-12-31")) # Should return TRUE
#' }
is_ISO8601_detailed <- function(date_col) {
  pattern <- "^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"

  all(grepl(pattern, date_col) | is.na(date_col) | date_col == "" | date_col == "unknown" | date_col == "NA")
}

#' Retrieveing which columns in the dataset only contain missing values
#' @param data data.frame, dataset to check for empty columns can be linelist of surveillance data
#' @returns named vector with column names and boolean specifying complete missingness or not
get_empty_columns <- function(data) {
  apply(data, 2, function(x) {
    (all(is.na(x)) | all(x == "") | all(x == "unknown") | all(x == "NA"))
  })
}

#' Removing columns from data which only contain missing values
#' @param data data.frame, dataset to remove empty columns from, can be linelist of surveillance data
#' @returns data.frame without columns which only contained missing values
remove_empty_columns <- function(data) {
  is_empty_column <- function(x) {
    if (is.factor(x)) x <- as.character(x)

    if (is.character(x)) {
      all(is.na(x) | x == "")
    } else {
      all(is.na(x))
    }
  }

  keep <- !vapply(data, is_empty_column, logical(1L))
  data[keep]
}



#' Helper to check that values of a character variable are in given levels
#' @param vector a vector i.e. column of the data whose values should be checked
#' @param levels vector with levels the vector should have
#' @returns boolean, TRUE when all values are inside levels, FALSE if there is a value which was not specified in levels
check_character_levels <- function(vector, levels) {
  all(vector %in% levels)
}

#' Checking for duplicates in case_id
#' @param data data.frame, raw linelist of surveillance cases
#' @returns data.frame with duplicated case_ids
get_case_id_duplicates <- function(data) {
  data %>%
    dplyr::filter(duplicated(case_id))
}

#' check that shapefile contains NUTS_ID, LEVL_CODE, and CNTR_CODE
#' @param shape shapefile object loaded using sf::st_read()
#' @returns if check passess, returns TRUE invisibly. If not, throws an error message
check_columns_shapefile <- function(shape) {
  col_names <- colnames(shape)
  # NUTS_ID, LEVL_CODE, and CNTR_CODE are subset of colnames(shape)
  checkmate::assert_subset(c("NUTS_ID", "LEVL_CODE", "CNTR_CODE"), col_names)
}
