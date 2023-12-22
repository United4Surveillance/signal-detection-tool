# returns empty list when there are no errors
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

  # remove all columns which are not filled
  empty_columns <- apply(data, 2, function(x) {
    (all(is.na(x)) | all(x == ""))
  })
  empty_column_names <- names(empty_columns)[empty_columns]
  data <- data %>%
    dplyr::select(-all_of(empty_column_names))

  # check mandatory and optional variabless
  errors_mandatory <- check_mandatory_variables(data)
  errors_optional <- check_type_and_value_optional_variables(data)
  errors <- c(errors, errors_mandatory, errors_optional)

  # check outbreak information when available
  if ("outbreak_status" %in% colnames(data)) {
    errors <- check_raw_outbreak_data(data, outbreaks_in_surveillance_data = TRUE)
  }

  # return TRUE or print error messages
  if (length(errors) != 0) {
    # remove empty slots
    errors <- errors[sapply(errors, function(element) !is.null(element))]
  }
    errors
}

check_raw_outbreak_data <- function(data,
                                    outbreaks_in_surveillance_data = TRUE) {
  errors <- list()

  if (!outbreaks_in_surveillance_data) {
    # check that reading in worked
    if (nrow(data) == 0) {
      errors <- append(errors, "Loaded data is empty")
      return(errors)
    }
  }

  errors_presence <- check_presence_mandatory_variables_outbreak(
    data,
    outbreaks_in_surveillance_data
  )
  errors_type_and_value <- check_type_and_value_outbreak_variables(
    data,
    outbreaks_in_surveillance_data
  )

  errors <- c(errors_presence, errors_type_and_value)

  if (length(errors) != 0) {
    # remove empty slots
    errors <- errors[sapply(errors, function(element) !is.null(element))]
  }
  errors
}

check_consistency_surveillance_and_outbreak_data <- function(data_surveillance,
                                                             data_outbreak) {
  errors <- list()

  # are the case_ids in the same format?
  type_case_id_surveillance <- class(data_surveillance$case_id)
  type_case_id_truth <- class(data_outbreak$case_id)
  if (type_case_id_surveillance != type_case_id_truth) {
    errors <- append(
      errors,
      "case_ids do not have the same format in the surveillance and outbreak data"
    )
  }

  # check that each case_id in data_surveillance is present in data_outbreak
  # Question: do we want to check this?
  unmatched_case_ids <- data_surveillance %>%
    dplyr::anti_join(data_outbreak, by = "case_id")

  if (nrow(unmatched_case_ids) > 0) {
    # we could return which case_ids are not found
    errors <- append(
      errors,
      paste0(
        nrow(unmatched_case_ids),
        "case_ids do not exist in the outbreak data"
      )
    )
  }
  errors
}

# checking mandatory variables in the surveillance data
# 1) if they are present in the data
# 2) if they have the correct type and correct values
check_mandatory_variables <- function(data) {
  errors_presence <- check_presence_mandatory_variables(data)
  errors_types <- check_type_and_value_mandatory_variables(data)

  errors <- c(errors_presence, errors_types)

  errors
}

check_character_levels <- function(vector, levels) {
  all(vector %in% levels)
}

check_presence_mandatory_variables <- function(data) {
  errors <- list()

  # age and date_report are checked seperately as there are several options how to provide them
  mandatory_columns <- c("case_id", "country", "country_id", "pathogen")

  data_columns <- colnames(data)
  missing_columns <- setdiff(mandatory_columns, data_columns)

  # check whether data contains one of date_report or year_report and week_report
  if (check_any_date_report(data_columns)) {
    missing_columns <- c(missing_columns, "date_report")
  }

  # check whether data contains one of age or agegroup
  if (check_any_age(data_columns)) {
    missing_columns <- c(missing_columns, "age or age_group")
  }

  if (length(missing_columns) != 0) {
    errors <- lapply(missing_columns, function(col) paste0("Missing or empty mandatory column '", col, "' in the data"))
  }

  errors
}

# optional parameter specifying whether outbreak information is already inside the surveillance data (TRUE)
# or provided as a seperate file
check_presence_mandatory_variables_outbreak <- function(data,
                                                        outbreaks_in_surveillance_data = TRUE) {
  errors <- list()

  # case_id is already checked in the surveillance data
  if (outbreaks_in_surveillance_data) {
    mandatory_columns <- c("outbreak_status")
  } else {
    mandatory_columns <- c("case_id", "outbreak_status")
  }

  data_columns <- colnames(data)
  missing_columns <- setdiff(mandatory_columns, data_columns)

  if (length(missing_columns) != 0) {
    errors <- lapply(missing_columns, function(col) paste0("Missing or empty mandatory column '", col, "' in the data"))
  }

  errors
}

# those mandatory variables which are there are checked for their type
check_type_and_value_mandatory_variables <- function(data) {
  errors <- list()
  data_columns <- colnames(data)

  if ("case_id" %in% data_columns) {
    errors <- append(errors, check_type_and_value_case_id(data))
  }
  if ("date_report" %in% data_columns) {
    errors <- append(errors, check_type_and_value_date(data, "date_report"))
  }
  if ("report_week" %in% data_columns) {
    if (!checkmate::test_integerish(data$report_week)) {
      errors <- append(errors, "report_week is not an integer")
    } else {
      # any NA or empty strings
      if (any(is.na(data$report_week)) | any(data$report_week == "")) {
        errors <- append(errors, "Missing/empty report_week inside data")
      }
    }
  }
  if ("report_year" %in% data_columns) {
    if (!checkmate::test_integerish(data$report_year)) {
      errors <- append(errors, "report_year is not an integer")
    } else {
      # any NA or empty strings
      if (any(is.na(data$report_year)) | any(data$report_year == "")) {
        errors <- append(errors, "Missing/empty report_year inside data")
      }
    }
  }
  if ("age" %in% data_columns) {
    if (!checkmate::test_integerish(data$age)) {
      errors <- append(errors, "age is not an integer")
    }
  }
  if ("age_group" %in% data_columns) {
    if (!checkmate::test_character(data$age_group)) {
      errors <- append(errors, "age_group is not a character")
    } # TODO add check that there are only numbers empty spaces and - but no characters
  }
  if ("country" %in% data_columns) {
    if (!checkmate::test_character(data$country)) {
      errors <- append(errors, "country is not a character")
    }
  }
  if ("country_id" %in% data_columns) {
    if (!checkmate::test_character(data$country_id)) {
      errors <- append(errors, "country_id is not a character")
    }
  }
  if ("pathogen" %in% data_columns) {
    if (!checkmate::test_character(data$pathogen)) {
      errors <- append(errors, "pathogen is not a character")
    }
  }
  errors
}

check_type_and_value_outbreak_variables <- function(data,
                                                    outbreaks_in_surveillance_data = TRUE) {
  errors <- list()
  data_columns <- colnames(data)

  if (!outbreaks_in_surveillance_data) {
    if ("case_id" %in% data_columns) {
      errors <- append(errors, check_type_and_value_case_id(data))
    }
  }
  if ("outbreak_status" %in% data_columns) {
    errors <- append(errors, check_type_and_value_yes_no_unknown(data, "outbreak_status"))
  }
  if ("outbreak_id" %in% data_columns) {
    if (!checkmate::qtest(data$outbreak_id, c("s", "n"))) {
      errors <- append(errors, "outbreak_id is not a character or a numeric")
    }
  }

  errors
}

check_type_and_value_optional_variables <- function(data) {
  errors <- list()
  data_columns <- colnames(data)

  if ("sex" %in% data_columns) {
    if (!checkmate::test_character(data$sex)) {
      errors <- append(errors, "sex is not a character")
    } else {
      if (!check_character_levels(tolower(data$sex), sex_raw_levels())) {
        errors <- append(errors, paste0("Sex does not have the required levels ", paste(unlist(sex_levels()), collapse = ", ")))
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
    if (!checkmate::test_character(data[[id_var]])) {
      errors <- append(errors, paste0(id_var, " is not a character"))
    }
  }

  # check yes no unknown vars
  yes_no_unknown_vars <- c(
    "hospitalization",
    "death",
    "vaccination"
  )
  yes_no_unknown_vars <- intersect(data_columns, yes_no_unknown_vars)

  for (yes_no_unknown_var in yes_no_unknown_vars) {
    errors <- append(errors, check_type_and_value_yes_no_unknown(data, yes_no_unknown_var))
  }

  # check remaining character variables
  remaining_char_vars <- c(
    "state",
    "regional_level1",
    "county",
    "regional_level2",
    "community",
    "regional_level3",
    "age_group",
    "occupation",
    "place_of_infection",
    "subtype",
    "symptoms",
    "risks"
  )
  remaining_char_vars <- intersect(data_columns, remaining_char_vars)

  for (remaining_char_var in remaining_char_vars) {
    if (!checkmate::test_character(data[[remaining_char_var]])) {
      errors <- append(errors, paste0(remaining_char_var, " is not a character"))
    }
  }

  errors
}

check_type_and_value_date <- function(data, date_var) {
  errors <- list()

  # case when column type was already read in as date by read_excel then transform to character for checks
  if (lubridate::is.POSIXct(data[[date_var]])) {
    data[[date_var]] <- as.character(data[[date_var]])
  }

  if (!checkmate::test_character(data[[date_var]])) {
    errors <- append(errors, paste0(date_var, " is not a character"))
  } else {
    if (!is_ISO8601(data[[date_var]])) {
      errors <- append(errors, paste0(date_var, " is not in ISO 8601 format YYYY-MM-DD"))
    } else if (!is_ISO8601_detailed(data[[date_var]])) {
      errors <- append(errors, paste0(date_var, " has values for month outside 01-12 or for days outside 01-31"))
    }
  }
  errors
}

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

check_type_and_value_yes_no_unknown <- function(data, var) {

  errors <- list()

  if (!checkmate::test_character(data[[var]])) {
    errors <- append(errors, paste0(var, " is not a character"))
  } else {
    if (!check_character_levels(tolower(data[[var]]), yes_no_unknown_raw_levels())) {
      error_message <- paste0(var, " does not have the required levels ", paste(unlist(yes_no_unknown_levels()), collapse = ", "))
      errors <- append(errors, error_message)
    }
  }
  errors
}

# when there is a problem set to TRUE
check_any_date_report <- function(data_columns) {
  if ("date_report" %in% data_columns) {
    FALSE
  } else if (all(c("week_report", "year_report") %in% data_columns)) {
    FALSE
  } else {
    TRUE
  }
}

check_any_age <- function(data_columns) {
  if (any(c("age", "age_group") %in% data_columns)) {
    FALSE
  } else {
    TRUE
  }
}

# retrieve variables which are in addition provided with the dataset but are not used for analysis
get_unused_variables <- function(data) {
  setdiff(colnames(data), input_and_outbreak_variables())
}

# check for a complete empty row in provided data
# when there is a problem set to TRUE
check_empty_rows <- function(data) {
  # transform POSICXct variables to character before to not produce an error in the check below
  data <- data %>%
    dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character))

  any(apply(data == "" | is.na(data) | is.null(data), 1, all))
}

# checking YYYYY-mm-dd format
is_ISO8601 <- function(date_var) {
  all(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_var) | is.na(date_var) | date_var == "")
}

#
# detailed check that months are numbers between 01-12 and days are from 01-31
is_ISO8601_detailed <- function(date_var) {
  pattern <- "^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"

  all(grepl(pattern, date_var) | is.na(date_var) | date_var == "")
}

get_case_id_duplicates <- function(data) {
  data %>%
    dplyr::filter(duplicated(case_id))
}

check_week_year_duplicates <- function(data, strata = NULL) {
  stopifnot(get_week_year_duplicates(data, strata) %>%
    nrow() == 0)
}

get_week_year_duplicates <- function(data, strata = NULL) {
  vars <- c(strata, "week", "year")

  data %>%
    dplyr::filter(duplicated(.[vars]))
}
