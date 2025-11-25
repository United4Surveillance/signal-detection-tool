# This file should include all relevant helper functions required to run the actual signal detection tool
# Feel free to complete the file


#' Creation of age_group levels from different formats of the age_group column
#'
#' This function returns a character vector with age_group levels based on the data provided.
#' It uses the age_format_check() and complete_agegrp_arr() functions to create the levels
#'
#' @param df A data frame containing an 'age_group' variable.
#' @return character vector containing all age_group levels
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 6, 16), age_group = c("01-05", "6-10", "16-20"))
#' create_age_group_levels(data_frame)
#' }
create_age_group_levels <- function(df) {
  format_check_results <- age_format_check(df)
  all_agegroups <- complete_agegrp_arr(df, format_check_results)
  age_group_levels <- stringr::str_sort(all_agegroups, numeric = TRUE)

  age_group_levels
}
#' Age Group Format Check
#'
#' This function checks the format of the 'age_group' variable in the given data frame. It performs several checks including:
#'   1. Checking if the lengths of age groups are equidistant.
#'   2. Verifying if the age group format is in the "xx-xx" format.
#'   3. Checking if any punctuation characters are used at either end of the age group.
#'
#' @param df A data frame containing an 'age_group' variable.
#'
#' @return A list containing the results of the formatting checks:
#'   \item{agegrp_div}{The most frequently used punctuation character, which serves as the divider in age groups.}
#'   \item{other_punct_character}{Any other punctuation character used. Also used as logical indicator.}
#'   \item{equal_sizing}{Logical indicating whether the lengths of age groups are equidistant.}
#'   \item{format_agegrp_xx}{Indices of entries in 'age_group' not following the "xx-xx" format.}
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 5, 15, 16), age_group = c("01-05", "6-10", "11-15", "16-20"))
#' check_results <- age_format_check(data_frame)
#' print(check_results)
#' }
age_format_check <- function(df) {
  # setting variables
  splits_uniq <- stringr::str_split_fixed(as.character(unique(df$age_group)), "[^[:alnum:]]", 2)
  splits_total <- stringr::str_split_fixed(as.character(df$age_group), "[^[:alnum:]]", 2)
  min_num <- as.numeric(splits_uniq) %>%
    stats::na.omit() %>%
    min()
  max_num <- as.numeric(splits_uniq) %>%
    stats::na.omit() %>%
    max()

  # checking if length is equidistant
  abs_diff <- abs(as.numeric(splits_uniq[, 1]) - as.numeric(splits_uniq[, 2])) %>% stats::na.omit()
  equal_sizing <- (length(unique(abs_diff)) == 1)

  # checking whether xx-xx format is in use
  tmp_check <- union(
    which(stringr::str_length(splits_total[, 1]) < 2),
    which(stringr::str_length(splits_total[, 2]) < 2)
  )

  format_agegrp_xx <-
    setdiff(
      tmp_check,
      which(splits_total == "", arr.ind = TRUE)[, 1]
    )

  # extract which divider is used
  agegrp_div <- stringr::str_match(
    string = as.character(unique(df$age_group)),
    pattern = "\\d+(.*?)\\d+"
  )[, 2] %>%
    stringr::str_subset(".+") %>%
    unique()

  # extract which other special characters are used and how and where
  tmp_start <- stringr::str_extract(
    string = stringr::str_sort(unique(df$age_group), numeric = TRUE),
    pattern = "(^[^[:alnum:]])"
  )

  tmp_end <- stringr::str_extract(
    string = stringr::str_sort(unique(df$age_group), numeric = TRUE),
    pattern = "([^[:alnum:]]$)"
  )
  tmp_df <- data.frame(tmp_start, tmp_end)

  other_punct_char <- list()
  for (df_col in 1:ncol(tmp_df)) {
    for (item in purrr::discard(tmp_df[, df_col], is.na)) {
      num_val <- stringr::str_extract(
        string  = grep(pattern = paste0("[\\", item, "]"), x = unique(df$age_group), value = TRUE),
        pattern = "\\d+"
      )

      other_punct_char[[item]] <- list(
        char_val = item,
        num_val = num_val,
        placement_in_arr = ifelse(num_val == min_num, "start", "end"),
        placement_in_str = ifelse(df_col == 1, "start", "end")
      )
    }
  }

  # return list of formatting checks results
  return(list(
    agegrp_div = agegrp_div,
    other_punct_char = other_punct_char,
    equal_sizing = equal_sizing,
    format_agegrp_xx = format_agegrp_xx
  ))
}


#' Complete Age Group Array
#'
#' This function generates a complete array of age groups based on the format check results and existing age group data.
#' It is particularly useful for completing missing age groups in datasets
#'
#' @param df A data frame containing an 'age_group' variable.
#' @param format_check_results A list containing the results of the age group format check.
#'
#' @return A character vector representing the complete array of age groups.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 5, 15, 16), age_group = c("01-05", "6-10", "11-15", "16-20"))
#' format_check_results <- list(equal_sizing = TRUE, agegrp_div = "-", other_punct_char = list(), format_agegrp_xx = 2)
#' complete_agegrp_arr(data_frame, format_check_results)
#' }
complete_agegrp_arr <- function(df, format_check_results) {
  # find unique elements of age_group
  # remove NA from the unique age_groups for the whole process and add it later again
  tmp_uniq_agegrp <- stringr::str_sort(unique(df$age_group), numeric = TRUE)
  tmp_uniq_agegrp <- stats::na.omit(tmp_uniq_agegrp)


  # check to see if there are any gaps
  # building regex string dependent on level of punct characters found
  regex_string <-
    paste0(
      "[\\",
      gsub(x = format_check_results$agegrp_div, pattern = " ", replacement = ""),
      ""
    )

  if (length(format_check_results$other_punct_char) > 0) {
    for (character in names(format_check_results$other_punct_char)) {
      regex_string <- paste0(regex_string, "\\", gsub(x = character, pattern = " ", replacement = ""))
    }
  }

  regex_string <- paste0(regex_string, "]")

  # splitting the ages, only keeping numeric
  splits <- stringr::str_split_fixed(
    string = tmp_uniq_agegrp,
    pattern = regex_string,
    n = 2
  ) %>%
    gsub(pattern = "\\D", replacement = "")

  # remove rows of only NA or empty
  splits <- splits[!apply(is.na(splits) | splits == "", 1, all), ]

  # if there is equidistance
  if (format_check_results$equal_sizing) {
    # create sequence based on existing age_groups
    # find an example of an age group that is not NULL
    tmp_agegrp <- tmp_uniq_agegrp[grepl(paste0("[\\", format_check_results$agegrp_div, "]"), tmp_uniq_agegrp)][1]

    # get the lower and upper of the age group gap using the first age_group and find the range
    lower_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp), format_check_results$agegrp_div)[1])
    upper_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp), format_check_results$agegrp_div)[2])
    split_range <- plyr::round_any(upper_split - lower_split, accuracy = 5)

    if ("age" %in% colnames(df)) {
      # find the maximum age in relation to the age group gap
      max_age <- max(df$age, na.rm = T)
      max_data_rounded <- plyr::round_any(
        x = max_age,
        accuracy = split_range,
        f = ceiling
      )
    } else {
      # age is not mandatory also only age_group can be in the dataset
      # then take the largest age_group as maximum age
      max_data_rounded <- as.numeric(max(splits))
      max_age <- max_data_rounded
    }


    # make sequences
    seq_lower <- seq(0, max_data_rounded, split_range)
    seq_upper <- (seq_lower - 1)[-1]
    if (max_age >= seq_lower[length(seq_lower)]) {
      # add the last missing upper seq which is lost and needed because we have max_age in this age_group
      seq_upper <- c(seq_upper, seq_upper[length(seq_upper)] + split_range)
    }
  } else if (!format_check_results$equal_sizing) {
    # find where there are missing gaps
    dummy_fill <- data.frame(
      lower = 1:length(tmp_uniq_agegrp) * NA,
      upper = 1:length(tmp_uniq_agegrp) * NA
    )

    for (i in 2:length(splits[, 1])) {
      # compare the start of the next split with the end of the previous to identify gaps
      if (as.numeric(splits[i, 1]) - as.numeric(splits[i - 1, 2]) > 1) {
        dummy_fill[i, 1] <- as.numeric(splits[i - 1, 2])
        dummy_fill[i, 2] <- as.numeric(splits[i, 1])
      }
    }

    dummy_fill <- dummy_fill %>% dplyr::filter(!is.na(lower))

    # create the sequence levels
    # if new age groups were created add them and put them in the correct order
    seq_lower <- c(splits[, 1], (dummy_fill$lower + 1)) %>%
      stringr::str_sort(., numeric = TRUE) %>%
      as.numeric()
    seq_upper <- c(splits[, 2], (dummy_fill$upper - 1)) %>%
      .[nzchar(., keepNA = FALSE)] %>%
      stringr::str_sort(., numeric = TRUE) %>%
      as.numeric()
  }

  # correcting for NAs
  if (any(is.na(seq_lower))) {
    if (which(is.na(seq_lower)) == 1) {
      seq_lower[1] <- 0
    }
    seq_lower <- stats::na.omit(seq_lower)
  }

  if (any(is.na(seq_upper))) {
    seq_upper <- stats::na.omit(seq_upper)
  }

  # ensuring lower and upper seq are equal in length
  if (!(length(seq_lower) == length(seq_upper))) {
    if (length(seq_lower) > length(seq_upper)) {
      seq_lower <- seq_lower[-length(seq_lower)]
    } else {
      seq_upper <- seq_upper[-length(seq_upper)]
    }
  }

  # combine to create the complete age group array
  # adding leading zeros on <10 ages
  all_agegrps <- paste0(
    sprintf("%02d", seq_lower),
    format_check_results$agegrp_div,
    sprintf("%02d", seq_upper)
  )

  # if symbol is used, add it where appropriate
  # make complete string
  if (length(format_check_results$other_punct_char) > 0) {
    max_num_inp <- NULL
    for (spc_char in names(format_check_results$other_punct_char)) {
      # if it belong in front of or behind value
      if (format_check_results$other_punct_char[[spc_char]]$placement_in_str == "start") {
        str_element_1 <- format_check_results$other_punct_char[[spc_char]]$char_val
        str_element_2 <- format_check_results$other_punct_char[[spc_char]]$num_val
      } else {
        str_element_1 <- format_check_results$other_punct_char[[spc_char]]$num_val
        str_element_2 <- format_check_results$other_punct_char[[spc_char]]$char_val
      }

      agegroup_str <- paste0(str_element_1, str_element_2)

      # if it is part of the first or the last number
      if (format_check_results$other_punct_char[[spc_char]]$placement_in_arr == "start") {
        all_agegrps[1] <- agegroup_str
      } else {
        all_agegrps[length(all_agegrps) + 1] <- agegroup_str
        max_num_inp <- format_check_results$other_punct_char[[spc_char]]$num_val
      }
    }

    # remove potential numbers smaller and larger than number with special character attached
    if (!is.null(max_num_inp)) {
      # splitting the ages, only keeping numeric
      splits_tmp <- stringr::str_split_fixed(
        string = all_agegrps,
        pattern = regex_string,
        n = 2
      ) %>%
        gsub(pattern = "\\D", replacement = "")

      rows_to_remove <- which(apply(splits_tmp, 2, as.numeric) > as.numeric(max_num_inp), arr.ind = T) %>%
        as.data.frame() %>%
        dplyr::select(row) %>%
        dplyr::distinct() %>%
        unlist()

      all_agegrps <- all_agegrps[-rows_to_remove]
    }
  }
  # adding back NAs when there were any
  if (any(is.na(df$age_group))) {
    all_agegrps <- c(all_agegrps, NA_character_)
  }

  # checking to see if all agegroups entities are found in the array
  # if not all are present just use the original age_groups in the data
  if (any(unique(df$age_group) %in% all_agegrps == FALSE)) {
    all_agegrps <- unique(df$age_group)
  }

  return(all_agegrps)
}

#' Creates age grouping variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
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
  # Falls age_group schon existiert, nichts neu berechnen
  if (!("age_group" %in% names(df))) {
    # --- Checks -------------------------------------------------------------
    if (!checkmate::test_integerish(df$age)) {
      stop("Type of age is not integer")
    }

    if (!is.null(break_at)) {
      if (!is.integer(break_at)) {
        stop("Input of integer type is only allowed")
      }
      if (is.unsorted(break_at, strictly = TRUE)) {
        stop("Invalid break points")
      }
      inner_breaks <- break_at
    } else {
      inner_breaks <- seq(5L, 125L, 5L)
    }

    # set = alle Untergrenzen inkl. 0, wie vorher
    set <- c(0L, inner_breaks)
    n_int <- length(set)

    # --- Labels vorbereiten (wie in Option 1, aber ohne cut()) -------------
    lower <- set
    upper <- c(set[-1L] - 1L, Inf) # letzte Gruppe offen

    is_last <- is.infinite(upper)
    lower_chr <- sprintf("%02d", as.integer(lower))

    upper_chr <- character(n_int)
    upper_chr[!is_last] <- sprintf("%02d", as.integer(upper[!is_last]))
    upper_chr[is_last] <- "+" # Platzhalter

    labels <- character(n_int)
    labels[!is_last] <- paste(lower_chr[!is_last], upper_chr[!is_last], sep = "-")
    labels[is_last] <- paste0(lower_chr[is_last], "+")

    # --- Gruppenzuordnung mit findInterval (vektorisiert) -------------------
    age <- df$age

    # Index des Intervalls: 0, 1, ..., n_int
    idx <- findInterval(age, set, rightmost.closed = FALSE, all.inside = FALSE)
    # NA in age -> NA in idx, das ist okay

    # EXAKTES Nachbauen der Originalfunktion:
    # Alle nicht-NA, die idx == 0 haben (also age < set[1]),
    # werden der LETZTEN Gruppe zugeordnet.
    idx[!is.na(idx) & idx == 0] <- n_int

    # Jetzt Labels zuordnen
    age_group <- rep(NA_character_, length(age))
    not_na <- !is.na(idx)
    age_group[not_na] <- labels[idx[not_na]]

    df$age_group <- age_group
    df <- df %>% dplyr::relocate(age_group, .after = age)
  }

  # --- Format-Checks wie gehabt --------------------------------------------
  format_check_results <- age_format_check(df)

  if (length(format_check_results$format_agegrp_xx) > 0) {
    splits <- stringr::str_split_fixed(
      as.character(df$age_group),
      format_check_results$agegrp_div,
      2
    )

    idx_fix <- format_check_results$format_agegrp_xx

    df$age_group[idx_fix] <- paste0(
      sprintf("%02d", as.numeric(splits[idx_fix, 1])),
      format_check_results$agegrp_div,
      sprintf("%02d", as.numeric(splits[idx_fix, 2]))
    )
  }

  all_agegroups <- complete_agegrp_arr(df, format_check_results)
  app_cache_env$age_group_levels <- stringr::str_sort(all_agegroups, numeric = TRUE)

  df$age_group <- factor(df$age_group,
    levels = app_cache_env$age_group_levels
  )

  df
}
