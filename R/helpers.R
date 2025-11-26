#' Function to extract corresponding region to the region_id variable
#' @param region_id character specifying the column name of regional variables for example "county_id"
#' @return character, giving the region column name without id
get_region_from_region_id <- function(region_id) {
  stringr::str_split_1(region_id, "_")[1]
}

#' Function to get the region_id variable names from the region variables
#' @param region character, such as "county", "state"
#' @return character, attaching "_id" to the region name, i.e. "county_id", "state_id"
get_region_id_from_region <- function(region) {
  paste0(region, "_id")
}

#' Function to retrieve name from named vector given its value
#' Can be used to retrieve the "pretty" names to show to the user and in the background work with the values
#' @param value character, value of the named list, i.e. "farrington"
#' @param named_vector named vector
#' @return character, name of the named_vector corresponding to value
#' @examples
#' \dontrun{
#' get_name_by_value("farrington", available_algorithms())
#' }
get_name_by_value <- function(value, named_vector) {
  names(named_vector)[which(named_vector == value)]
}

#' Create a factor out of the stratum column with transforming NA to unknown
#' @param signals_agg tibble or data.frame, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}} for only one category.
#'
#' @return tibble with stratum column being a factor
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   year = 2020:2023,
#'   week = 1:4,
#'   cases = 10:13,
#'   any_alarms = c(TRUE, FALSE, TRUE, FALSE),
#'   n_alarms = c(0, 2, 0, 1),
#'   category = c("sex", "sex", "sex", "sex"),
#'   stratum = c("female", "male", "diverse", NA)
#' )
#' create_factor_with_unknown(data)
#' }
create_factor_with_unknown <- function(signals_agg) {
  category <- unique(signals_agg$category)
  stopifnot(length(category) == 1)
  # age_group should be added later as well
  if (category == "sex" | category == "age_group") {
    category_levels <- paste0(category, "_levels")
    signals_agg <- signals_agg %>%
      dplyr::mutate(stratum = factor(stratum, levels = do.call(category_levels, list())))
  } else {
    # if it is a yes no variable order it
    if (all(signals_agg$stratum %in% yes_no_unknown_levels())) {
      signals_agg <- signals_agg %>%
        dplyr::mutate(stratum = factor(stratum, levels = yes_no_unknown_levels()))
      # otherwise just create a factor without special ordering
    } else {
      signals_agg <- signals_agg %>%
        dplyr::mutate(stratum = factor(stratum))
    }
  }
  # it is an open issue #347 on forcats that unknown levels are added even when no NA was there
  # thus write code for workaround
  if (any(is.na(signals_agg$stratum))) {
    signals_agg <- signals_agg %>%
      dplyr::mutate(stratum = forcats::fct_na_value_to_level(stratum, level = "unknown"))
  }

  signals_agg
}

#' Get isoweek and isoyear from a given date
#' @param date character in the format "yyyy-mm-dd"
#' @return list containing a isoweek (integer) and isoyear (integer)
#' @examples \dontrun{
#' get_iso_week_year("2020-03-04")
#' }
get_iso_week_year <- function(date) {
  # think about problematic case of wrong aggregation
  date <- as.Date(date)

  iso_week <- lubridate::isoweek(date)
  iso_year <- lubridate::isoyear(date)

  return(list(iso_week = iso_week, iso_year = iso_year))
}


#' Create a Date from ISO Year and Week
#'
#' This function converts an ISO year and ISO week number into a date. The date
#' returned corresponds to the first day (Monday) of the specified ISO week.
#'
#' @param week Integer. The ISO week number (1 to 53).
#' @param year Integer. The ISO year.
#' @return A `Date` object representing the first day of the specified ISO week.
#'
#' @examples \dontrun{
#' isoweek_to_date(week = 15, year = 2023) # Returns the date for the first day of ISO week 15 in 2023
#' }
isoweek_to_date <- function(week, year) {
  # Format the ISO week string
  iso_week_str <- sprintf("%d-W%02d-1", year, week)

  # Convert to date (the first day of the ISO week)
  date <- ISOweek::ISOweek2date(iso_week_str)
  return(date)
}


#' Get row number of aggregated data which is the isoweek and isoyear corresponding to the date given
#' @param date character in the format "yyyy-mm-dd"
#' @param data_aggregated data.frame, aggregated data with case counts
#' @return integer row number of data_aggregated where the isoweek and isoyear of the given date are
#' @examples \dontrun{
#' data_agg <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' get_intervention_timepoint("2020-03-04", data_agg)
#' }
get_intervention_timepoint <- function(date, data_aggregated) {
  iso_week_year <- get_iso_week_year(date)
  which(data_aggregated$year == iso_week_year$iso_year & data_aggregated$week == iso_week_year$iso_week)
}

#' Get minimum and maximum date in the linelist
#' @param data A \code{data.frame} containing the case linelist.
#' @param date_var A character string specifying the name of the date variable in \code{data}.
#'        Default is \code{"date_report"}.
#' @return list with Dates, the minimum and maximum dates of the linelist
get_min_max_date <- function(data, date_var = "date_report"){

  min_date <- min(data[[date_var]], na.rm = TRUE)
  max_date <- max(data[[date_var]], na.rm = TRUE)

  # Convert to Date if needed
  if (!inherits(min_date, "Date")) {
    min_date <- as.Date(min_date)
  }

  if (!inherits(max_date, "Date")) {
    max_date <- as.Date(max_date)
  }

  return(list(min_date = min_date,
              max_date = max_date))
}
