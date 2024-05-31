#' Function to extract corresponding region to the region_id variable
#' @param region_id character specifying the column name of regional variables for example "county_id"
#' @return character, giving the region column name without id
get_region_from_region_id <- function(region_id){
  stringr::str_split_1(region_id,"_")[1]
}

#' Function to get the region_id variable names from the region variables
#' @param region character, such as "county", "state"
#' @return character, attaching "_id" to the region name, i.e. "county_id", "state_id"
get_region_id_from_region <- function(region){
  paste0(region,"_id")
}

#' Function to retrieve name from named vector given its value
#' Can be used to retrieve the "pretty" names to show to the user and in the background work with the values
#' @param value character, value of the named list, i.e. "farrington"
#' @param named_vector named vector
#' @return character, name of the named_vector corresponding to value
#' @examples
#' \dontrun{
#' get_name_by_value("farrington",available_algorithms())
#' }
get_name_by_value <- function(value, named_vector){
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
  if (category == "sex"| category == "age_group") {
    category_levels <- paste0(category, "_levels")
    signals_agg <- signals_agg %>%
      dplyr::mutate(stratum = factor(stratum, levels = do.call(category_levels, list())))
  } else {
    # if it is a yes no variable order it
    if(all(signals_agg$stratum %in% yes_no_unknown_levels())){
      signals_agg <- signals_agg %>%
        dplyr::mutate(stratum = factor(stratum, levels = yes_no_unknown_levels()))
    # otherwise just create a factor without special ordering
    }else{
      signals_agg <- signals_agg %>%
        dplyr::mutate(stratum = factor(stratum))
    }

  }
  # it is an open issue #347 on forcats that unknown levels are added even when no NA was there
  # thus write code for workaround
  if(any(is.na(signals_agg$stratum))){
    signals_agg <- signals_agg %>%
      dplyr::mutate(stratum = forcats::fct_na_value_to_level(stratum, level = "unknown"))
  }

  signals_agg
}


