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
