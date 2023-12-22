sex_raw_levels <- function() {
  c(
    "male",
    "female",
    "diverse",
    "unknown",
    "",
    NA_character_
  )
}

sex_levels <- function() {
  c(
    "male",
    "female",
    "diverse",
    "unknown"
  )
}

yes_no_unknown_raw_levels <- function() {
  c(
    "yes",
    "no",
    "unknown",
    "",
    NA_character_
  )
}

yes_no_unknown_levels <- function() {
  c(
    "yes",
    "no",
    NA_character_
  )
}

# TODO when inside the signal-detection-tool package use 
# input_metadata$Variable and add the outbreak_metadata$variable
input_and_outbreak_variables <- function() {
  c(
    "case_id",
    "date_report",
    "date_onset",
    "date_hospitalization",
    "date_death",
    "date_vaccination",
    "country",
    "country_id",
    "state",
    "state_id",
    "county",
    "county_id",
    "community",
    "community_id",
    "age",
    "age_group",
    "sex",
    "occupation",
    "place_of_infection",
    "place_of_infection_id",
    "pathogen",
    "pathogen_id",
    "subtype",
    "subtype_id",
    "hospitalization",
    "death",
    "vaccination",
    "symptoms",
    "risks",
    "outbreak_status",
    "outbreak_id"
  )
}

yes_no_unknown_variables <- function(){
  
  c(
    "hospitalization",
    "death",
    "vaccination",
    "outbreak_status"
  )
}

region_id_variable_names <- function() {
  c(
    "country_id",
    "state_id",
    "county_id",
    "community_id"
  )
}
