#' Allowed levels for sex in raw surveillance data
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

#' Allowed levels for sex in preprocessed surveillance data used for all calculations
sex_levels <- function() {
  app_cache_env$sex_levels
}

#' Allowed levels for variables with yes, no, unknown levels in raw surveillance data
yes_no_unknown_raw_levels <- function() {
  c(
    "yes",
    "no",
    "unknown",
    "",
    NA_character_
  )
}

#' Allowed levels for variables with yes, no, unknown levels in preprocessed surveillance data used for calculations
yes_no_unknown_levels <- function() {
  c(
    "yes",
    "no",
    NA_character_
  )
}

#' Variable names of the variables which have yes, no , unknown levels
yes_no_unknown_variables <- function() {
  c(
    "hospitalization",
    "death",
    "vaccination",
    "outbreak_status"
  )
}

#' Variable names of the region_id variables including those which do not necessarily follow NUTS format
region_id_variable_names <- function() {
  c(
    "country_id",
    "state_id",
    "county_id",
    "community_id",
    "region_level1_id",
    "region_level2_id",
    "region_level3_id"
  )
}

#' Variable names of the region_id variables including those which do not necessarily follow NUTS format
region_variable_names <- function() {
  c(
    "country",
    "state",
    "county",
    "community",
    "region_level1",
    "region_level2",
    "region_level3"
  )
}

available_algorithms <- function() {
  c(
    "FarringtonFlexible" = "farrington",
    "EARS" = "ears",
    "CUSUM" = "cusum",
    "Mean" = "glm mean",
    "Timetrend" = "glm timetrend",
    "Harmonic" = "glm harmonic",
    "Harmonic with timetrend" = "glm harmonic with timetrend",
    "Step harmonic" = "glm farrington",
    "Step harmonic with timetrend" = "glm farrington with timetrend"
  )
}

pretty_variable_names <- function() {
  list(
    age_group = "Age group",
    subtype = "Subtype",
    sex = "Sex",
    hospitalization = "Hospitalization",
    occupation = "Occupation",
    place_of_infection = "Place of infection",
    death = "Death",
    vaccination = "Vaccination"
  )
}

#' Varible names which should be checked for missing values
check_for_missing_values <- function() {
  c("date_report")
}

age_group_levels <- function() {
  app_cache_env$age_group_levels
}
