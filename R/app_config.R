app_cache_env <- new.env()
app_cache_env$sex_levels <- c("male", "female", "diverse", NA_character_)
app_cache_env$age_group_levels <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100-104", "105-109", NA_character_)



#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "SignalDetectionTool")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv(
        "R_CONFIG_ACTIVE",
        "default"
      )
    ),
    use_parent = TRUE,
    # Modify this if your config file is somewhere else
    file = app_sys("golem-config.yml")) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' Retrieve a Configuration Value from DATA_CONFIG
#'
#' This function retrieves a configuration value from the global `DATA_CONFIG` list
#' using a colon-separated parameter name. If the parameter is not found, a default
#' value is returned. Optionally, the retrieved value can be checked against a set
#' of acceptable values.
#'
#' @param parameter_name A character string specifying the configuration parameter.
#'   The parameter can be a colon-separated path to a nested value.
#' @param default_value The value to return if the parameter is not found in `DATA_CONFIG`.
#'   Defaults to `NULL`.
#' @param acceptable_values An optional vector of acceptable values. If provided,
#'   the function returns the intersection of the retrieved value and this set.
#'   If no intersection is found, the `default_value` is returned.
#'
#' @return The retrieved configuration value if found. If `acceptable_values` is provided,
#'   the function returns the intersection of the retrieved value and `acceptable_values`,
#'   or `default_value` if there is no intersection. If the parameter is not found,
#'   `default_value` is returned.
#'
#' @examples
#' # Assuming DATA_CONFIG is defined as:
#' DATA_CONFIG <- list(api = list(endpoint = "https://example.com", timeout = 30))
#'
#' # Retrieve a nested value
#' get_data_config_value("api:endpoint", default_value = "https://fallback.com")
#'
#' # Retrieve with acceptable values
#' get_data_config_value("api:timeout", default_value = 10, acceptable_values = c(10, 20, 30))
#'
#' @importFrom purrr pluck
#' @export
get_data_config_value <- function(parameter_name,
                                  default_value=NULL,
                                  acceptable_values=NULL){

  if(!exists("DATA_CONFIG")){
    return(default_value)
  }
  # turn colon separated into list of parameters
  params <- as.list(unlist(strsplit(parameter_name, ":")))
  # prepare arguments for pluck
  args <- c(list(DATA_CONFIG), params, list(.default=default_value))
  # get values from config
  config_value <- do.call(purrr::pluck, args)

  if(!is.null(acceptable_values)){
    intersection <- base::intersect(config_value, acceptable_values)
    if(length(intersection)){
      return(intersection)
    } else { return(default_value)}
  } else{
    return(config_value)
  }
}
