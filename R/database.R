#' Load Data from Database (Example Implementation)
#'
#' This function demonstrates how to load data from a database.
#' Users should override this function to implement their own database-specific logic.
#'
#' @return A data frame containing the queried and transformed data.
#' @export
load_data_db <- function(){

  dbcon <- get_database_connection()

  dbdata <- query_database(dbcon)

  transform_required_format(dbdata)
}

#' Establish a Database Connection (Example Implementation)
#'
#' This function provides an example connection to an SQLite database shipped with the package.
#' Users must override this function to establish connections to their specific database systems.
#'
#' @return A database connection object.
#' @export
get_database_connection <- function(){

  # load data into temporary db
  dbcon <- DBI::dbConnect(RSQLite::SQLite(),
                          dbname=system.file("extdata/example_db.sqlite", package="SignalDetectionTool"))

  dbcon
}

#' Query Database (Example Implementation)
#'
#' This function demonstrates how to retrieve data from a database using dbplyr's lazy collection method.
#' Users should override this function to define their own queries tailored to their database schema.
#'
#' @param db_connection A database connection object.
#' @return A data frame containing the query results.
#' @export
query_database <- function(db_connection){

  # can use dbplyr
  dbtable <- dplyr::tbl(db_connection, "cases")
  pathogen <- get_data_config_value("params:pathogen")

  data <- dbtable %>%
    dplyr::filter(pathogen == pathogen) %>%
    dplyr::collect()

  DBI::dbDisconnect(db_connection)

  as.data.frame(data) # class of returned object needs to be data.frame
}


#' Transform Data to Required Format (Example Implementation)
#'
#' This function applies transformations to the queried data.
#' Users should override this function to perform any necessary data transformations specific to their application.
#'
#' @param data A data frame containing the raw query results.
#' @return A data frame with transformed data.
#' @export
transform_required_format <- function(data){
  # convert to app required formats
  data %>% dplyr::mutate(sex = dplyr::case_when(
    sex == "f" ~ "female",
    sex == "m" ~ "male",
    TRUE ~ sex # other cases not affected
  ))
}
