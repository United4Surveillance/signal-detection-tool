# functions for helping to decide whether a visualisation in form of a map or a bar graph are shown or a table is shown


#' Decider for creating a map or a table based on whether all NUTS_ids are found in the shapefile
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms
#' @param data_surveillance data.frame, surveillance linelist
#' @param region_id character, specifying the variable for the region_id to be shown should be one of ("country_id","state_id",
#' "county_id","community_id","region_level1_id", "region_level2_id","region_level3_id")
#' @param shape sf, shapefile
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @returns a table or a plot depending on whether the matching of the NUTS IDs was fully possible, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "county_id"))
#' signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
#' create_map_or_table(signals_agg, input_example, "county_id", nuts_shp)
#' }
create_map_or_table <- function(signals_agg,
                                data_surveillance,
                                region_id,
                                shape,
                                interactive = TRUE) {
  checkmate::assertChoice(region_id, region_id_variable_names())

  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  checkmate::assertClass(shape, "sf")
  checkmate::assertClass(data_surveillance, "data.frame")
  checkmate::assertClass(signals_agg, "data.frame")

  signals_agg <- signals_agg %>%
    dplyr::filter(category == region_id)

  shape <- shape %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    identity()

  # get the matching NUTS ids
  signals_with_matching_NUTS <- signals_agg %>%
    dplyr::semi_join(shape, by = c("stratum" = "NUTS_ID"))

  # check whether all NUTS_ids are found in the shapefile
  n_NUTS_signals <- dplyr::n_distinct(signals_agg$stratum)
  n_NUTS_matching <- dplyr::n_distinct(signals_with_matching_NUTS$stratum)

  shape_with_signals <- shape %>%
    dplyr::left_join(signals_agg, by = c("NUTS_ID" = "stratum")) %>%
    dplyr::filter(!is.na(cases))

  # when not all NUTS in the signals match NUTS IDs in the shapefile
  if (n_NUTS_matching < n_NUTS_signals) {
    # merge the region names for the NUTS_ids to the data when names are available
    region <- get_region_from_region_id(region_id)

    if (region %in% colnames(data_surveillance)) {
      region_and_region_id <- data_surveillance %>%
        dplyr::distinct(!!rlang::sym(region_id), !!rlang::sym(region)) %>%
        dplyr::rename(stratum = region_id)
      signals_agg <- signals_agg %>%
        dplyr::left_join(region_and_region_id, by = c("stratum")) %>%
        dplyr::select(region, cases, any_alarms, n_alarms)
    } else {
      # leave the ids in the table as no region names available
      signals_agg <- signals_agg %>%
        dplyr::select(!!region := stratum, cases, any_alarms, n_alarms)
    }

    # make a table function so that those rows with any_alarms = TRUE are marked in red
    # for this write a seperate function that uses create_table but adapt it to explicitly the table for the signals with red marking and other column headers
    output <- create_table(signals_agg %>%
      convert_columns_integer(c("cases", "n_alarms")),
      interactive = interactive)
  } else {
    output <- plot_regional(shape_with_signals,
      interactive = interactive
    )
  }
  return(output)
}
