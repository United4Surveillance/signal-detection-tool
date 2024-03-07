# functions for helping to decide whether a visualisation in form of a map or a bar graph are shown or a table is shown


#' Decider for creating a map or a table based on whether all NUTS_ids are found in the shapefile
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}}. This tibble can contain the aggregated signals for multiple categories i.e. state and county.
#' @param data_surveillance data.frame, surveillance linelist
#' @param region character, specifying the variable for the region to be shown should be one of ("country","state",
#' "county","community","region_level1", "region_level2","region_level3")
#' @param shape sf, shapefile default set to internal europe shapefile nuts_shp
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @returns a table or a plot depending on whether the matching of the NUTS IDs was fully possible, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "county"))
#' signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
#' create_map_or_table(signals_agg, input_example, "county_id", nuts_shp)
#' }
create_map_or_table <- function(signals_agg,
                                data_surveillance,
                                region,
                                shape = nuts_shp,
                                interactive = TRUE) {
  checkmate::assertChoice(region, region_variable_names())

  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  checkmate::assertClass(shape, "sf")
  checkmate::assertClass(data_surveillance, "data.frame")
  checkmate::assertClass(signals_agg, "data.frame")

  signals_agg <- signals_agg %>%
    dplyr::filter(category == region)

  shape <- shape %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    identity()

  region_id <- get_region_id_from_region(region)

  # checking whether plotting a map is possible
  # 1) the corresponding id column needs to be given in the dataset
  # 2) all NUTS ids need to have a match in the shapefile
  plot_map <- FALSE

  if (region_id %in% colnames(data_surveillance)) {
    region_and_region_id <- data_surveillance %>%
      dplyr::distinct(!!rlang::sym(region_id), !!rlang::sym(region)) %>%
      dplyr::rename(stratum = region)

    # we assume that each NUTS_ID is only present once, i.e. not multiple region names were used for the same NUTS_ID. This is checked in the data_checks.R.
    # another option would be that we just show a table with all the region names that were in the data when there are duplicates
    stopifnot(!any(duplicated(region_and_region_id %>% dplyr::pull(region_id))))

    # merge and replace the region by its region_id variable
    # keep the signals_agg with the region names as it is
    # if a table is shown we want to show the region names
    signals_agg_map <- signals_agg %>%
      dplyr::left_join(region_and_region_id, by = c("stratum")) %>%
      dplyr::select(stratum := !!rlang::sym(region_id), cases, any_alarms, n_alarms)

    signals_with_matching_NUTS <- signals_agg_map %>%
      dplyr::semi_join(shape, by = c("stratum" = "NUTS_ID"))

    # check whether all NUTS_ids are found in the shapefile
    n_NUTS_signals <- dplyr::n_distinct(signals_agg_map$stratum)
    n_NUTS_matching <- dplyr::n_distinct(signals_with_matching_NUTS$stratum)

    if (n_NUTS_matching == n_NUTS_signals) {
      plot_map <- TRUE
    }
  }

  if (plot_map) {
    shape_with_signals <- shape %>%
      dplyr::left_join(signals_agg_map, by = c("NUTS_ID" = "stratum")) %>%
      dplyr::filter(!is.na(cases))

    output <- plot_regional(shape_with_signals,
                            interactive = interactive
    )
  } else {
    output <- create_table(
      signals_agg %>%
        dplyr::select(-category) %>%
        convert_columns_integer(c("cases", "n_alarms")),
      interactive = interactive
    )
  }

  return(output)
}

#' Decider function to create barplot or table of aggregated cases with signals

#' Depending on the number of unique levels to visualise it is decided whether a barplot or a table is shown. The aggregated number of cases for each stratum and whether any alarm are shown.
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}}. This tibble can contain the aggregated signals for multiple categories i.e. age_group and county.
#' @param category_selected the category from the signals_agg we want to visualise
#' @param n_levels the threshold for the number of levels from which we decide when a table is generated instead of a barchart visualisation
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @returns a table or a plot depending on whether number of unique levels for the category to visualise, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "age_group"))
#' signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
#' create_barplot_or_table(signals_agg, "age_group")
#' }
create_barplot_or_table <- function(signals_agg,
                                    category_selected,
                                    n_levels = 25,
                                    interactive = TRUE) {
  signals_agg <- signals_agg %>%
    dplyr::filter(category == category_selected)

  n_levels_data <- length(unique(signals_agg$stratum))

  if (n_levels_data < n_levels) {
    plot_barchart(signals_agg, interactive = interactive)
  } else {
    create_table(
      signals_agg %>%
        dplyr::select(-category) %>%
        convert_columns_integer(c("cases", "n_alarms")),
      interactive = interactive
    )
  }
}

#' Decider function whether create_map_or_table or create_barplot_or_table is used
#'
#' Depending on the category which should be visualised (regional variable) or non regional category such as age_group, sex, ... a map is tried for plotting or a barchart.
#'
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}}. This tibble can contain the aggregated signals for multiple categories i.e. state and county.
#' @param data_surveillance data.frame, surveillance linelist
#' @param signal_category character, naming the category which should be visualised, i.e. "state","age_group","sex"
#' @return a table or a plot depending on signal_category, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
decider_barplot_map_table <- function(signals_agg,
                                      data_surveillance,
                                      signal_category,
                                      interactive = TRUE) {
  if (signal_category %in% c("state", "county", "community")) {
    plot_or_table <- create_map_or_table(signals_agg,
                                         data_surveillance,
                                         signal_category,
                                         interactive = interactive
    )
  } else {
    plot_or_table <- create_barplot_or_table(signals_agg,
                                             signal_category,
                                             interactive = interactive
    )
  }
  return(plot_or_table)
}
