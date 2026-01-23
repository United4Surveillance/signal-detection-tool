# functions for helping to decide whether a visualisation in form of a map or a bar graph are shown or a table is shown


#' Decider for creating a map or a table based on whether all NUTS_ids are found in the shapefile
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}}. This tibble can contain the aggregated signals for multiple categories i.e. state and county.
#' @param data_surveillance data.frame, surveillance linelist
#' @param region character, specifying the variable for the region to be shown should be one of ("country","state",
#' "county","community","region_level1", "region_level2","region_level3")
#' @param shape sf. The shapefile used for mapping.
#'   - If an external shapefile path is provided in the configuration file, it is used.
#'   - Otherwise, the function defaults to the internal European shapefile (`nuts_shp`).
#'   - By default, this parameter is set to `get_shp_config_or_internal()`, which handles this logic dynamically.
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @returns a table or a plot depending on whether the matching of the NUTS IDs was fully possible, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "county"))
#' signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
#' create_map_or_table(signals_agg, input_example, "county", nuts_shp)
#' }
create_map_or_table <- function(signals_agg,
                                data_surveillance,
                                region,
                                shape = get_shp_config_or_internal(),
                                interactive = TRUE,
                                toggle_alarms = FALSE) {
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
    # when there is a mismatch we generate a warning
    missing <- setdiff(setdiff(signals_agg_map$stratum, NA), signals_with_matching_NUTS$stratum)

    if (length(missing) > 0) {
      warning(
        sprintf(
          "Possible signals are not shown for these regions, as they didn't match the shapefile: %s",
          paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (nrow(signals_with_matching_NUTS) > 0) {
      plot_map <- TRUE
    }
  }

  if (plot_map) {
    # filter shapefile according to level selected
    level_code <- shape %>%
      dplyr::left_join(signals_agg_map, by = c("NUTS_ID" = "stratum")) %>%
      dplyr::filter(!is.na(cases)) %>%
      dplyr::distinct(LEVL_CODE) %>%
      dplyr::pull()

    shape_with_signals <- shape %>%
      dplyr::filter(
        .data$CNTR_CODE %in% unique(data_surveillance$country_id),
        .data$LEVL_CODE == level_code
      ) %>%
      dplyr::left_join(signals_agg_map, by = c("NUTS_ID" = "stratum")) %>%
      dplyr::mutate(
        cases = dplyr::case_when(is.na(.data$cases) ~ 0, .default = .data$cases),
        any_alarms = dplyr::case_when(is.na(.data$any_alarms) ~ FALSE, .default = .data$any_alarms),
        n_alarms = dplyr::case_when(is.na(.data$n_alarms) ~ 0, .default = .data$n_alarms),
      )

    # computation of tibble for the information about the cases with NA region
    # only show the unknown information when there were more than 0 cases
    # NA can still be there even when cases = 0 because when it was there in general in the dataset it is added as level
    signals_agg_unknown_region <- signals_agg_map %>%
      dplyr::filter(is.na(stratum) & cases > 0)

    output <- plot_regional(shape_with_signals,
      signals_agg_unknown_region,
      interactive = interactive,
      toggle_alarms = toggle_alarms
    )
  } else {
    format <- ifelse(interactive, "DataTable", "Flextable")
    output <- build_signals_agg_table(
      signals_agg,
      format = format
    )
  }

  return(output)
}

#' Decider function to create barplot or table of aggregated cases with signals

#' Depending on the number of unique levels to visualise it is decided whether a barplot or a table is shown. The aggregated number of cases for each stratum and whether any signal are shown.
#' @param signals_agg tibble, aggregated signals over n weeks with columns number of cases, any_alarms and n_alarms \code{\link{aggregate_signals}}. This tibble can contain the aggregated signals for multiple categories i.e. age_group and county.
#' @param category_selected the category from the signals_agg we want to visualise
#' @param n_levels the threshold for the number of levels from which we decide when a table is generated instead of a barchart visualisation
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
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
                                    interactive = TRUE,
                                    toggle_alarms = FALSE) {
  signals_agg <- signals_agg %>%
    dplyr::filter(category == category_selected)

  n_levels_data <- length(unique(signals_agg$stratum))

  if (n_levels_data < n_levels) {
    plot_barchart(signals_agg,
      interactive = interactive, toggle_alarms = toggle_alarms
    )
  } else {
    format <- ifelse(interactive, "DataTable", "Flextable")
    build_signals_agg_table(
      signals_agg,
      format = format
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
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @return a table or a plot depending on signal_category, the table and plots can be interactive or not depening on the interactive parameter, can be class "ggplot" or "plotly" for plot and class "gt_tbl" or "datatables" for table
decider_barplot_map_table <- function(signals_agg,
                                      data_surveillance,
                                      signal_category,
                                      interactive = TRUE,
                                      toggle_alarms = FALSE) {
  if (signal_category %in% region_variable_names()) {
    plot_or_table <- create_map_or_table(
      signals_agg,
      data_surveillance,
      signal_category,
      interactive = interactive,
      toggle_alarms = toggle_alarms
    )
  } else {
    plot_or_table <- create_barplot_or_table(
      signals_agg,
      signal_category,
      interactive = interactive,
      toggle_alarms = toggle_alarms
    )
  }
  return(plot_or_table)
}
