
#' Plot surveillance data and signal detection results by region
#'
#' @param data data.frame containing surveillance data in linelist format
#' @param signals data.frame containing signal detection output stratified by
#'   regions
#' @param shape data.frame with geometry column in sf format
#' @param country_id two-letter country-code for the country that should be
#'   plotted
#' @param regional_level character describing which level of aggregation should
#'   be shown ("country", "state","county" or "community")
#' @param interactive boolean identifying whether the plot should be static or
#'   interactive
#'
#' @return either a ggplot object if static plot is chosen or a ggiraph object
#'   for the interactive plot
#' @export
#'
#' @examples
#' \dontrun{
#' plot_regional(data,
#'               signals_county,
#'               shape,
#'               country_id = country, regional_level = "county")
#' }
plot_regional <- function(data,
                          signals,
                          shape,
                          country_id = "AT",
                          regional_level = "county",
                          interactive = FALSE){

  checkmate::assertChoice(regional_level, c("country", "state","county", "community"))

  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  checkmate::assertClass(shape, "sf")
  checkmate::assertClass(data, "data.frame")
  checkmate::assertClass(signals, "data.frame")

  shape <- shape %>% dplyr::filter(.data$CNTR_CODE == country_id)
  shape <- switch(regional_level,
                  country = shape %>% dplyr::filter(.data$LEVL_CODE == 0),
                  state = shape %>% dplyr::filter(.data$LEVL_CODE == 1),
                  county = shape %>% dplyr::filter(.data$LEVL_CODE == 2),
                  community = shape %>% dplyr::filter(.data$LEVL_CODE == 3),
                  shape
  )
  data_reg <- switch(regional_level,
                     country = data %>%
                       dplyr::group_by(.data$country_id) %>%
                       plyr::summarise(count = dplyr::n()) %>%
                       dplyr::rename("region_id" = .data$country_id),
                     state = data %>%
                       dplyr::group_by(.data$state_id) %>%
                       dplyr::summarise(count = dplyr::n()) %>%
                       dplyr::rename("region_id" = .data$state_id),
                     county = data %>%
                       dplyr::group_by(.data$county_id) %>%
                       dplyr::summarise(count = dplyr::n()) %>%
                       dplyr::rename("region_id" = .data$county_id),
                     community = data %>%
                       dplyr::group_by(.data$community_id) %>%
                       dplyr::summarise(count = dplyr::n()) %>%
                       dplyr::rename("region_id" = .data$community_id),
                     data
  )

  signals <- signals %>%
    dplyr::group_by(.data$stratum) %>%
    dplyr::summarise("alarms" = sum(.data$alarms, na.rm = T)) %>%
    dplyr::filter(.data$alarms > 1)

  data_reg <- dplyr::left_join(data_reg, signals, by = c("region_id" = "stratum"))

  shape <- dplyr::left_join(shape, data_reg, by = c("NUTS_ID" = "region_id"))

  plot <- shape %>%
    dplyr::filter(!is.na(.data$count)) %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$count, color = .data$alarms)) +
    ggiraph::geom_sf_interactive(
      ggplot2::aes(size = 2*.data$alarms,
                   tooltip = paste0("cases: ", .data$count," alarms: ", .data$alarms),
                   data_id = .data$NUTS_ID)) +
    ggplot2::theme_void() +
    ggiraph::scale_fill_distiller_interactive(palette = "YlGn", direction = 1) +
    ggiraph::scale_color_distiller_interactive(palette = "Reds", direction = 1) +
    ggplot2::scale_size_identity()

  if (interactive) {
    return(ggiraph::girafe(ggobj = plot))
  }

  plot <- plot + ggplot2::geom_sf_text(ggplot2::aes(label = .data$alarms),
                                       color = "red")

  plot

}

