#' Plot number of cases with number of signals by region
#' @param shape_with_signals sf shapefile, with additional columns from signals cases, n_alarms, any_alarms
#' @param signals_agg_unknown_region tibble default NULL, if not NULL tibble containing only the row for signals_agg for the missing regions (is.na(stratum)) with the columns cases and n_alarms which are used for creating the annotation text below the map
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @returns either a ggplot object if static plot is chosen or a plotly object for the interactive plot
#' @export
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' signals <- data_preprocessed %>% get_signals(stratification = c("county"))
#' signals_agg <- signals %>% aggregate_signals(number_of_weeks = 6)
#' nuts_shp <- nuts_shp %>%
#'   sf::st_as_sf() %>%
#'   dplyr::filter(LEVL_CODE == 2 & CNTR_CODE == "AT") %>%
#'   dplyr::select(NUTS_NAME, geometry)
#' shape_with_signals <- nuts_shp %>%
#'   dplyr::inner_join(
#'     signals_agg,
#'     by = c("NUTS_NAME" = "stratum")
#'   ) %>%
#'   sf::st_as_sf()
#' signals_map <- plot_regional(
#'   shape_with_signals,
#'   signals_agg_unknown_region = data.frame(),
#'   interactive = FALSE,
#'   toggle_alarms = FALSE
#' )
#' signals_map
#' }
plot_regional <- function(shape_with_signals,
                          signals_agg_unknown_region = NULL,
                          interactive = FALSE,
                          toggle_alarms = FALSE) {
  checkmate::assertClass(shape_with_signals, "sf")

  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_true(toggle_alarms),
    checkmate::check_false(toggle_alarms),
    combine = "or"
  )

  shape_with_signals <- shape_with_signals %>%
    dplyr::mutate(
      n_alarms_label = dplyr::if_else(n_alarms > 0, n_alarms, NA),
      any_alarms = dplyr::if_else(any_alarms, "At least 1 signal", "No signals",
        missing = "No signals"
      ),
      any_alarms = factor(any_alarms, levels = c("No signals", "At least 1 signal")) # level ordering determines render ordering: black < red
    )

  shape_with_signals <- shape_with_signals %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    sf::st_make_valid()

  # Plotly uses Cartesian axes here. For lon/lat data, transform to a metric CRS first.
  # EPSG:3035 = ETRS89 / LAEA Europe, suitable for European NUTS maps.
  if (isTRUE(sf::st_is_longlat(shape_with_signals))) {
    shape_with_signals <- sf::st_transform(shape_with_signals, 3035)
  }

  lower_th <- ceiling(max(shape_with_signals$cases) * 0.40)
  col_alarm_text <- shape_with_signals %>%
    dplyr::mutate(col_var = dplyr::case_when(
      cases <= lower_th ~ "black",
      cases > lower_th ~ "white"
    )) %>%
    dplyr::pull(col_var)

  plot <- ggplot2::ggplot(data = shape_with_signals) +
    ggplot2::geom_sf(
      data = shape_with_signals,
      mapping = ggplot2::aes(
        fill = cases,
        colour = any_alarms,
        text = paste0(
          NUTS_NAME,
          "<br>Number of cases: ", round(cases, 0), # show actual case numbers
          "<br>Number of signals: ", n_alarms
        )
      ),
      lwd = 1.2
    ) +
    ggplot2::coord_sf() +
    ggplot2::theme_void() +
    ggplot2::scale_fill_gradientn(
      colours = grDevices::colorRampPalette(c("#eaecf4", "#304794", "#1c2a58"))(8),
      name = "Cases",
      labels = function(x) round(x, 0) # show actual case numbers
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "No signals" = "black",
        "At least 1 signal" = "red"
      ),
      name = ""
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 20, family = "bold"),
      legend.text = ggplot2::element_text(size = 15),
      legend.text.align = 0
    )

  # creating text if cases missing region > 0
  text_region_missing <- NULL
  if (!is.null(signals_agg_unknown_region) & nrow(signals_agg_unknown_region) > 0) {
    text_region_missing <- paste0(
      signals_agg_unknown_region$cases, " case",
      ifelse(signals_agg_unknown_region$cases > 1, "s ", " "),
      "from unknown region with \n", signals_agg_unknown_region$n_alarms,
      " signal",
      ifelse(signals_agg_unknown_region$n_alarms > 1, "s", ""), ".\n"
    )
    if (!interactive) {
      plot <- plot +
        ggplot2::labs(caption = text_region_missing) +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 14, hjust = 0.5))
    }
  }

  # removing fill-legend if entire map is 1 zone, to avoid legend issues.
  if (nrow(shape_with_signals) < 2) {
    plot <- plot +
      ggplot2::guides(fill = "none")
  }

  if (!(interactive) | toggle_alarms == TRUE) {
    plot <- plot + ggplot2::geom_sf_text(
      ggplot2::aes(label = n_alarms_label),
      color = col_alarm_text,
      fontface = "bold",
      size = 8,
      na.rm = TRUE
    )
  }

  if (interactive) {
    if (is.na(sf::st_crs(shape_with_signals))) {
      stop("shape_with_signals needs a valid CRS for interactive map plotting.")
    }

    shape_areas_sf <- shape_with_signals %>%
      dplyr::filter(!sf::st_is_empty(geometry)) %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON", warn = FALSE) %>%
      sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
      sf::st_zm(drop = TRUE, what = "ZM") %>%
      sf::st_transform(4326) %>%
      dplyr::mutate(
        NUTS_ID = as.character(NUTS_ID),
        hover_text = paste0(
          NUTS_NAME,
          "<br>Number of cases: ", round(cases, 0),
          "<br>Number of signals: ", n_alarms
        )
      )

    if (any(!sf::st_is_valid(shape_areas_sf))) {
      stop("Invalid geometries remain after st_make_valid().")
    }

    if (anyDuplicated(shape_areas_sf$NUTS_ID)) {
      stop("NUTS_ID must be unique for the choropleth GeoJSON mapping.")
    }

    geojson_text <- geojsonsf::sf_geojson(
      shape_areas_sf %>%
        dplyr::select(NUTS_ID, NUTS_NAME),
      atomise = FALSE
    )

    geojson <- jsonlite::fromJSON(
      geojson_text,
      simplifyVector = FALSE
    )

    plot <- plotly::plot_ly(
      data = shape_areas_sf,
      type = "choropleth",
      geojson = geojson,
      locations = ~NUTS_ID,
      featureidkey = "properties.NUTS_ID",
      z = ~cases,
      text = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      colorscale = list(
        list(0.00, "#eaecf4"),
        list(0.50, "#304794"),
        list(1.00, "#1c2a58")
      ),
      marker = list(
        line = list(
          color = "black",
          width = 1
        )
      ),
      colorbar = list(
        title = "Cases",
        tickformat = ".0f"
      ),
      showscale = nrow(shape_areas_sf) >= 2
    ) %>%
      plotly::layout(
        geo = list(
          projection = list(type = "mercator"),
          fitbounds = "geojson",
          visible = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          showcountries = FALSE,
          showland = FALSE,
          showlakes = FALSE,
          bgcolor = "rgba(0,0,0,0)"
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )

    stars_sf <- shape_areas_sf %>%
      dplyr::filter(any_alarms == "At least 1 signal")

    if (nrow(stars_sf) > 0) {
      stars_points_sf <- stars_sf %>%
        sf::st_transform(3035) %>%
        sf::st_point_on_surface() %>%
        sf::st_transform(4326)

      coords <- sf::st_coordinates(stars_points_sf)

      stars_points_df <- stars_points_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          lon = coords[, "X"],
          lat = coords[, "Y"]
        )

      plot <- plot %>%
        plotly::add_trace(
          data = stars_points_df,
          type = "scattergeo",
          mode = "markers",
          lon = ~lon,
          lat = ~lat,
          marker = list(
            symbol = "star",
            size = 10,
            color = "red"
          ),
          text = ~hover_text,
          hovertemplate = "%{text}<extra></extra>",
          showlegend = FALSE,
          inherit = FALSE
        )
    }

    if (!is.null(text_region_missing)) {
      plot <- plot %>%
        plotly::layout(
          annotations = list(
            text = text_region_missing,
            x = 0.5,
            y = 0,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            align = "center"
          )
        )
    }

    plot <- plot %>%
      plotly::config(modeBarButtonsToRemove = c(
        "autoScale2d",
        "resetScale2d",
        "select2d",
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d",
        "pan2d",
        "zoom2d",
        "toggleSpikelines"
      ))
  }

  plot
}
