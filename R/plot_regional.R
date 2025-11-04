#' Plot number of cases with number of signals by region
#' @param shape_with_signals sf shapefile, with additional columns from signals cases, n_alarms, any_alarms
#' @param signals_agg_unknown_region tibble default NULL, if not NULL tibble containing only the row for signals_agg for the missing regions (is.na(stratum)) with the columns cases and n_alarms which are used for creating the annotation text below the map
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @param partial logical, add partial bundle to plotly
#' @returns either a ggplot object if static plot is chosen or a plotly object for the interactive plot
plot_regional <- function(shape_with_signals,
                          signals_agg_unknown_region = NULL,
                          interactive = FALSE,
                          toggle_alarms = FALSE,
                          partial = FALSE) {
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
                                  , .default = "No signals"),
      any_alarms = factor(any_alarms, levels = c("No signals", "At least 1 signal")) # level ordering determines render ordering: black < red
    )

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
    shape_areas_sf <- shape_with_signals %>%
      dplyr::filter(!sf::st_is_empty(geometry)) %>%
      sf::st_make_valid()

    plot <- plotly::plotly_empty() %>%
      plotly::add_sf( # add geometries and colours by cases
        type = "scatter",
        data = shape_areas_sf,
        split = ~NUTS_ID,
        color = ~cases,
        colors = grDevices::colorRampPalette(c("#eaecf4", "#304794", "#1c2a58"))(8),
        stroke = I("black"),
        alpha = 1,
        text = ~ paste(NUTS_NAME,
                       "\nNumber of cases:", cases,
                       "\nNumber of signals:", n_alarms),
        hoverinfo = "text",
        hoveron = "fills",
        hoverlabel = list(bgcolor = "white"),
        showlegend = FALSE,
        inherit = FALSE
      )

    stars_sf <- shape_with_signals |>
      dplyr::filter(any_alarms == "At least 1 signal")
    nrow_stars_before <- nrow(stars_sf)

    if (nrow(stars_sf) > 0) {
      #  temporarily disable s2: can help avoid spherical edge cases at coasts
      old_s2 <- sf::sf_use_s2()
      sf::sf_use_s2(FALSE)

      # calculate centre robustly
      stars_sf <- stars_sf |>
        sf::st_make_valid() |>
        sf::st_centroid(of_largest_polygon = TRUE) |>
        sf::st_collection_extract("POINT") |>    # only keep points
        dplyr::filter(!sf::st_is_empty(geometry))  # remove empty ones

      sf::sf_use_s2(old_s2)

      if(nrow(stars_sf) < nrow_stars_before){
        stop("Empty geometry would silently remove signal marker(s).
             Please fix your supplied shapefile.")
      }

      # explicitly add points
      coords <- sf::st_coordinates(stars_sf)
      plot <- plot %>%
        plotly::add_markers(
          x = coords[,1], y = coords[,2],
          marker = list(symbol = "star", size = 10, color = "red"),
          text = paste(stars_sf$NUTS_NAME, "\nNumber of cases:", stars_sf$cases,
                       "\nNumber of signals:", stars_sf$n_alarms),
          hoverinfo = "text",
          showlegend = FALSE,
          inherit = FALSE
        )
    }

    if (!is.null(text_region_missing)) {
      plot <- plot %>%
        plotly::layout(annotations = list(
          text = text_region_missing,
          x = 0.5, y = 0,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          align = "center"
        ))
    }
    plot <- plot %>%
      plotly::layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
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

    if (partial) {
      plot <- plotly::partial_bundle(plot)
    }
  }

  plot
}
