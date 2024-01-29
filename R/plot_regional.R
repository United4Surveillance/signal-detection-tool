#' Plot number of cases with number of signals by region
#' @param shape_with_signals sf shapefile, with additional columns from signals cases, n_alarms, any_alarms
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @returns either a ggplot object if static plot is chosen or a plotly object for the interactive plot
plot_regional <- function(shape_with_signals,
                          interactive = FALSE) {
  checkmate::assertClass(shape_with_signals, "sf")

  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )

  plot <- shape_with_signals %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$cases, color = .data$n_alarms)) +
    ggplot2::geom_sf(
      ggplot2::aes(
        size = 2 * .data$n_alarms,
        text = paste0("cases: ", .data$cases, " , alarms: ", .data$n_alarms),
        data_id = .data$NUTS_ID
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_distiller(palette = "YlGn", direction = 1) +
    ggplot2::scale_color_distiller(palette = "Reds", direction = 1) +
    ggplot2::scale_size_identity()

  if (interactive) {
    plot <- plotly::ggplotly(plot, tooltip = "text") %>%
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

    return(plot)
  }

  plot <- plot + ggplot2::geom_sf_text(ggplot2::aes(label = .data$n_alarms),
    color = "red"
  )

  plot
}
