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
    dplyr::mutate(n_alarms_label = dplyr::if_else(n_alarms > 0, n_alarms, NA)) %>%
    ggplot2::ggplot(ggplot2::aes(fill = cases,
                                 text = paste0("Number of cases: ", cases,
                                               "<br>Number of alarms: ", n_alarms))) +
    ggplot2::geom_sf(mapping = ggplot2::aes(colour = "No alarms"), lwd = 1.2) +
    ggplot2::geom_sf(data = . %>% dplyr::filter(any_alarms == TRUE),
                     mapping = ggplot2::aes(colour = "At least 1 alarm"), lwd = 1.2) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_gradientn(colours = colorRampPalette(c("#eaecf4","#304794","#1c2a58"))(8),
                                  name = "Cases") +
    ggplot2::scale_color_manual(values = c("No alarms"        = "black",
                                           "At least 1 alarm" = "red"),
                                limits = c("No alarms", "At least 1 alarm"),
                                labels = c("No alarms", "At least 1 alarm"),
                                name = "") +
    ggplot2::scale_size_identity() +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 20, family = "bold"),
      legend.text  = ggplot2::element_text(size = 15),
      legend.text.align = 0
    )


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
      )) %>%
      plotly::style(hoveron = "fill")

    return(plot)
  }

  plot <- plot + ggplot2::geom_sf_text(ggplot2::aes(label = n_alarms_label),
                                       color  = "black",
                                       family = "bold",
                                       size   = 8,
                                       na.rm  = TRUE
                                       )

  plot
}
