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
  shape_with_signals <- shape_with_signals %>%
    # make case numbers unique to keep plotly from combining the traces!
    # as long as the perturbation is small enough the fill color doesn't change
    # (at least noticeably) and as long as there aren't enough regional units
    # with equal cases the displayed number of cases isn't changed by rounding
    dplyr::group_by(cases) %>%
    dplyr::mutate(cases = cases + dplyr::row_number() * 1e-5) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_alarms_label = dplyr::if_else(n_alarms > 0, n_alarms, NA),
      any_alarms = dplyr::if_else(any_alarms, "At least 1 alarm", "No alarms"),
      any_alarms = factor(any_alarms, levels = c("No alarms", "At least 1 alarm")) # level ordering determines render ordering: black < red
    )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = shape_with_signals,
      mapping = ggplot2::aes(
        fill = cases,
        colour = any_alarms,
        text = paste0(
          NUTS_NAME,
          "<br>Number of cases: ", round(cases, 0), # show actual case numbers
          "<br>Number of alarms: ", n_alarms
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
        "No alarms" = "black",
        "At least 1 alarm" = "red"
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

  # removing fill-legend if entire map is 1 zone, to avoid legend issues.
  if (nrow(shape_with_signals) < 2) {
    plot <- plot +
      ggplot2::guides(fill = "none")
  }

  if (interactive) {

    plot <- plotly::ggplotly(plot, tooltip = "text") %>%
      plotly::style(
        hoveron = "fill",
        hoverlabel = list(bgcolor = "white")
      ) %>%
      plotly::config(modeBarButtonsToRemove = c(
        # "autoScale2d",
        "resetScale2d",
        "select2d",
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d",
        "pan2d",
        "zoom2d",
        "toggleSpikelines"
      ))
  } else {
    plot <- plot + ggplot2::geom_sf_text(
      ggplot2::aes(label = n_alarms_label),
      color = "black",
      family = "bold",
      size = 8,
      na.rm = TRUE
    )
  }

  plot
}



# plot_regional <- function(shape_with_signals,
#                           interactive = FALSE) {
#   checkmate::assertClass(shape_with_signals, "sf")
#
#   checkmate::assert(
#     checkmate::check_true(interactive),
#     checkmate::check_false(interactive),
#     combine = "or"
#   )
#
#   plot <- shape_with_signals %>%
#     dplyr::mutate(n_alarms_label = dplyr::if_else(n_alarms > 0, n_alarms, NA)) %>%
#     ggplot2::ggplot(ggplot2::aes(fill = cases,
#                                  text = paste0(NUTS_NAME,
#                                                "<br>Number of cases: ", cases,
#                                                "<br>Number of alarms: ", n_alarms))) +
#     ggplot2::geom_sf(mapping = ggplot2::aes(colour = "No alarms"), lwd = 1.2) +
#     ggplot2::theme_void() +
#     ggplot2::scale_fill_gradientn(colours = grDevices::colorRampPalette(c("#eaecf4","#304794","#1c2a58"))(8),
#                                   name = "Cases") +
#     ggplot2::scale_color_manual(values = c("No alarms"        = "black",
#                                            "At least 1 alarm" = "red"),
#                                 limits = c("No alarms", "At least 1 alarm"),
#                                 labels = c("No alarms", "At least 1 alarm"),
#                                 name = "") +
#     ggplot2::scale_size_identity() +
#     ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA))) +
#     ggplot2::theme(
#       legend.title = ggplot2::element_text(size = 20, family = "bold"),
#       legend.text  = ggplot2::element_text(size = 15),
#       legend.text.align = 0
#     )
#
#   if (any(shape_with_signals$any_alarms == TRUE)) {
#     plot <- plot +
#       ggplot2::geom_sf(data = shape_with_signals %>% dplyr::filter(any_alarms == TRUE),
#                        mapping = ggplot2::aes(colour = "At least 1 alarm"), lwd = 1.2)
#   }
#
#
#   if (interactive) {
#     plot <- plotly::ggplotly(plot, tooltip = "text") %>%
#       plotly::style(hoveron = "fill",
#                     hoverlabel = list(bgcolor = "white")) %>%
#       plotly::config(modeBarButtonsToRemove = c(
#         "autoScale2d",
#         "resetScale2d",
#         "select2d",
#         "lasso2d",
#         "zoomIn2d",
#         "zoomOut2d",
#         "pan2d",
#         "zoom2d",
#         "toggleSpikelines"
#       ))
#
#
#   } else {
#     plot <- plot + ggplot2::geom_sf_text(ggplot2::aes(label = n_alarms_label),
#                                          color  = "black",
#                                          family = "bold",
#                                          size   = 8,
#                                          na.rm  = TRUE
#                                          )
#   }
#
#
#   plot
# }
