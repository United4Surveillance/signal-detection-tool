#' Plot number of cases with number of signals by region
#' @param shape_with_signals sf shapefile, with additional columns from signals cases, n_alarms, any_alarms
#' @param signals_agg_unknown_region tibble default NULL, if not NULL tibble containing only the row for signals_agg for the missing regions (is.na(stratum)) with the columns cases and n_alarms which are used for creating the annotation text below the map
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @returns either a ggplot object if static plot is chosen or a plotly object for the interactive plot
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
    # make case numbers unique to keep plotly from combining the traces!
    # as long as the perturbation is small enough the fill color doesn't change
    # (at least noticeably) and as long as there aren't enough regional units
    # with equal cases the displayed number of cases isn't changed by rounding
    dplyr::group_by(cases) %>%
    dplyr::mutate(cases = cases + dplyr::row_number() * 1e-5) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_alarms_label = dplyr::if_else(n_alarms > 0, n_alarms, NA),
      any_alarms = dplyr::if_else(any_alarms, "At least 1 signal", "No signals"),
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
  if(!is.null(signals_agg_unknown_region) & nrow(signals_agg_unknown_region) > 0){
    text_region_missing <- paste0(signals_agg_unknown_region$cases, " case",
                                  ifelse(signals_agg_unknown_region$cases > 1,"s "," "),
                                  "from unknown region with \n",signals_agg_unknown_region$n_alarms,
                                  " signal",
                                  ifelse(signals_agg_unknown_region$n_alarms > 1,"s",""),".\n")
    if(!interactive){
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
      family = "bold",
      size = 8,
      na.rm = TRUE
    )
  }

  if (interactive) {
    plot <- plotly::ggplotly(plot, tooltip = "text")

    if(!is.null(text_region_missing)){
      plot <- plot %>%
        plotly::layout(annotations = list(text = text_region_missing,
                                          x = 0.5, y = 0,
                                          xref = "paper", yref = "paper",
                                          showarrow = FALSE,
                                          align = "center"))
    }
    plot <- plot %>%
      plotly::style(
        hoveron = "fill",
        hoverlabel = list(bgcolor = "white")
      ) %>%
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
