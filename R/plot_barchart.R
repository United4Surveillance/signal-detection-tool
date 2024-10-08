#' Barplot visualising the number of cases and information about any signals
#'
#' Bars showing number of cases and coloring around the bar showing whether any signal was generated in the last n weeks for this stratum
#'
#' @param signals_agg tibble, aggregated signals which can be obtained from using the function \code{\link{aggregate_signals}}. It contains the number of cases, any_alarms and n_alarms for one category, i.e. age group summed over the number of weeks used in \code{\link{aggregate_signals}}.
#' @param interactive boolean identifying whether the plot should be static or interactive
#' @param toggle_alarms boolean identifying whether the plot should showing number of signals explicitly or only when hovering
#' @returns either a gg or plotly object
#' @examples
#' \dontrun{
#' signals_agg_sex <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex")) %>%
#'   aggregate_signals(number_of_weeks = 12)
#' plot_barchart(signals_agg_sex)
#' }
plot_barchart <- function(signals_agg,
                          interactive = TRUE,
                          toggle_alarms = FALSE) {
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

  category <- unique(signals_agg$category)
  stopifnot(length(category) == 1)

  if (category %in% names(pretty_variable_names())) {
    x_label <- pretty_variable_names()[category]
  } else {
    x_label <- category
  }

  signals_agg <- create_factor_with_unknown(signals_agg)

  p <- ggplot2::ggplot(data = signals_agg) +
    ggplot2::geom_bar(
      stat = "identity",
      mapping = ggplot2::aes(
        x = stratum,
        y = cases,
        color = dplyr::if_else(any_alarms, "At least 1 signal", "No signals"),
        text = sprintf(
          "Number of cases: %.0f \nNumber of signals: %.0f",
          .data$cases, .data$n_alarms
        )
      ),
      fill = "#304794",
      linewidth = 1.2
    ) +
    ggplot2::labs(x = x_label, y = "Number of cases") +
    ggplot2::scale_color_manual("",
      values = c(
        "At least 1 signal" = "red",
        "No signals" = "#304794"
      )
    ) +
    ggplot2::scale_x_discrete(
      na.translate = TRUE, labels = function(x) ifelse(is.na(x), "unknown", x),
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::theme(
      legend.direction = "vertical",
      legend.position = "top",
      legend.title.align = 0.5,
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey75"),
      panel.grid.minor.y = ggplot2::element_line(colour = "grey90"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title.y = ggplot2::element_text(face = "bold")
    )


  if (!(interactive) | toggle_alarms == TRUE) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(x = stratum, y = cases, label = dplyr::if_else(any_alarms, n_alarms, NA)),
        # fill   = "red",
        color = "black",
        family = "bold",
        size = 8,
        nudge_y = (max(signals_agg$cases) / 20)
      )
  }

  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::config(modeBarButtonsToRemove = c(
        "autoScale2d",
        "select2d",
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d",
        "pan2d",
        "zoom2d",
        "toggleSpikelines"
      ))
  }
  return(p)
}
