
#' Plot time-series based on the results of Farrington Flexible
#'
#' @param results data returned by the get_signals_farringtonflexible()
#' @param interactive if TRUE, interactive plot is returned
#' @param number_of_weeks number of weeks to be covered in the plot
#'
#' @return either a gg or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read.csv("data/input/input.csv", header = TRUE, sep = ",")
#' results <- get_signals_farringtonflexible(data)
#' plot_time_series(results)
#' }
plot_time_series <- function(results, interactive = FALSE,
                             number_of_weeks = 52) {

  results <- results %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      date = ISOweek::ISOweek2date(
        paste(.data$year, "-W",
              stringr::str_pad(.data$week, width = 2, pad = "0"),
              "-1", sep = "")),
      set_status = dplyr::if_else(is.na(alarms), "Training data", "Test data")) %>%
    dplyr::arrange(date) %>%
    dplyr::slice_tail(n = number_of_weeks)

  col.threshold <- "#2297E6"
  col.expected  <- "#000000"
  col.alarm     <- "#DF536B"
  col.training  <- "#9E9E9E"
  col.test      <- "#304794"

  plot_text <- paste0(
    results$set_status,
    "<br>Date: ", results$date, "<br>Observed: ", results$cases,
    "<br>Threshold: ", round(results$upperbound_pad)
  )

  # if any expected values not NA, then `farrington` is method chosen
  if (any(!is.na(results$expected_pad))) {
    plot_text <- paste0(plot_text,
                        "<br>Expected: ", round(results$expected_pad))
  }


  plt <-
    results %>%
    ggplot2::ggplot(ggplot2::aes(x = date, group = 1, text = paste0(
      set_status,
      "<br>Date: ", date, "<br>Observed: ", cases,
      "<br>Threshold: ", round(upperbound),
      "<br>Expected: ", round(expected)
    ))) +
    ggplot2::geom_col(ggplot2::aes(y = cases, fill = set_status)) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound, color = "Threshold"),
                       linewidth = 1, direction = "hv") +
    ggplot2::geom_step(ggplot2::aes(y = upperbound_pad, color = "Threshold", linetype = "Test data"),
                       linewidth = 0.3, direction = "hv") +
    ggplot2::geom_point(data = dplyr::filter(results, alarms == TRUE),
                        ggplot2::aes(y = cases, shape = alarms),
                        color = col.alarm, size = 4)

  if (any(!is.na(results$expected_pad))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
                         linewidth = 1, direction = "hv") +
      ggplot2::geom_step(ggplot2::aes(y = expected_pad, color = "Expected", linetype = "Training data"),
                         linewidth = 0.3, direction = "hv")
  }

  plt <- plt +
    ggplot2::scale_x_date(breaks = scales::breaks_pretty(n = 8),
                          date_labels = "%Y-%d-%m") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                                expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 8),
                                labels = c("TRUE" = "Alarm")) +
    ggplot2::scale_color_manual(values = c("Expected" = col.expected,
                                           "Threshold" = col.threshold)) +
    ggplot2::scale_fill_manual(values = c("Test data" = col.test, "Training data" = col.training),
                               labels = c("Test data", "Training data")) +
    ggplot2::scale_linetype_manual(values = c("Training data" = 1, "Test data" = 1), name = "", guide = "none") +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.background = ggplot2::element_blank(),
      legend.key  = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),

      panel.background   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey75"),
      panel.grid.minor.y = ggplot2::element_blank(),

      axis.line  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(face = "bold", size = 14),
      axis.title.y = ggplot2::element_text(face = "bold", size = 14)) +
    ggplot2::guides(fill  = ggplot2::guide_legend(order = 1),
                    color = ggplot2::guide_legend(order = 2),
                    shape = ggplot2::guide_legend(order = 3)) +
    ggplot2::labs(x = "Time",
                  y = "Number of infected",
                  color = NULL,
                  fill  = NULL,
                  shape = NULL)

  if (interactive) {
    plt <- plotly::ggplotly(plt, tooltip = "text") %>%
      plotly::layout(legend = list(orientation = "h", x = 0.3, y = -0.1)) %>%
      plotly::style(hoverlabel = list(bgcolor = "white")) %>%
      plotly::config(modeBarButtonsToRemove = c('autoScale2d',
                                                # 'resetScale2d',
                                                'select2d',
                                                'lasso2d',
                                                'zoomIn2d',
                                                'zoomOut2d',
                                                'pan2d',
                                                'zoom2d',
                                                'toggleSpikelines'))

  }

  return(plt)


}
