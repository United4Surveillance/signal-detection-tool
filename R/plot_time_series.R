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
    dplyr::mutate(
      date = ISOweek::ISOweek2date(
        paste(.data$year, "-W",
          stringr::str_pad(.data$week, width = 2, pad = "0"),
          "-1",
          sep = ""
        )
      ),
      set_status = dplyr::if_else(is.na(alarms), "Training data", "Test data"),
      set_status = factor(set_status, levels = c("Training data", "Test data"))
    )

  if (!interactive) {
    results <- results %>%
      dplyr::arrange(date) %>%
      dplyr::slice_tail(n = number_of_weeks)
  }


  # Find dates for the training period and _signal _detection _period (test data)
  # and for the last ~year (`number_of_weeks` period).
  period_dates_df <- results %>% dplyr::group_by(set_status) %>%
    dplyr::summarise(start = min(date), end = max(date) + lubridate::days(7))
  range_dates_data <- with(period_dates_df, c(min(start), max(end)))
  range_dates_year <- max(period_dates_df$end) - lubridate::weeks(c(number_of_weeks, 0))
  # signal detection period in number of days
  ndays_sdp <- dplyr::filter(period_dates_df, set_status == "Test data") %>%
    {difftime(.$end, .$start, units = "days")} %>% as.numeric()

  # function for finding the ymax value
  custom_round_up <- function(x, levels=c(1, 2, 5, 10)) {
    if(length(x) != 1) stop("'x' must be of length 1")

    10^floor(log10(x)) * levels[[which(x <= 10^floor(log10(x)) * levels)[[1]]]]
  }

  col.threshold <- "#2297E6"
  col.expected <- "#000000"
  col.alarm <- "#FF0000"
  col.training <- "#9E9E9E"
  col.test <- "#304794"

  plt <-
    results %>%
    ggplot2::ggplot(ggplot2::aes(x = date, group = 1, text = paste0(
      ifelse(set_status == "Test data", "Signal detection period", ""),
      "<br>Date: ", date,
      "<br>Observed: ", cases,
      ifelse(!is.na(upperbound_pad) | !is.na(upperbound), (
        ifelse(is.na(upperbound_pad),
          paste0("<br>Threshold: ", round(upperbound, 1)),
          paste0("<br>Threshold: ", round(upperbound_pad, 1))
        )
      ), ""),
      ifelse(!is.na(expected_pad) | !is.na(expected), (
        ifelse(is.na(expected_pad),
          paste0("<br>Expected: ", round(expected, 1)),
          paste0("<br>Expected: ", round(expected_pad, 1))
        )
      ), "")
    ))) +
    ggplot2::geom_rect(data = period_dates_df, inherit.aes = FALSE,
                       ggplot2::aes(x = NULL, y = NULL,
                                    xmin = start, xmax = end,
                                    fill = paste0("bg_", set_status)),
                       ymin = 0, ymax = plyr::round_any(x = max(results$cases),
                                                        f = ceiling,
                                                        accuracy = custom_round_up(max(results$cases))),
                       colour = "white", linewidth = 0.5, alpha = 0.2) +
    ggplot2::geom_col(ggplot2::aes(y = cases, fill = set_status)) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound, color = "Threshold"),
      linewidth = 1.3, direction = "hv"
    ) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound_pad, color = "Threshold", linetype = "Test data"),
      linewidth = 0.3, direction = "hv"
    )

  if (any(!is.na(results$expected_pad))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
        linewidth = 1.3, direction = "hv"
      ) +
      ggplot2::geom_step(ggplot2::aes(y = expected_pad, color = "Expected", linetype = "Training data"),
        linewidth = 0.3, direction = "hv"
      )
  }

  # adding alarm points
  plt <- plt +
    ggplot2::geom_point(
      data = dplyr::filter(results, alarms == TRUE),
      ggplot2::aes(y = cases, shape = alarms, stroke = 1),
      color = col.alarm, size = 6
    )

  plt <- plt +
    ggplot2::scale_x_date(
      date_breaks = "month", date_labels = "%Y-%m-%d",
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = 5),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 8),
      labels = c("TRUE" = "Signal")
    ) +
    ggplot2::scale_color_manual(values = c(
      "Expected" = col.expected,
      "Threshold" = col.threshold
    )) +
    ggplot2::scale_fill_manual(
      values = c("Test data" = col.test, "Training data" = col.training,
                 "bg_Training data" = "white", "bg_Test data" = col.threshold),
      labels = c("Test data" = "Signal detection period"),
      breaks = c("Test data")
    ) +
    ggplot2::scale_linetype_manual(values = c("Training data" = 1, "Test data" = 1), name = "", guide = "none") +
    ggplot2::theme(
      legend.position = "top",
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey75"),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks.length.x = ggplot2::unit(0.25, "cm"),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, vjust = 1),
      axis.title.x = ggplot2::element_text(face = "bold", size = 14),
      axis.title.y = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2),
      shape = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::labs(
      x = "Time",
      y = "Number of infected",
      color = NULL,
      fill = NULL,
      shape = NULL
    )

  if (interactive) {
    plt <- plotly::ggplotly(plt, tooltip = "text", dynamicTicks = TRUE) %>%
      plotly::layout(
        xaxis = list(
          type = "date",
          autorange = FALSE,
          range = range_dates_year,
          rangeslider = list(
            range = range_dates_data,
            visible = TRUE, type = "date", thickness = 0.10
          ),
          rangeselector = list(
            buttons = list(
              list(count=ndays_sdp, label="Signal detection period", step="day", stepmode="backward"),
              list(count=1, label="1 month", step="month", stepmode="backward"),
              list(count=6, label="6 months", step="month", stepmode="backward"),
              list(count=1, label="1 year", step="year", stepmode="backward"),
              list(step="all", label = "All time points")
              )
            )
          ),
        legend = list(
          orientation = "h", x = 0.5, y = -0.9,
          yanchor = "top", xanchor = "center")
      ) %>%
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

    # modifying the interactive plot legend
    plt$x$data[[1]]$showlegend <-
      plt$x$data[[2]]$showlegend <- FALSE
    plt$x$data[[1]]$hoverinfo  <-
      plt$x$data[[2]]$hoverinfo  <- 'skip'
    plt$x$data[[3]]$showlegend <- FALSE
    plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <- "Signal detection period"
    plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <- "Threshold"
    plt$x$data[[6]]$showlegend <- FALSE

    if (any(!is.na(results$expected_pad))) {
      plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Expected"
      plt$x$data[[8]]$showlegend <- FALSE
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[9]]$name <- plt$x$data[[9]]$legendgroup <- "Signal"
      }
    } else {
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Signal"
      }
    }
  }
  return(plt)
}
