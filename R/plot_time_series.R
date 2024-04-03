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

  # finding number of weeks for signal detection period
  # and dates for the last year and for the signal detection period
  nweeks_sdp  <- results %>% dplyr::filter(set_status == "Test data") %>% nrow
  range_dates_year <- list(min_date = format(max(results$date) - lubridate::weeks(number_of_weeks), "%Y-%m-%d"),
                           max_date = format(max(results$date, "%Y-%m-%d")))
  range_dates_sdp  <- list(min_date = as.Date(max(results$date) - lubridate::weeks(nweeks_sdp-1)),
                      max_date = as.Date(max(results$date)))

  bgcolor_df <- data.frame(name  = c("bg_white", "bg_sdp"),
                           start = as.Date(c(min(results$date), range_dates_sdp$min_date)),
                           end   = as.Date(c(range_dates_sdp$min_date, range_dates_sdp$max_date)),
                           stringsAsFactors = FALSE)

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
    ggplot2::geom_rect(data = bgcolor_df, inherit.aes = FALSE,
                       ggplot2::aes(x = NULL, y = NULL,
                                    xmin = start, xmax = end,
                                    fill = name),
                       ymin = 0, ymax = plyr::round_any(x = max(results$cases),
                                                        f = ceiling,
                                                        accuracy = 50),
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
      breaks = scales::breaks_pretty(n = 10),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 8),
      labels = c("TRUE" = "Alarm")
    ) +
    ggplot2::scale_color_manual(values = c(
      "Expected" = col.expected,
      "Threshold" = col.threshold
    )) +
    ggplot2::scale_fill_manual(
      values = c("Test data" = col.test, "Training data" = col.training,
                 "bg_white" = "white", "bg_sdp" = "#2297E6"),
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
          range = list(
            lubridate::as_datetime(range_dates_year$min_date),
            lubridate::as_datetime(range_dates_year$max_date)
          ),
          rangeslider   = list(
            range = list(c(as.Date(range_dates_year$min_date), as.Date(range_dates_year$max_date))),
            visible = TRUE, type = "date", thickness = 0.10),
          rangeselector = list(
            buttons = list(
              list(count=nweeks_sdp * 7, label="Signal detection period", step="day", stepmode="backward"),
              list(count=1, label="1 month", step="month", stepmode="backward"),
              list(count=6, label="6 months", step="month", stepmode="backward"),
              list(count=1, label="1 year", step="year", stepmode="backward"),
              list(step="all", label = "All time points")
              )
            )
          ),
        legend = list(
          orientation = "h", x = 0.5, y = -0.9,
          yanchor = "bottom", xanchor = "center")
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
    plt$x$data[[1]]$showlegend <- FALSE
    plt$x$data[[2]]$name <- plt$x$data[[2]]$legendgroup <- "Signal detection period"
    plt$x$data[[3]]$name <- plt$x$data[[3]]$legendgroup <- "Threshold"
    plt$x$data[[4]]$showlegend <- FALSE

    if (any(!is.na(results$expected_pad))) {
      plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <- "Expected"
      plt$x$data[[6]]$showlegend <- FALSE
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Alarm"
      }
    } else {
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <- "Alarm"
      }
    }
  }
  return(plt)
}
