#' Plot time-series based on the results of Farrington Flexible
#'
#' Static plots (default) are only based on the dates of the latest
#' `number_of_weeks` weeks. Interactive plots are based on all data, but zoom in
#' by default on the latest `number_of_weeks` weeks.
#'
#' @param results data returned by the get_signals_farringtonflexible()
#' @param interactive logical, if TRUE, interactive plot is returned; default, static plot.
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

  # Periods - ends on the first date in the following week, [start; end)
  # Dates for the latest ~year (`number_of_weeks` period).
  range_dates_year <- max(results$date) - lubridate::weeks(c(number_of_weeks, 0) - 1)

  # Static plots should be based only on the latest `number_of_weeks` weeks
  if (!interactive) {
    results <- results %>%
      dplyr::filter(date >= range_dates_year[1])
  }

  # Dates for the training period and _signal _detection _period (test data period)
  period_dates_df <- results %>% dplyr::group_by(set_status) %>%
    dplyr::summarise(start = min(date), end = max(date) + lubridate::days(7))
  # number of days in _signal _detection _period
  ndays_sdp <- dplyr::filter(period_dates_df, set_status == "Test data") %>%
    {difftime(.$end, .$start, units = "days")} %>% as.numeric()
  # Add dummy week to `results` to end the threshold line by a
  #   horizontal segment (geom_step) in the final week
  results <- results %>%
    dplyr::filter(date == max(date)) %>% # final week-date
    dplyr::mutate(cases = NA, alarms = NA, date = date + lubridate::days(7)) %>%
    dplyr::bind_rows(results, .)

  # function for finding the ymax value
  #   (plotly does not work with ymax=Inf)
  custom_round_up <- function(x, levels=c(1, 2, 5, 10)) {
    if(length(x) != 1) stop("'x' must be of length 1")

    10^floor(log10(x)) * levels[[which(x <= 10^floor(log10(x)) * levels)[[1]]]]
  }
  # local ymax for plotly zoom on x-axis
  results <- results %>%
    dplyr::mutate(ymax = purrr::map_int(
      pmax(cases * dplyr::if_else(alarms, 1.1, 1, missing = 1),
           # * 1.1 to make space for signal markers on case-bars
           upperbound, upperbound_pad,
           1, na.rm = TRUE),
      custom_round_up))
  # ymax overall for rectangle background
  ymax_data <- max(results$ymax)

  col.threshold <- "#2297E6"
  col.expected <- "#000000"
  col.alarm <- "#FF0000"
  col.training <- "#9E9E9E"
  col.test <- "#304794"

  half_week <- lubridate::days(3)

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
                       ymin = 0, ymax = custom_round_up(ymax_data),
                       colour = "white", linewidth = 0.5, alpha = 0.2) +
    ggplot2::geom_col(
      ggplot2::aes(x = date + half_week, # center bars around mid-week
                   y = cases, fill = set_status)
    ) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound, color = "Threshold"),
      linewidth = 1.3, direction = "hv"
    ) +
    ggplot2::geom_step(
      ggplot2::aes(y = upperbound_pad, color = "Threshold", linetype = "Test data"),
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
      ggplot2::aes(x = date + half_week, y = cases, shape = alarms, stroke = 1),
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
    range_dates_all <- range(results$date)
    plt <- plotly::ggplotly(plt, tooltip = "text", dynamicTicks = TRUE) %>%
      plotly::layout(
        xaxis = list(
          type = "date",
          autorange = FALSE,
          range = range_dates_year,
          rangeslider = list(
            range = range_dates_all,
            visible = TRUE,
            thickness = 0.10
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
        yaxis = list(range = c(0,
                               results %>%
                                 # pick the default x-range view
                                 dplyr::filter(date >= range_dates_year[1]) %>%
                                 dplyr::select(ymax) %>% max() )
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

    # JavaScript callback function using htmlwidgets::onRender() to listen for
    # the plotly_relayout event when x-axis range is adjusted.
    # Since it is client-side also works in HTML-reports. Two purposes:
    # 1. Dynamically adapt ymax of the interactive plot based on the x-axis zoom.
    # 2. Fix to plotly rangeselector step="all", which extends x-axis into
    ##   the future, when signal markers are present. See
    ##   https://github.com/United4Surveillance/signal-detection-tool/issues/231
    update_axes <- function(plot) {
        htmlwidgets::onRender(plot, "
          function(el, x, jsondata) {
            el.on('plotly_relayout', function(eventdata) {
              var x_autorange = eventdata['xaxis.autorange'];
              if(x_autorange === true) {
                // correct possible plotly-auto-extended x-axis
                // use a copy of full date range to avoid modification
                var data_xrange = [...jsondata['date_range']];
                Plotly.relayout(el, {'xaxis.rangeslider.range': data_xrange,
                                     'xaxis.range': data_xrange});
              }

              var x_range = eventdata['xaxis.range'];
                // undefined when x_autorange is true
              if(x_range) {
                // adapt ymax on y-axis to zoomed data
                var x_min = x_range[0];
                var x_max = x_range[1];
                var max_y_in_view =
                  Math.max.apply(null, jsondata['results'].filter(function(d) {
                      return d.date >= x_min && d.date <= x_max;
                    }).map(d => d.ymax)
                  );
                Plotly.relayout(el, {'yaxis.range': [0, max_y_in_view]});
              }
            });
          }
      ", data = list(results = dplyr::select(results, date, ymax),
                     date_range = range_dates_all)
      )
    }
    # Update the plot with dynamic y-axis adjustment and x-axis bugfix
    plt <- update_axes(plt)

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
