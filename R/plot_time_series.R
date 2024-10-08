#' Plot time-series based on the results of a signal detection algorithm, being alarms, threshold and expectation
#'
#' Static plots (default) are only based on the dates of the latest
#' `number_of_weeks` weeks. Interactive plots are based on all data, but zoom in
#' by default on the latest `number_of_weeks` weeks.
#'
#' @param results data returned by the get_signals_farringtonflexible()
#' @param interactive logical, if TRUE, interactive plot is returned; default, static plot.
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention in the pandemic correction models. Default is NULL which indicates that no intervention is done.The  intervention is marked with a dashed line.
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
                             intervention_date = NULL,
                             number_of_weeks = 52) {
  # check whether timeseries contains padding or not
  padding_upperbound <- "upperbound_pad" %in% colnames(results)
  padding_expected <- "expected_pad" %in% colnames(results)
  padding <- any(padding_expected, padding_upperbound)

  results <- results %>%
    dplyr::mutate(
      isoweek = paste0(
        .data$year, "-W",
        stringr::str_pad(.data$week, width = 2, pad = "0")
      ),
      date = ISOweek::ISOweek2date(paste0(.data$isoweek, "-1")),
      set_status = dplyr::if_else(is.na(.data$alarms), "Training data", "Test data"),
      set_status = factor(.data$set_status, levels = c("Training data", "Test data"))
    )

  if (padding_upperbound) {
    results <- results %>%
      dplyr::mutate(hover_text = paste0(
        ifelse(.data$set_status == "Test data", "Signal detection period", ""),
        "<br>Week: ", .data$isoweek,
        "<br>Observed: ", .data$cases,
        ifelse(!is.na(.data$upperbound_pad) | !is.na(.data$upperbound), (
          ifelse(is.na(.data$upperbound_pad),
            paste0("<br>Threshold: ", round(.data$upperbound, 1)),
            paste0("<br>Threshold: ", round(.data$upperbound_pad, 1))
          )
        ), ""),
        ifelse(!is.na(.data$expected_pad) | !is.na(.data$expected), (
          ifelse(is.na(.data$expected_pad),
            paste0("<br>Expected: ", round(.data$expected, 1)),
            paste0("<br>Expected: ", round(.data$expected_pad, 1))
          )
        ), "")
      ))
  } else {
    results <- results %>%
      dplyr::mutate(hover_text = paste0(
        ifelse(.data$set_status == "Test data", "Signal detection period", ""),
        "<br>Week: ", .data$isoweek,
        "<br>Observed: ", .data$cases,
        ifelse(!is.na(.data$upperbound),
          paste0("<br>Threshold: ", round(.data$upperbound, 1)), ""
        ),
        ifelse(!is.na(.data$expected),
          paste0("<br>Expected: ", round(.data$expected, 1)), ""
        )
      ))
  }


  # Periods - ends on the first date in the following week, [start; end)
  # Dates for the latest ~year (`number_of_weeks` period).
  range_dates_year <- max(results$date) - lubridate::weeks(c(number_of_weeks, 0) - 1)

  # Static plots should be based only on the latest `number_of_weeks` weeks
  if (!interactive) {
    results <- results %>%
      dplyr::filter(.data$date >= .env$range_dates_year[1])
  }

  # Dates for the training period and _signal _detection _period (test data period)
  period_dates_df <- results %>%
    dplyr::group_by(.data$set_status) %>%
    dplyr::summarise(
      start = min(.data$date),
      end = max(.data$date) + lubridate::days(7)
    )
  # number of days in _signal _detection _period
  ndays_sdp <- dplyr::filter(
    period_dates_df,
    .data$set_status == "Test data"
  ) %>%
    {
      difftime(.$end, .$start, units = "days")
    } %>%
    as.numeric()
  # Add dummy week to `results` to end the threshold line by a
  #   horizontal segment (geom_step) in the final week
  results <- results %>%
    dplyr::filter(date == max(.data$date)) %>% # final week-date
    dplyr::mutate(
      cases = NA, alarms = NA,
      date = .data$date + lubridate::days(7),
      hover_text = "" # don't show misleading hover at dummy data
    ) %>%
    dplyr::bind_rows(results, .)

  # function to find a nice-looking ymax value for y-axis range
  #   (plotly does not work with ymax=Inf)
  custom_round_up <- function(x) max(pretty(c(0, x)))
  # compute local ymax for plotly adaptive y-axis when zooming in on x-axis
  if (padding_upperbound) {
    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ymax = custom_round_up(c(
        .data$cases * dplyr::if_else(.data$alarms, 1.1, 1, missing = 1),
        # 1.1 to add space for signal-* on top-edge of case-number bars
        .data$upperbound, .data$upperbound_pad, 1
      )))
  } else {
    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ymax = custom_round_up(c(
        .data$cases * dplyr::if_else(.data$alarms, 1.1, 1, missing = 1),
        # 1.1 to add space for signal-* on top-edge of case-number bars
        .data$upperbound, 1
      )))
  }

  # ymax overall for rectangle background
  ymax_data <- max(results$ymax)

  col.threshold <- "#2297E6"
  col.expected <- "#000000"
  col.alarm <- "#FF0000"
  col.training <- "#9E9E9E"
  col.test <- "#304794"
  col.intervention <- "#ff8c00"

  legend_values <- c(
    "Expected" = col.expected,
    "Threshold" = col.threshold
  )

  half_week <- lubridate::days(3)

  plt <-
    results %>%
    ggplot2::ggplot(ggplot2::aes(x = date, group = 1, text = hover_text)) +
    ggplot2::geom_rect(
      data = period_dates_df, inherit.aes = FALSE,
      ggplot2::aes(
        x = NULL, y = NULL,
        xmin = start, xmax = end,
        fill = paste0("bg_", set_status)
      ),
      ymin = 0, ymax = ymax_data,
      colour = "white", linewidth = 0.5, alpha = 0.2
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        x = date + half_week, # center bars around mid-week
        y = cases, fill = set_status
      )
    ) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound, color = "Threshold"),
      linewidth = 1.3, direction = "hv"
    )

  if (padding_upperbound && any(!is.na(results$upperbound_pad))) {
    plt <- plt +
      ggplot2::geom_step(
        ggplot2::aes(y = upperbound_pad, color = "Threshold", linetype = "Test data"),
        linewidth = 0.3, direction = "hv"
      )
  }

  if (padding_expected && any(!is.na(results$expected_pad))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
        linewidth = 1.3, direction = "hv"
      ) +
      ggplot2::geom_step(ggplot2::aes(y = expected_pad, color = "Expected", linetype = "Training data"),
        linewidth = 0.3, direction = "hv"
      )
  } else if (any(!is.na(results$expected))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
        linewidth = 1.3, direction = "hv"
      )
  }

  # adding intervention vertical line
  if (!is.null(intervention_date)) {
    plt <- plt +
      ggplot2::geom_vline(xintercept = intervention_date, linetype = "dashed", color = col.intervention, size = 0.7)
    legend_values <- c(legend_values, "Intervention" = col.intervention)
  }

  # adding signal points
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
    ggplot2::scale_color_manual(values = legend_values) +
    ggplot2::scale_fill_manual(
      values = c(
        "Test data" = col.test, "Training data" = col.training,
        "bg_Training data" = "white", "bg_Test data" = col.threshold
      ),
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
            yaxis = list(
              range = c(0, ymax_data),
              rangemode = "fixed"
            ), # so we always can see the big picture in the rangeslider plot
            thickness = 0.10
          ),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
              list(step = "all", label = "All time points")
            )
          )
        ),
        yaxis = list(range = c(
          0,
          results %>%
            # pick the default x-range view
            dplyr::filter(date >= range_dates_year[1]) %>%
            dplyr::select("ymax") %>% max()
        )),
        legend = list(
          orientation = "h", x = 0.5, y = -0.9,
          yanchor = "top", xanchor = "center"
        )
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
      ", data = list(
        results = dplyr::select(results, c("date", "ymax")),
        date_range = range_dates_all
      ))
    }
    # Update the plot with dynamic y-axis adjustment and x-axis bugfix
    plt <- update_axes(plt)

    # modifying the interactive plot legend
    # This is horrible, we need to find a solution at some point to do this differently
    plt$x$data[[1]]$showlegend <-
      plt$x$data[[2]]$showlegend <- FALSE
    plt$x$data[[1]]$hoverinfo <-
      plt$x$data[[2]]$hoverinfo <- "skip"
    plt$x$data[[3]]$showlegend <- FALSE
    plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <- "Signal detection period"
    plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <- "Threshold"
    plt$x$data[[6]]$showlegend <- FALSE

    if (padding && any(!is.na(results$expected_pad))) {
      plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Expected"
      if (!is.null(intervention_date)) {
        plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Intervention (pandemic)"
        plt$x$data[[8]]$showlegend <- TRUE
      }

      if (length(plt$x$data) == 9 && any(results$alarms == TRUE, na.rm = TRUE)) {
        if (is.null(intervention_date)) {
          plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Signal"
        } else {
          plt$x$data[[9]]$name <- plt$x$data[[9]]$legendgroup <- "Signal"
          plt$x$data[[9]]$showlegend <- TRUE
        }
      } else {
        if (any(results$alarms == TRUE, na.rm = TRUE)) {
          plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Signal"
        }
      }
    } else {
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Signal"
      }
    }
  }
  return(plt)
}
