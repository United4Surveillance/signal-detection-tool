#' Plot in how many strata an signal was detected under the detection period
#'
#' Using the results of signal detection, plot a time unit-to-time unit representation of
#' strata had higher than expected case numbers
#'
#' @param results dataframe of a single-pathogen signal detection results for a strata category
#' @param n_strata integer. Number of stratification levels in category. Usually determined automatically by signals_agg
#' @param interactive logical, if TRUE, interactive plot is returned; default, static plot.
#' @param branding named vector with branding colours
#'
#' @return either a ggplot or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' signals <- data_preprocessed %>% get_signals(stratification = "county")
#' n.strata <- 9
#' signals_time_unit_barchart <- plot_signals_per_time_unit(
#'   signals,
#'   n_strata = n.strata
#' )
#' signals_time_unit_barchart
#' }
plot_signals_per_time_unit <- function(results, n_strata, interactive = FALSE, branding = NULL) {
  if (is.null(branding)) {
    branding <- stats::setNames(c("lightgray", "#be1622"), c("primary", "danger"))
  } else {
    # check that given branding have named colors
    checkmate::assert_names(names(branding), must.include = c("primary", "danger"))

    branding["primary"] <- "lightgray"
  }

  # filter out dates outside signal detection period
  results <- results %>% dplyr::filter(!is.na(.data$alarms))

  # extract time unit used
  if ("monthly" == results$time_unit %>% head(1)) {
    time_unit <- "monthly"
  } else if ("biweekly" == results$time_unit %>% head(1)) {
    time_unit <- "biweekly"
  } else {
    time_unit <- "weekly"
  }

  # add date
  if (time_unit %in% c("weekly", "biweekly")) {
    results <- results %>%
      dplyr::mutate(
        year_time_unit = sprintf("%d-W%02d", .data$year, .data$week),
        date = ISOweek::ISOweek2date(paste0(.data$year_time_unit, "-1"))
      )
  } else if (time_unit %in% "monthly") {
    results <- results %>%
      dplyr::mutate(
        year_time_unit = sprintf("%d-%02d", .data$year, .data$month),
        date = as.Date(paste0(.data$year_time_unit, "-01"))
      )
  }

  # count strata with signals for each time unit
  signals_time_units <- results %>% # to do
    dplyr::group_by(.data$year_time_unit) %>%
    dplyr::summarise(
      n.signals = sum(.data$alarms),
      n.rest = n_strata - .data$n.signals,
      p.signals = sum(.data$alarms) / n_strata * 100 %>% round(1),
      p.rest = 100 - .data$p.signals
    ) %>%
    dplyr::ungroup()

  if (!interactive) {
    signals_time_units <- signals_time_units %>%
      tidyr::pivot_longer(
        cols = c("p.signals", "p.rest"),
        names_to = "type",
        values_to = "p.strata"
      ) %>%
      dplyr::mutate(
        type = factor(.data$type,
          levels = c("p.rest", "p.signals"),
          labels = c("without signals", "with signals")
        )
      )

    p <- signals_time_units %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(
        ggplot2::aes(
          x = .data$year_time_unit, y = .data$p.strata, fill = .data$type
        )
      ) +
      ggplot2::labs(
        x = if (time_unit == "weekly") "Week" else if (time_unit == "biweekly") "Two-week period, starting week" else if (time_unit == "monthly") "Month",
        y = "Strata with signals (%)"
      ) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(c(branding["primary"], branding["danger"]), NULL)
      ) +
      ggplot2::theme(
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
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
      )
  } else {
    p <- plotly::plot_ly() %>%
      plotly::add_trace(
        type = "bar",
        name = "with signals",
        x = signals_time_units$year_time_unit,
        y = signals_time_units$p.signals,
        text = signals_time_units$n.signals,
        textposition = "none",
        marker = list(color = branding["danger"]),
        hovertemplate = "%{text} (%{y:.1f}%) strata<extra></extra>"
      ) %>%
      plotly::add_trace(
        type = "bar",
        name = "without signals",
        x = signals_time_units$year_time_unit,
        y = signals_time_units$p.rest,
        text = signals_time_units$n.rest,
        textposition = "none",
        marker = list(color = branding["primary"]),
        hovertemplate = "%{text} (%{y:.1f}%) strata<extra></extra>"
      ) %>%
      plotly::layout(
        xaxis = list(
          title = if (time_unit == "weekly") {
            "Week"
          } else if (time_unit == "biweekly") {
            "Two-week period, starting week"
          } else if (time_unit == "monthly") {
            "Month"
          }
        ),
        yaxis = list(
          title = "Strata with signals (%)"
        ),
        barmode = "stack",
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.5, y = 1,
          xref = "paper", yref = "container",
          xanchor = "center", yanchor = "bottom"
        )
      )
  }

  return(p)
}
