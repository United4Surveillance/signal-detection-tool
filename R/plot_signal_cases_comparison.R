#' Age group comparison plot for signal detection
#'
#' Creates an interactive bar plot comparing the distribution of cases across age groups,
#' split by signal status (cases part of a signal vs. non-signal cases over a specified time window).
#'
#' The function ensures consistent factor levels, fills missing combinations with zero counts,
#' and returns a Plotly interactive visualization with a minimal dashboard-style theme.
#'
#' @param data_agg A data frame containing aggregated case counts. Must include at least
#'   the variables `signal`, `age_group`,`n`,`perc` and `total_n`. Typically created using `dplyr::count()`.
#'
#' @param number_of_weeks Integer scalar. Number of weeks signal detection was applied for, which defines the time window or cases used for the comparison.
#'   Must be a single positive integer (>= 1).
#'
#' @return A Plotly object representing a grouped bar chart of case counts by age group and signal status.
plot_agegroup_comparison <- function(data_agg, number_of_weeks) {
  checkmate::assert_data_frame(
    data_agg,
    min.rows = 1,
    min.cols = 2
  )

  checkmate::assert_integerish(
    number_of_weeks,
    lower = 1,
    len = 1
  )

  data_agg <- data_agg |>
    dplyr::mutate(signal = factor(signal,
      levels = c(TRUE, FALSE),
      labels = c("Signal cases", paste0("All cases excluding signal cases (last ", number_of_weeks, " weeks)"))
    )) |>
    tidyr::complete(age_group, signal, fill = list(n = 0))

  p <- ggplot2::ggplot(
    data_agg,
    ggplot2::aes(
      x = age_group,
      y = perc,
      fill = signal,
      text = paste0(
        "Age group: ", age_group, "<br>",
        "Proportion of cases: ", perc, "%", "<br>",
        "Number of cases: ", n, "<br>",
        "Total number of cases: ", total_n, "<br>",
        "Group: ", signal
      )
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single")) +
    ggplot2::scale_fill_manual(values = c("Signal cases" = "#304898")) +
    ggplot2::labs(
      title = "Distribution by age group",
      x = "Age group",
      y = "Proportion (%) of cases",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
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

  plotly::ggplotly(p, tooltip = "text") |>
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
