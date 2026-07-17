#' Plot epidemic curve for signal detection for different strata
#'
#' Creates an interactive epidemic curve visualizing the daily case counts by stratum category for selected signals.
#'
#' The function ensures consistent factor levels, fills missing combinations with zero counts,
#' and returns a Plotly interactive visualization with a minimal dashboard-style theme.
#'
#' @param data_cases A data frame containing case-level data. It has to include
#'   the variables `signal` and `date_report`. If `stratum` is not `NULL`,
#'   the corresponding stratum variable must also be present.
#' @param stratum A character specifying the stratum which should be used for the epicurve
#'
#' @return A Plotly object representing an epicurve of case counts by selected stratum for each reporting day for each selected signal.
plot_epicurve <- function(
    data_cases,
    stratum = NULL
) {
  checkmate::assert_data_frame(
    data_cases,
    min.rows = 1,
    min.cols = 2
  )

  checkmate::assert_string(
    stratum,
    null.ok = TRUE
  )

  if (!is.null(stratum)) {
    checkmate::assert_choice(
      stratum,
      choices = names(data_cases)
    )
  }

  # Save the original age-group order before aggregation
  age_group_levels <- if (
    identical(stratum, "age_group") &&
    is.factor(data_cases[[stratum]])
  ) {
    levels(data_cases[[stratum]])
  } else {
    NULL
  }

  if (!is.null(age_group_levels)) {
    age_group_levels <- age_group_levels[
      !is.na(age_group_levels) &
        age_group_levels != "unknown"
    ]
  }

  stratum_label <- if (is.null(stratum)) {
    "Cases"
  } else {
    stratum
  }

  # Aggregate selected signal cases
  data_cases <- data_cases |>
    dplyr::filter(signal) |>
    dplyr::mutate(
      plot_stratum = if (is.null(stratum)) {
        "All cases"
      } else {
        dplyr::coalesce(
          as.character(.data[[stratum]]),
          "unknown"
        )
      },
  n = 1L
  )

  stratum_levels <- unique(data_cases$plot_stratum)
  has_unknown <- "unknown" %in% stratum_levels

  # Define order and colors
  if (is.null(stratum)) {
    plot_levels <- "All cases"

    fill_colors <- c(
      "All cases" = "#304898"
    )

  } else if (identical(stratum, "age_group")) {
    known_levels <- if (!is.null(age_group_levels)) {
      age_group_levels[
        age_group_levels %in% stratum_levels
      ]
    } else {
      stratum_levels[
        stratum_levels != "unknown"
      ]
    }

    age_colors <- grDevices::hcl.colors(
      n = length(known_levels),
      palette = "Blues 3"
    )

    fill_colors <- stats::setNames(
      age_colors,
      known_levels
    )

    if (has_unknown) {
      fill_colors <- c(
        fill_colors,
        "unknown" = "#B3B3B3"
      )
    }

    plot_levels <- names(fill_colors)

  } else {
    known_levels <- stratum_levels[
      stratum_levels != "unknown"
    ]

    category_colors <- grDevices::hcl.colors(
      n = length(known_levels),
      palette = "Dark 3"
    )

    fill_colors <- stats::setNames(
      category_colors,
      known_levels
    )

    if (has_unknown) {
      fill_colors <- c(
        fill_colors,
        "unknown" = "#B3B3B3"
      )
    }

    plot_levels <- names(fill_colors)
  }

  stack_levels <- if (identical(stratum, "age_group")) {
    rev(plot_levels)
  } else {
    plot_levels
  }

  # Apply plotting order and add missing date-stratum combinations
  data_cases <- data_cases |>
    dplyr::mutate(
      plot_stratum = factor(
        plot_stratum,
        levels = stack_levels
      )
    ) |>
    tidyr::complete(
      date_report,
      plot_stratum,
      fill = list(n = 0)
    ) |>
    dplyr::mutate(
      tooltip = paste0(
        "Date: ", date_report,
        "<br>", stratum_label, ": ", plot_stratum,
        "<br>Cases: ", n
      )
    )

  p <- ggplot2::ggplot(
    data_cases,
    ggplot2::aes(
      x = date_report,
      y = n,
      fill = plot_stratum,
      text = tooltip
    )
  ) +
    ggplot2::geom_col(position = "stack",
                      colour = "white",
                      linewidth = 0.35,
                      width = 1
                      ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty()
    ) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      breaks = plot_levels,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = if (is.null(stratum)) {
        "Epidemic curve"
      } else {
        paste("Epidemic curve by", stratum)
      },
      x = "Date of report",
      y = "Case count",
      fill = if (is.null(stratum)) {
        NULL
      } else {
        stratum
      }
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
    )+
    ggplot2::guides(
      fill = if (is.null(stratum)) {
        "none"
      } else {
        "legend"
      }
    )+
    ggplot2::scale_x_date(
      date_labels = "%Y-%m-%d"
    )

  plotly::ggplotly(
    p,
    tooltip = "text"
  )
}
