#' Title
#'
#' @param dat_padded dataframe of a single-pathogen signal detection results for a strata category
#' @param interactive logical, if TRUE, interactive plot is returned; default, static plot.
#' @param branding named vector with branding colours
#'
#' @return either a ggplot or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- preprocess_data(input_example)
#' signals <- get_signals(dat, stratification = "county")
#'
#' plot_signals_per_week(signals)
#' }
plot_signals_per_week <- function(dat_padded, interactive = FALSE, branding = NULL){

  # if(dplyr::n_distinct())
  if(is.null(branding)){
    branding <- setNames(c("lightgray", "#be1622"), c("primary","danger"))
  } else {
    #check that given branding have named colors
    checkmate::assert_names(names(branding), must.include = c("primary","danger"))

    branding["primary"] <- "lightgray"
  }

  # filter out dates outside signal detection period
  dat_padded <- dat_padded %>% dplyr::filter(!is.na(alarms))

  # add date
  dat_padded <- dat_padded %>%
    dplyr::mutate(
      isoweek = sprintf("%d-W%02d", .data$year, .data$week),
      date = ISOweek::ISOweek2date(paste0(.data$isoweek, "-1"))
      )

  # count strata with signals for each week
  signals_week <- dat_padded %>%
    dplyr::group_by(isoweek) %>%
    dplyr::summarise(
      n.signals = sum(alarms),
      n.rest = dplyr::n() - n.signals,
      p.signals = sum(alarms)/dplyr::n() * 100 %>% round(1),
      p.rest = 100 - p.signals
    ) %>%
    dplyr::ungroup()

  if(!interactive){
    signals_week <- signals_week %>%
      tidyr::pivot_longer(
        cols = c(p.signals, p.rest),
        names_to = "type",
        values_to = "p.strata"
        ) %>%
      dplyr::mutate(
        type = factor(type,
          levels = c("p.rest", "p.signals"),
          labels = c("without signals", "with signals"))
      )

    p <- signals_week %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = isoweek, y = p.strata, fill = type)) +
      ggplot2::labs(x = "Week", y = "Strata with signals (%)") +
      ggplot2::scale_fill_manual(
        values = setNames(c(branding["primary"], branding["danger"]), NULL)
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
        x = signals_week$isoweek,
        y = signals_week$p.signals,
        text = signals_week$n.signals,
        textposition = "none",
        marker = list(color = branding["danger"]),
        hovertemplate = "%{text} (%{y:.1f}%) strata<extra></extra>"
      ) %>%
      plotly::add_trace(
        type = "bar",
        name = "without signals",
        x = signals_week$isoweek,
        y = signals_week$p.rest,
        text = signals_week$n.rest,
        textposition = "none",
        marker = list(color = branding["primary"]),
        hovertemplate = "%{text} (%{y:.1f}%) strata<extra></extra>"
      ) %>%
      plotly::layout(
        xaxis = list(title = "Week"),
        yaxis = list(
          title = "Strata with signals (%)"),
        barmode = "stack",
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.5, y = 1,
          xref = "paper", yref = "container",
          xanchor = "center", yanchor = "bottom"
        ))

    p <- plotly::partial_bundle(p)

    # change toJSON function to save max 1 significant digit
    attr(p$x, "TOJSON_FUNC") <- function(x, ...) {
      jsonlite::toJSON(x,
                       digits = 1, auto_unbox = TRUE, force = TRUE,
                       null = "null", na = "null", time_format = "%Y-%m-%d",
                       ...
      )
    }
  }

  return(p)
}
