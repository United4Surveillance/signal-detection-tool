
#' Plot age-groups grouped by another variable
#'
#' @param df case data
#' @param age_group_col name of the age-group column
#' @param by_col name of the grouping column
#' @param interactive if TRUE, interactive plot is returned
#'
#' @return either a gg or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_agegroup_by(df = test_data,
#'                   age_group_col = "age_group",
#'                   by_col = "county")
#' }
plot_agegroup_by <- function(df,
                             age_group_col = "age_group",
                             by_col = "sex",
                             interactive = FALSE) {

  # check if age_group_col is contained in df and if age_group_col is a factor
  checkmate::assert(
    checkmate::check_choice(age_group_col, choices = names(df)),
    checkmate::check_factor(df[, age_group_col], all.missing = FALSE),
    combine = "and"
  )

  # check if by_col is NULL or if by_col is contained in df and if by_col is a
  # factor
  checkmate::assert(
    checkmate::check_choice(by_col, choices = names(df), null.ok = TRUE),
    if (!is.null(by_col)) {
      checkmate::check_factor(df[, by_col], all.missing = FALSE)
    } else {
      TRUE
    },
    combine = "and"
  )

  # plot
  p <- ggplot2::ggplot(data = df)

  if (!is.null(by_col)) {
    n_levels <- length(levels(df[, by_col]))
    p <- p +
      ggplot2::geom_bar(
        stat = "count",
        position = ggplot2::position_dodge(preserve = "single"),
        mapping = ggplot2::aes(
          x = !!rlang::sym(age_group_col),
          fill = !!rlang::sym(by_col), # TODO: colors
          text = sprintf("%s: %.0f",
                            .data$fill,
                            ggplot2::after_stat(.data$count))),
        color = "black") +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = min(n_levels, 5)))
  } else {
    p <- p +
      ggplot2::geom_bar(
        stat = "count",
        position = "dodge",
        mapping = ggplot2::aes(
          x = !!rlang::sym(age_group_col),
          text = sprintf("Count: %.0f", ggplot2::after_stat(.data$count))),
        color = "black",
        fill = scales::hue_pal()(1)) # TODO: colors
  }

  p <- p +
    ggplot2::labs(x = "Age-group", y = "Count") +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      expand = ggplot2::expansion(mult = c(0, 0.1))) +
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
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold")) +
    NULL

  if(interactive) {
    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(legend = list(orientation = "h", x = 0.3, y = 1.1)) %>%
      plotly::config(modeBarButtonsToRemove = c('autoScale2d',
                                                'resetScale2d',
                                                'select2d',
                                                'lasso2d',
                                                'zoomIn2d',
                                                'zoomOut2d',
                                                'pan2d',
                                                'zoom2d',
                                                'toggleSpikelines'))
  }

  p

}


# # Example usage
# source("R/tool_functions.R", encoding = "UTF-8")
# test_data <- read.csv("data/input/input.csv") %>%
#   age_groups() %>%
#   dplyr::mutate_at(c("sex", "county"), factor)
# p_ag <- plot_agegroup_by(df = test_data,
#                          age_group_col = "age_group",
#                          by_col = NULL)
# p_ag
# p_ag_sex <- plot_agegroup_by(df = test_data,
#                              age_group_col = "age_group",
#                              by_col = "sex", interactive = TRUE)
# p_ag_sex
# p_ag_county <- plot_agegroup_by(df = test_data,
#                                age_group_col = "age_group",
#                                by_col = "county")
# p_ag_county

