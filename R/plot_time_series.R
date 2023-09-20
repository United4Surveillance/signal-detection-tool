library(tidyverse)
library(plotly)
library(ISOweek)

#' Plot time-series based on the results of Farrington Flexible
#'
#' @param results data returned by the get_signals_farringtonflexible() 
#' @param interactive if TRUE, interactive plot is returned
#' @param number_of_weeks number of weeks to be covered in the plot
#'
#' @return either a gg or plotly object
#'
#' @examples
#' source("R/tool_functions.R")
#' source("R/farrington_flexible.R")
#' data <- read.csv("data/input/input.csv", header = TRUE, sep = ",")
#' results <- get_signals_farringtonflexible(data)
#' plot_time_series(results)
plot_time_series <- function(results, interactive = FALSE, 
                         number_of_weeks = 52) { 
  
  results <- results %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(date = ISOweek::ISOweek2date(paste(year, "-W", stringr::str_pad(week,width=2, pad="0"), "-1", sep=""))) %>% 
    dplyr::arrange(date) %>% 
    dplyr::slice_tail(n = number_of_weeks)
  
  col.observed <- "#9E9E9E"
  col.threshold <- "#2297E6"
  col.expected <- "#000000"
  col.alarm <- "#DF536B"
  
  # Time-series plot
  if(interactive) { 
    
    p <- plotly::plot_ly(data = results) %>% 
      plotly::add_bars(x = ~date, y = ~cases, 
                       color = I(col.observed),
                       hovertemplate = ~paste0("Date: ", date, "\nObserved: ", cases, "<extra></extra>"), 
                       name = "Observed", legendgroup = "group1") %>% 
      plotly::add_lines(x = ~date, y = ~upperbound, 
                        color = I(col.threshold),
                        line = list(shape = "hvh"), 
                        hovertemplate = ~paste0("Date: ", date, "\nThreshold: ", round(upperbound), "<extra></extra>"), 
                        name = "Threshold", legendgroup = "group2") %>% 
      plotly::add_lines(x = ~date, y = ~expected, 
                        color = I(col.expected),
                        line = list(shape = "hvh"), 
                        hovertemplate = ~paste0("Date: ", date, "\nExpected: ", round(expected), "<extra></extra>"), 
                        name = "Expected", legendgroup = "group3") %>% 
      plotly::add_markers(data = filter(results, alarms == TRUE), x = ~date, y = ~cases, 
                          color = I(col.alarm),
                          marker = list(size = 12),
                          symbol = I("asterisk-open"), 
                          hovertemplate = ~paste0("Date: ", date, "\nAlarm", "<extra></extra>"),
                          name = "Alarm", legendgroup = "group4") %>% 
      plotly::layout(xaxis = list(tickformat = "%Y-%m-%d", tickvals = pretty(results$date, n = 8), 
                                  title = "Time"), 
                     yaxis = list(title = "Number of Infected", 
                                  tickvals = pretty(c(results$cases, results$expected, results$upperbound), 10)),
                     legend = list(orientation = "h", x = 0.25, y = -0.2)) %>% 
      plotly::config(modeBarButtonsToRemove = c('autoScale2d', 'resetScale2d', 'select2d', 
                                                'lasso2d', 
                                                'zoomIn2d', 'zoomOut2d', 'pan2d', 'zoom2d', 'toggleSpikelines'))
    
  } else{ 
    
    p <- results %>% 
      ggplot2::ggplot(aes(x = date)) + 
      ggplot2::geom_col(aes(y = cases, fill = "Observed")) + 
      ggplot2::geom_step(aes(y = upperbound, color = "Threshold"), 
                         linewidth = 1, direction = "mid") +
      ggplot2::geom_step(aes(y = expected, color = "Expected"), 
                         linewidth = 1, direction = "mid") + 
      ggplot2::geom_point(data = filter(results, alarms == TRUE), aes(y = cases, shape = alarms), 
                          color = col.alarm, size = 4) + 
      ggplot2::scale_x_date(breaks = scales::breaks_pretty(n = 8), 
                            date_labels = "%Y-%d-%m") +
      scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                         expand = ggplot2::expansion(mult = c(0, 0.1))) + 
      ggplot2::scale_shape_manual(values = c("TRUE" = 8), 
                                  labels = c("TRUE" = "Alarm")) + 
      ggplot2::scale_color_manual(values = c("Expected" = col.expected, "Threshold" = col.threshold)) + 
      ggplot2::scale_fill_manual(values = c("Observed" = col.observed)) + 
      ggplot2::theme( 
        legend.position = "bottom",
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "grey75"),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold")) + 
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1), color = ggplot2::guide_legend(order = 2), 
                      shape = ggplot2::guide_legend(order = 3)) + 
      ggplot2::labs(x = "Time", y = "Number of infected", color = NULL, fill = NULL, shape = NULL)
  }
  
  p
  
}
