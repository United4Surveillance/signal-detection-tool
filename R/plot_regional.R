#' Plot surveillance data and signal detection results by region
#'
#' @param data data.frame containing surveillance data in linelist format 
#' @param signals data.frame containing signal detection output stratified by regions
#' @param shape data.frame with geometry column in sf format
#' @param country_id two-letter country-code for the country that should be plotted
#' @param regional_level character describing which level of aggregation should be shown ("country", "state","county" or "community")
#' @param interactive boolean identifying whether the plot should be static or interactive
#'
#' @return either a ggplot object if static plot is chosen or a ggiraph object for the interactive plot 
#' @export
#'
#' @examples
plot_regional <- function(data, signals, shape, country_id = "AT", regional_level = "county", interactive = FALSE){
  
  assertChoice(regional_level, c("country", "state","county", "community"))
  
  checkmate::assert(
    checkmate::check_true(interactive),
    checkmate::check_false(interactive),
    combine = "or"
  )
  
  assertClass(shape, "sf")
  assertClass(data, "data.frame")
  assertClass(signals, "data.frame")
  
  shape <- shape %>% filter(CNTR_CODE == country_id)
  shape <- switch(regional_level,
                  country = shape %>% filter(LEVL_CODE == 0),
                  state = shape %>% filter(LEVL_CODE == 1),
                  county = shape %>% filter(LEVL_CODE == 2),
                  community = shape %>% filter(LEVL_CODE == 3),
                  shape
  )
  data_reg <- switch(regional_level,
                     country = data %>% group_by(country_id) %>% summarise(count = n()) %>% rename(region_id = country_id),
                     state = data %>% group_by(state_id) %>% summarise(count = n())  %>% rename(region_id = state_id),
                     county = data %>% group_by(county_id) %>% summarise(count = n()) %>% rename(region_id = county_id),
                     community = data %>% group_by(community_id) %>% summarise(count = n()) %>% rename(region_id = community_id),
                     data
  )
  
  signals <- signals %>% group_by(stratum) %>% summarise(alarms = sum(alarms, na.rm = T)) %>% filter(alarms>1)
  
  data_reg <- left_join(data_reg, signals, by = c("region_id" = "stratum"))
  
  shape <- left_join(shape, data_reg, by = c("NUTS_ID" = "region_id"))
  
  plot <- shape %>%
    filter(!is.na(count)) %>%
    ggplot(aes(fill = count, color = alarms)) +
    geom_sf_interactive(aes(size = 2*alarms, tooltip = paste0("cases: ", count," alarms: ", alarms), data_id = NUTS_ID)) +
    theme_void() +
    scale_fill_distiller_interactive(palette = "YlGn", direction = 1) + 
    scale_color_distiller_interactive(palette = "Reds", direction = 1) +
    scale_size_identity()
  
  if(interactive){
    return(girafe(ggobj = plot))
  }
  
  plot <- plot + geom_sf_text(aes(label=alarms), color="red") 
  
  plot
  
}

