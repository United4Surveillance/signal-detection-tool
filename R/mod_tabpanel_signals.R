#' tabpanel "signals" UI Function
#'
#' @description A shiny Module for a tab to generate and display results from
#' signal detection methods based on parameters inputs chosen.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_signals_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Signals",
    shiny::uiOutput(ns("signals_tab_ui")),

    icon = shiny::icon("wave-square")
  )
}


#' tabpanel "signals" Server Functions
#'
#' @noRd
mod_tabpanel_signals_server <- function(
    id,
    data,
    errors_detected,
    number_of_weeks,
    strat_vars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI-portion of the tab below!
    # ensuring that content is onlyu shown if data check returns no errors
    output$signals_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(shiny::tagList(
          shiny::br(),
          shiny::h2("Data Format Check Failed"),
          shiny::p("Unfortunately, the selected data does not meet the required format."),
          shiny::p("Please make sure the data follows the correct structure and try again."),
          shiny::br(),
          shiny::hr(),
          shiny::p("You can check the data in the 'Data' tab for more details on the issue.")
        ))
      } else {
        return(shiny::tagList(
          uiOutput(ns("plot_table_stratas")),
          shiny::br(),
          shiny::h3("Signal detection table"),
          DT::DTOutput(ns("signals"))
        ))
      }
    })

    # fix stratification vars
    strat_vars_tidy <- reactive({
      req(strat_vars)
      strat_vars_chr <- strat_vars()
      # Tidy up stratification vector
      if ("None" %in% strat_vars_chr) {strat_vars_chr <- NULL}
      # 'None' takes precedence over 'All'
      else if ("All" %in% strat_vars_chr) {strat_vars_chr <- names(data())}

      return(strat_vars_chr)
    })

    # generate signals once
    signal_results <- shiny::reactive({
      shiny::req(!errors_detected())
      results <- SignalDetectionTool::get_signals(
        data = data(),
        stratification = strat_vars_tidy(),
        date_var = "date_report",
        number_of_weeks = number_of_weeks()
      )
      results
    })

    signals_agg <- shiny::reactive({
      shiny::req(signal_results)
      aggregate_signals(signal_results(), number_of_weeks = number_of_weeks())
    })

    signal_data <- shiny::reactive({
      shiny::req(!errors_detected())
      shiny::req(signal_results())
      weeks <- paste0(signal_results()$year, "-", signal_results()$week, "-1") %>%
        as.Date("%Y-%W-%u") %>%
        unique() %>%
        sort()

      dates <- seq(weeks[1], weeks[length(weeks)], by = "day")
      data_n_weeks <- data() %>%
        dplyr::filter(date_report %in% dates) # this has to use the same variable as in get_signals()
      data_n_weeks
    })

    output$plot_table_stratas <- renderUI({
      req(signal_results)
      plot_table_list <- list()

      # using the categories of the signal_results instead of strat_vars_tidy
      # because it could be that not for all selected strat_vars signals could have been generated
      # we would still want to give this feedback to the user then
      signal_categories <- unique(signal_results()$category)
      # remove the NA category which is generate when signals were generated unstratified
      signal_categories <- signal_categories[!is.na(signal_categories)]
      n_plots_tables <- length(signal_categories)

      if (n_plots_tables == 0){
        # plot the raw timeseries
        plot_timeseries <- plot_time_series(signal_results(), interactive = TRUE)
        plot_table_list[[1]] <- plot_timeseries
      }
      else if(n_plots_tables == 1){
        if(signal_categories %in% c("county","state")){
          plot_or_table1 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[1])
        }else{
          plot_or_table1 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[1])
        }
        plot_table_list[[1]] <- plot_or_table1
      }

      else if(n_plots_tables == 2){

        if(signal_categories[1] %in% c("state","county","community")){
          plot_or_table1 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[1])
        }else{
          plot_or_table1 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[1])
        }
        if(signal_categories[2] %in% c("state","county","community")){
          plot_or_table2 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[2])
        }else{
          plot_or_table2 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[2])
        }

        plot_table_list[[1]] <- plot_or_table1
        plot_table_list[[2]] <- plot_or_table2

      }

      else if(n_plots_tables == 3){

        if(signal_categories[1] %in% c("state","county","community")){
          plot_or_table1 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[1])
        }else{
          plot_or_table1 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[1])
        }
        if(signal_categories[2] %in% c("state","county","community")){
          plot_or_table2 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[2])
        }else{
          plot_or_table2 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[2])
        }

        if(signal_categories[3] %in% c("state","county","community")){
          plot_or_table3 <- create_map_or_table(signals_agg(),
                                                data(),
                                                signal_categories[3])
        }else{
          plot_or_table3 <- create_barplot_or_table(signals_agg(),
                                                    signal_categories[3])
        }
        plot_table_list[[1]] <- plot_or_table1
        plot_table_list[[2]] <- plot_or_table2
        plot_table_list[[3]] <- plot_or_table3

      }

      if(n_plots_tables == 0| n_plots_tables == 1){
        column_plots <- fluidRow(column(12, plot_table_list[1]))
      } else{
        column_plots <- fluidRow(
          lapply(1:n_plots_tables, function(x) column(12/n_plots_tables, plot_table_list[x]))
        )
      }

      return(column_plots)

    })

    # signals table
    output$signals <- DT::renderDT({
      req(!errors_detected())
      create_results_table(signal_results(),
                           interactive = TRUE)
      # FIXME: interactive mode not working here?
    })

  })
}
