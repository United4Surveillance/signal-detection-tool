#' tabpanel "input" UI Function
#'
#' @description A shiny Module for a tab to select input parameters for analyses in the shiny app.
#'
#' @param id Internal parameter for {shiny}, ensuring namespace coherency in sessions.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Input parameters",
    shiny::uiOutput(ns("input_tab_ui")),
    icon = icon("viruses")
  )
}


#' tabpanel "input" Server Functions
#' @param id,input,output,session standard \code{shiny} boilerplate
#' @param data reactive input dataset preprocessed if no errors
#' @param errors_detected reactive boolean, when TRUE errors on mandatory variables where detected
mod_tabpanel_input_server <- function(id, data, errors_detected) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # function for removing value from reactiveValues - haven't found a better way
    removeReactiveValuesIndex <- function(rvs, ind) {
      rvs_r6 <- .subset2(rvs, "impl")
      rvs_r6$.values$remove(ind)
      rvs_r6$.nameOrder <- setdiff(rvs_r6$.nameOrder, ind)
    }

    ## UI-portion of the tab below!
    # ensuring that content is onlyu shown if data check returns no errors
    output$input_tab_ui <- shiny::renderUI({
      if (errors_detected() == TRUE) {
        return(datacheck_error_message)
      } else {
        return(shiny::tagList(
          tags$style(
            HTML("
      .well {
        margin-top: 20px; /* Adjust the value to move the wellPanel lower */
      }
    ")
          ),
    shiny::fluidRow(
      column(
        6,
        shiny::wellPanel(
          span("Dataset settings", style = "font-size:140%;color:#304794"),
          hr(),
          span("Pathogen", style = "font-size:100%;font-weight: bold"),
          shiny::uiOutput(ns("pathogen_choices")),
          br(),
          span("Filters", style = "font-size:100%;font-weight: bold"),
          br(),
          span("You can chose to investigate a subset of your data according to the filters you select. When filtering by date_report you have the possibility select a specific timeperiod you want to investigate. In the timeseries visualisation only the timeperiod you selected will be shown and the outbreak detection algorithms will only train on the data from the timeperiod you selected."),
          br(),
          br(),
          shiny::div(
            id = "filter_input",
            span(
              "Add and remove filters",
              shiny::actionButton(
                inputId = ns("add_filter"),
                label = "",
                icon = shiny::icon("plus")
              ),
              shiny::actionButton(
                inputId = ns("remove_filter"),
                label = "",
                icon = shiny::icon("minus")
              )
            ),
          ),
          br(),
          mod_input_filter_ui(id = ns("filter0"))
        )
      ),
      column(
        6,
        shiny::wellPanel(
          span("Signal Detection settings", style = "font-size:140%;color:#304794"),
          hr(),
          span("Strata", style = "font-size:100%;font-weight: bold"),
          br(),
          span("Select up to 3 variables you want to stratify by. Signals and visualisations will be generated for each stratum."),
          shiny::uiOutput(ns("strat_choices")),
          br(),
          span("Signal Detection Period", style = "font-size:100%;font-weight: bold"),
          br(),
          span("Set the number of weeks you want to generate signals for. The signals are generated for the most recent weeks."),
          br(),
          shiny::uiOutput(ns("weeks_selection")),
          shiny::textOutput(ns("text_weeks_selection")),
          br(),
          span("Signal detection algorithm", style = "font-size:100%;font-weight: bold"),
          br(),
          span("Depending on the number of weeks you want to generate alarms for and the filters you set, the choice of algorithms is automatically updated to those which are possible to apply for your settings."),
          br(),
          shiny::uiOutput(ns("algorithm_choice"))
        )
      )
    )
        ))
      }
    })

    output$weeks_selection <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::numericInput(
        inputId = ns("n_weeks"),
        label = "",
        value = 6,
        min = 1,
        max = 52,
        step = 1,
        width = "40%"
      ) # TODO: make this dynamic
    })

    # using shinyvalidate to ensure value between min and max
    iv_weeks <- shinyvalidate::InputValidator$new()
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_required(
      message = "This input is required to be able to choose a signal detection algorithm."))
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_integer())
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_between(1, 52))
    iv_weeks$enable()

    output$text_weeks_selection <- shiny::renderText({
      shiny::req(!errors_detected())
      shiny::req(input$n_weeks)
      shiny::req(iv_weeks$is_valid())

      date_floor <- lubridate::floor_date(max(filtered_data()$date_report) - lubridate::weeks(input$n_weeks),
                                          week_start = 1, unit = "week")
      date_ceil  <- lubridate::ceiling_date(max(filtered_data()$date_report), unit = "week", week_start = 7)

      paste("Chosen signal detection period from", date_floor, "to", date_ceil)
    })

    data_sub <- shiny::reactive({
      req(data)
      req(!errors_detected())

      # add subset indicator for selected pathogens
      dat <- data() %>%
        dplyr::mutate(subset = pathogen %in% input$pathogen_vars)

      return(dat)
    })

    ## showing options in ui
    output$pathogen_choices <- shiny::renderUI({
      shiny::req(!errors_detected())
      return(shiny::selectInput(
        inputId = ns("pathogen_vars"),
        label = "",
        choices = unique(data()$pathogen),
        width = "40%"
      ))
    })

    # variable options for filter ui and strata selection
    available_var_opts <- shiny::reactive({
      shiny::req(data_sub)
      shiny::req(!errors_detected())
      available_vars <- intersect(
        c(
          "state",
          "county",
          "community",
          "region_level1",
          "region_level2",
          "region_level3",
          "subtype",
          "age_group",
          "sex"
        ),
        names(data_sub())
      ) %>%
        sort()
      available_vars
    })

    filter_var_opts <- shiny::reactive({
      shiny::req(available_var_opts())
      shiny::req(data_sub())
      date_opts <- intersect(names(data_sub()), c("date_report"))
      all_opts <- c("None", date_opts, available_var_opts())
      all_opts
    })


    # filtering ----------------------------------------------------------------
    # initalize reactive values containing filter parameters
    all_filters <- shiny::reactiveValues()
    # value connecting reactives and forcing reevaluation + keeping track of
    # filter count
    n_filters <- shiny::reactiveVal(1)

    # inital filter ui
    filter0_reactives <- mod_input_filter_server(
      id = "filter0",
      data = data_sub,
      filter_opts = filter_var_opts(),
      all_filters = all_filters,
      n_filters = n_filters
    )

    shiny::observeEvent(lapply(filter0_reactives, function(x) do.call(x, list())), {
      all_filters$filter0 <- filter0_reactives
    })

    # add filters
    shiny::observeEvent(input$add_filter, {
      if (n_filters() < (length(filter_var_opts()) - 1)) {
        # id to add
        new_filter_id <- paste0("filter", input$add_filter)
        # add ui
        shiny::insertUI(
          selector = "#filter_input",
          where = "afterEnd",
          ui = mod_input_filter_ui(id = ns(new_filter_id))
        )
        # add parameters
        all_filters[[new_filter_id]] <- mod_input_filter_server(
          id = new_filter_id,
          data = data_sub,
          filter_opts = filter_var_opts(),
          all_filters = all_filters,
          n_filters = n_filters
        )
        # update filter count
        n_filters(n_filters() + 1)
      }
    })

    # remove last filter added
    shiny::observeEvent(input$remove_filter, {
      if (n_filters() > 0) {
        # id to remove
        remove_filter_id <- names(all_filters)[length(names(all_filters))]
        # remove ui
        shiny::removeUI(
          selector = paste0("#", id, "-", remove_filter_id),
          immediate = TRUE
        )
        # remove parameters
        removeReactiveValuesIndex(all_filters, remove_filter_id)
        # update filter count
        n_filters(max(0, n_filters() - 1)) # needs to be updated here to trigger filtered_data()
      }
    })

    # apply filters to data_sub
    filtered_data <- shiny::reactive({
      shiny::req(data_sub())
      shiny::req(lapply(shiny::reactiveValuesToList(all_filters), function(x) {
        lapply(x, function(y) do.call(y, list()))
      }))
      df <- data_sub()
      n_filters() # triggers reevaluation whenever a filter is removed
      for (filter in names(all_filters)) {
        if (all_filters[[filter]]$filter_var() != "None") {
          filter_var <- rlang::sym(all_filters[[filter]]$filter_var())

          filter_val <- all_filters[[filter]]$filter_val()

          if (!is.null(filter_val)) {
            if (class(df[[rlang::as_name(filter_var)]]) == "Date") { # apply filter if filtering date
              df <- df %>%
                dplyr::filter(!!filter_var %in% seq(filter_val[1], filter_val[2], "day"))
            } else if ("N/A" %in% filter_val) { # apply filter if filtering for NAs
              df <- df %>%
                dplyr::filter(
                  is.na(!!filter_var) |
                    !!filter_var %in% filter_val[filter_val != "N/A"]
                )
            } else if (class(data()[[filter_var]]) == "factor") {    # apply filter for factors (dropping levels)
              df <- df %>%
                dplyr::mutate(!!filter_var := factor(!!filter_var, levels = filter_val))
            } else { # otherwise
              df <- df %>%
                dplyr::filter(!!filter_var %in% filter_val)
            }
          } else {
          }
        } else {
          df
        }
      }
      df
    })


    output$strat_choices <- shiny::renderUI({
      req(!errors_detected())
      shiny::req(available_var_opts)

      shiny::selectizeInput(
        inputId = ns("strat_vars"),
        label = "",
        choices = c(
          "None",
          available_var_opts()
        ),
        selected = "None",
        multiple = TRUE,
        options = list(maxItems = 3),
        width = "40%"
      )
    })

    # tracks the last selection made (starts as NULL)
    last_selection <- shiny::reactiveValues(d = NULL)

    # updating stratification choices, removing 'None' if any is chosen
    shiny::observeEvent(input$strat_vars,
                        {
                          Selected <- input$strat_vars

                          # finding lastest selection change
                          new_selection <- setdiff(Selected, last_selection$d)

                          if (length(new_selection) > 0) {
                            # if lastest selection is 'None', only keep 'None'
                            if (new_selection == "None") {
                              Selected <- "None"
                              # if latest selection is not 'None', keep everything except 'None'
                            } else {
                              Selected <- Selected[Selected != "None"]
                            }
                          }

                          # updating UI component
                          shiny::updateSelectizeInput(
                            session = session,
                            inputId = "strat_vars",
                            selected = Selected
                          )

                          # updating last selection
                          last_selection$d <<- Selected
                        },
                        ignoreNULL = FALSE
    )

    # apply signal detection on country level to the filtered data to check which algorithms are working
    # this is checking whether there is enough training data for the algorithm to compute a baseline
    algorithms_possible <- shiny::reactive({
      shiny::req(filtered_data)
      shiny::req(input$n_weeks)
      shiny::req(iv_weeks$is_valid())

      signals_all_methods <- dplyr::bind_rows(purrr::map(unlist(available_algorithms()), function(algorithm) {
        signals <- get_signals(filtered_data(),
                               method = algorithm,
                               number_of_weeks = input$n_weeks
        )
        if (!is.null(signals)) {
          signals <- signals %>% dplyr::mutate(method = algorithm)
        }
      }))

      algorithms_working <- unique(signals_all_methods$method)
      algorithms_working_named <- available_algorithms()[unlist(available_algorithms()) %in% algorithms_working]


      return(algorithms_working_named)
    })

    # implementing that the algorithm choice does not always move back to the default
    # farrington when the number of weeks is changed but stays with the last selected
    # algorithm as this algorithm is still working
    last_selected_algorithm <- shiny::reactiveVal("farrington")

    shiny::observeEvent(input$algorithm_choice, {
      last_selected_algorithm(input$algorithm_choice)
    })

    output$algorithm_choice <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::req(algorithms_possible)

      selected_algorithm <- last_selected_algorithm()
      # when last selected algorithm is no longer possible chose the first of the list
      if (!(selected_algorithm %in% algorithms_possible()) & length(algorithms_possible()) >= 1) {
        selected_algorithm <- algorithms_possible()[1]
      }

      if (length(algorithms_possible()) == 0) {
        return(shiny::tagList(
          br(),
          HTML("<b> For the selection you chose no algorithm is possible to apply. Please reduce the number of weeks you want generate alarms for or change the filters you set. </b>"),
          br()
        ))
      } else {
        return(shiny::selectInput(
          inputId = ns("algorithm_choice"),
          multiple = FALSE,
          label = "",
          selected = selected_algorithm,
          choices = algorithms_possible(),
          selectize = FALSE,
          width = "40%"
        ))
      }
    })

    no_algorithm_possible <- shiny::reactive({
      req(algorithms_possible)
      if (length(algorithms_possible()) == 0) {
        TRUE
      } else {
        FALSE
      }
    })

    # Return list of subsetted data and parameters
    return(list(
      data = reactive({
        dplyr::filter(filtered_data(), subset == TRUE)
      }),
      n_weeks = shiny::reactive(input$n_weeks),
      weeks_input_valid = shiny::reactive(iv_weeks$is_valid()),
      strat_vars = shiny::reactive({input$strat_vars}),
      pathogen_vars = shiny::reactive({input$pathogen_vars}),
      method = shiny::reactive({input$algorithm_choice}),
      no_algorithm_possible = shiny::reactive(no_algorithm_possible())
    ))
  })
}
