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
    icon = icon("viruses"),
    title = "Input parameters",
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        shiny::uiOutput(ns("input_tab_ui"))
      ),
      footer_text
    )
  )
}


#' tabpanel "input" Server Functions
#' @noRd
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
        datacheck_error_message
      } else {
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_title("Dataset settings", container = shiny::h1),
            shiny::h2("Pathogen"),
            shiny::uiOutput(ns("pathogen_choices")),
            shiny::br(),
            shiny::h2("Filters"),
            span("You can chose to investigate a subset of your data according to the filters you select. When filtering by date_report you have the possibility select a specific timeperiod you want to investigate. In the timeseries visualisation only the timeperiod you selected will be shown and the outbreak detection algorithms will only train on the data from the timeperiod you selected."),
            shiny::div(
              id = "filter_input",
              shiny::div(
                "Add and remove filters",
              ),
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
            mod_input_filter_ui(id = ns("filter0"))
          ),
          bslib::card(
            bslib::card_title("Signal Detection settings", container = shiny::h1),
            shiny::h2("Strata"),
            shiny::span("Select up to 3 variables you want to stratify by. Signals and visualisations will be generated for each stratum."),
            shiny::uiOutput(ns("strat_choices")),
            shiny::br(),
            shiny::h2("Signal detection period"),
            shiny::span("Set the number of weeks you want to generate signals for. The signals are generated for the most recent weeks."),
            shiny::uiOutput(ns("weeks_selection")),
            shiny::textOutput(ns("text_weeks_selection")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::h2("Signal detection algorithm"),
                shiny::span("Depending on the number of weeks you want to generate signals for and the filters you set, the choice of algorithms is automatically updated to those which are possible to apply for your settings."),
                shiny::uiOutput(ns("algorithm_choice"))
              ),
              shiny::column(
                width = 12,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'TRUE' || output['%s'] == 'TRUE'", ns("algorithm_glm"), ns("algorithm_farrington_chosen")),
                  shiny::span("Set a p-value"),
                  shiny::uiOutput(ns("p_value"))
                )
              ),
              shiny::column(
                width = 12,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'TRUE'", ns("algorithm_glm")),
                  checkboxInput(ns("pandemic_correction"), "Covid19 Pandemic Correction",
                    value = get_data_config_value(
                      "params:pandemic_correction",
                      FALSE, c(TRUE, FALSE)
                    )
                  )
                ),
                shiny::uiOutput(ns("conditional_date_input"))
              ),
              shiny::column(
                width = 12,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'FALSE'", ns("algorithm_glm")),
                  checkboxInput(ns("pad_signals_choice"), "Show expectation and threshold for historic data (computation intensive)")
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::h2("Post-processing filter"),
                shiny::span(
                  style = "display:block;",
                  "Apply filters after the execution of the signal detection."
                ),
                shiny::span(
                  style = "font-weight: bold; display:block; margin-top:12px; margin-bottom:4px;",
                  "Minimum number of cases per signal"
                ),
                shiny::div(
                  style = "margin-top:0px, padding-top:0px",
                  shiny::uiOutput(ns("filter_min_cases_signals"))
                )
              )
            )
          )
        )
      }
    })

    output$weeks_selection <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::numericInput(
        inputId = ns("n_weeks"),
        label = "",
        value = get_data_config_value("params:signal_detection_period", 6),
        min = 1,
        max = 52,
        step = 1,
        width = "40%"
      ) # TODO: make this dynamic
    })

    output$p_value <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::numericInput(
        inputId = ns("p_value"),
        label = NULL,
        value = as.numeric(
          sub(",", ".",get_data_config_value("params:p_value", 0.05))),
        min = 0.01,
        max = 0.2,
        step = 0.01,
        width = "40%"
      )
    })

    output$filter_min_cases_signals <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::numericInput(
        inputId = ns("min_cases_signals"),
        label = NULL,
        value = get_data_config_value("post-processing:min_cases_signals", 1),
        min = 1,
        step = 1,
        width = "40%"
      )
    })

    # using shinyvalidate to ensure value between min and max
    iv_weeks <- shinyvalidate::InputValidator$new()
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_required(
      message = "This input is required to be able to choose a signal detection algorithm."
    ))
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_integer())
    iv_weeks$add_rule("n_weeks", shinyvalidate::sv_between(1, 12))
    iv_weeks$enable()

    iv_p_value <- shinyvalidate::InputValidator$new()
    iv_p_value$add_rule("p_value", shinyvalidate::sv_numeric())
    iv_p_value$add_rule("p_value", shinyvalidate::sv_between(0.01, 0.2))
    iv_p_value$enable()

    iv_min_cases <- shinyvalidate::InputValidator$new()
    iv_min_cases$add_rule("min_cases_signals", shinyvalidate::sv_integer())
    iv_min_cases$add_rule("min_cases_signals", shinyvalidate::sv_gte(1))
    iv_min_cases$enable()

    output$text_weeks_selection <- shiny::renderText({
      shiny::req(!errors_detected())
      shiny::req(input$n_weeks)
      shiny::req(iv_weeks$is_valid())

      # subtracting 1 from input$n_weeks to get correct dates for flooring (issue #256)
      date_floor <- lubridate::floor_date(max(filtered_data()$date_report) - lubridate::weeks(input$n_weeks - 1),
        week_start = 1, unit = "week"
      )
      date_ceil <- lubridate::ceiling_date(max(filtered_data()$date_report), unit = "week", week_start = 7)

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
        label = "Select a pathogen",
        choices = unique(data()$pathogen),
        selected = get_data_config_value(
          "params:pathogen",
          unique(data()$pathogen)[1],
          unique(data()$pathogen)
        ),
        width = "40%"
      ))
    })

    # variable options for filter ui and strata selection
    available_var_opts <- shiny::reactive({
      shiny::req(data_sub())
      shiny::req(!errors_detected())
      available_vars <- data_sub() %>%
        dplyr::select(where(is.character) | where(is.factor)) %>%
        dplyr::select(-pathogen, -dplyr::all_of(dplyr::ends_with("_id"))) %>%
        names() %>%
        sort()
      available_vars
    })

    # adding date_report and age to the possible filter vars
    filter_var_opts <- shiny::reactive({
      shiny::req(available_var_opts())
      shiny::req(data_sub())
      date_opts <- intersect(names(data_sub()), c("date_report"))
      age_opts <- intersect(names(data_sub()), c("age"))
      all_opts <- c("None", date_opts, age_opts, available_var_opts())
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

    all_filters$filter0 <- filter0_reactives

    # add filters
    shiny::observeEvent(input$add_filter, {
      shiny::req(n_filters() < (length(filter_var_opts()) - 1))
      # id to add
      new_filter_id <- paste0("filter", n_filters())
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
    })

    # remove last filter added
    shiny::observeEvent(input$remove_filter, {
      shiny::req(n_filters() > 0)
      # id to remove
      remove_filter_id <- tail(names(all_filters), 1)
      # remove ui
      shiny::removeUI(
        selector = paste0("#", ns(remove_filter_id)),
        immediate = TRUE
      )
      # remove parameters
      removeReactiveValuesIndex(all_filters, remove_filter_id)

      # update filter count
      n_filters(n_filters() - 1) # needs to be updated here to trigger filtered_data()
    })


    # Apply filters
    filtered_data <- shiny::reactive({
      shiny::req(data_sub())

      df <- data_sub()
      n_filters() # Ensures reactivity

      # reset levels
      app_cache_env$sex_levels <- c("male", "female", "diverse", NA_character_)
      app_cache_env$age_group_levels <- create_age_group_levels(df)

      for (filter in names(all_filters)) {
        params <- all_filters[[filter]]

        if (params$filter_var() == "None" | is.null(params$filter_val())) next

        filter_var <- rlang::sym(params$filter_var())
        filter_val <- params$filter_val()

        df <- if (lubridate::is.Date(df[[rlang::as_name(filter_var)]])) {
          dplyr::filter(df, !!filter_var %in% seq(filter_val[1], filter_val[2], "day"))
        } else if (rlang::as_name(filter_var) == "age") {
          dplyr::filter(df, between(!!filter_var, filter_val[1], filter_val[2]))
        } else {
          dplyr::filter(df, !!filter_var %in% filter_val |
            (is.na(!!filter_var) & "unknown" %in% filter_val))
        }
      }

      # update levels
      sex_levels_char <- as.character(unique(df$sex))
      filtered_levels <- if (any(is.na(df$sex))) c(sex_levels_char, NA_character_) else sex_levels_char
      app_cache_env$sex_levels <- intersect(app_cache_env$sex_levels, filtered_levels)
      app_cache_env$age_group_levels <- create_age_group_levels(df)

      df
    })


    output$strat_choices <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::req(available_var_opts)

      shiny::selectizeInput(
        inputId = ns("strat_vars"),
        label = "",
        choices = c(
          "None",
          available_var_opts()
        ),
        selected = get_data_config_value("params:strata", "None", available_var_opts()),
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
          if (any(new_selection == "None")) {
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

      # checking whether data has any rows after filtering
      if (nrow(filtered_data()) < 1) {
        return(NULL)
      }
      # compute based on the data when which algorithms are possible
      min_max_date <- get_min_max_date(filtered_data())
      algorithms_working <- get_possible_methods(min_max_date[["min_date"]], min_max_date[["max_date"]], number_of_weeks = input$n_weeks)

      algorithms_working_named <- available_algorithms()[unlist(available_algorithms()) %in% algorithms_working]


      return(algorithms_working_named)
    })

    # implementing that the algorithm choice does not always move back to the default
    # farrington when the number of weeks is changed but stays with the last selected
    # algorithm as this algorithm is still working
    last_selected_algorithm <- shiny::reactiveVal(
      get_data_config_value("params:signal_detection_algorithm", "farrington")
    )

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
          HTML("<b> It is not possible to apply any algorithm for the settings you chose. Please reduce the number of weeks you want generate signals for or change the filters you set. </b>"),
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
          width = "90%"
        ))
      }
    })

    algorithm_glm <- reactive({
      shiny::req(!errors_detected())
      shiny::req(input$algorithm_choice)

      if (grepl("glm", input$algorithm_choice)) {
        TRUE
      } else {
        FALSE
      }
    })

    time_trend <- reactive({
      shiny::req(!errors_detected())
      shiny::req(input$algorithm_choice)
      shiny::req(algorithm_glm)

      if (algorithm_glm() && grepl("timetrend", input$algorithm_choice)) {
        TRUE
      } else {
        FALSE
      }
    })

    output$algorithm_glm <- renderText({
      algorithm_glm() # This will return "TRUE" or "FALSE" as a string
    })

    # Force the output to be sent to the client even if not rendered in UI
    # this needs to be here otherwise the conditionalPanel for the tickbox is not evaluated!
    outputOptions(output, "algorithm_glm", suspendWhenHidden = FALSE)

    # Observe changes in algorithm_choice to reset pandemic_correction checkbox to FALSE when other algorithm is selected
    observeEvent(input$algorithm_choice, {
      if (!algorithm_glm()) {
        updateCheckboxInput(session, "pandemic_correction", value = FALSE)
      }
    })

    # Output (not seen in UI) for FarringtonFlexible p-value output
    algorithm_farrington_chosen <- reactive({
      shiny::req(!errors_detected())
      shiny::req(input$algorithm_choice)

      if (grepl("farrington", input$algorithm_choice, ignore.case = TRUE)) {
        TRUE
      } else {
        FALSE
      }
    })

    output$algorithm_farrington_chosen <- renderText({
      algorithm_farrington_chosen() # This will return "TRUE" or "FALSE" as a string
    })

    # Force the output to be sent to the client even if not rendered in UI
    # this needs to be here otherwise the conditionalPanel for the input box is not evaluated!
    outputOptions(output, "algorithm_farrington_chosen", suspendWhenHidden = FALSE)

    # Conditional UI for date input
    output$conditional_date_input <- shiny::renderUI({
      if (isTRUE(input$pandemic_correction)) {
        valid_dates <- valid_dates_intervention()
        if (is.null(valid_dates$valid_start_date)) {
          shiny::p("Your dataset does not have sufficient number of weeks to do a pandemic correction.")
        } else {
          intervention_date_config <- as.Date(get_data_config_value("params:intervention_date"))
          valid_intervention_interval <- lubridate::interval(valid_dates$valid_start_date, valid_dates$valid_end_date)

          if (isTRUE(intervention_date_config %within% valid_intervention_interval)) {
            default_intervention_date <- intervention_date_config
          } else {
            default_intervention_date <- valid_dates$default_intervention
          }

          shiny::dateInput(ns("intervention_date"), "Choose the date when you first notice a significant change in the number of cases.",
            value = default_intervention_date, min = valid_dates$valid_start_date, max = valid_dates$valid_end_date,
            width = "90%"
          )
        }
      } else {
        NULL
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

    # Reactive expression for intervention start date
    intervention_date <- shiny::reactive({
      if (!is.null(input$pandemic_correction) && input$pandemic_correction && algorithm_glm()) {
        input$intervention_date
      } else {
        NULL
      }
    })

    # get default, min and max dates for intervention date
    valid_dates_intervention <- shiny::reactive({
      get_valid_dates_intervention_start(filtered_data(), number_of_weeks = input$n_weeks, time_trend = time_trend())
    })

    # Return list of subsetted data and parameters
    return(list(
      filtered_data = reactive({
        dplyr::filter(filtered_data(), subset == TRUE)
      }),
      n_weeks = shiny::reactive(input$n_weeks),
      weeks_input_valid = shiny::reactive(iv_weeks$is_valid()),
      p_value = shiny::reactive(input$p_value),
      #p_value_input_valid = shiny::reactive(iv_p_value$is_valid()),
      strat_vars = shiny::reactive(input$strat_vars),
      pathogen_vars = shiny::reactive(input$pathogen_vars),
      method = shiny::reactive(input$algorithm_choice),
      no_algorithm_possible = shiny::reactive(no_algorithm_possible()),
      intervention_date = shiny::reactive(intervention_date()),
      pad_signals_choice = shiny::reactive(input$pad_signals_choice),
      min_cases_signals = shiny::reactive(input$min_cases_signals)
    ))
  })
}
