#' input_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_filter_ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    shiny::div(
      id = id,
      shiny::uiOutput(ns("filter_var_ui")),
      shiny::br(),
      shiny::uiOutput(ns("filter_val_ui")),
      shiny::br(),
      tags$style(shiny::HTML(paste0("#", id, "-filter_var_ui{display:inline-block}"))),
      tags$style(shiny::HTML(paste0("#", id, "-filter_val_ui{display:inline-block; vertical-align: top;}"))),
      tags$style(shiny::HTML(paste0("#", id, "-filter_min_val_sel{display:inline-block; vertical-align: top;}"))), # TODO: make these conditional html output
      tags$style(shiny::HTML(paste0("#", id, "-filter_max_val_sel{display:inline-block; vertical-align: top;}")))
    )
  )
}

#' input_filter Server Functions
#'
#' @noRd
mod_input_filter_server <- function(id, data, filter_opts, all_filters, n_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # filter variable
    output$filter_var_ui <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("filter_var_sel"),
        multiple = FALSE,
        label = "Choose variable to filter",
        selected = "None",
        choices = filter_opts
        # width = "100%"
      )
    })


    is_date <- shiny::reactive({
      shiny::req(input$filter_var_sel)
      class(data()[[input$filter_var_sel]]) == "Date"
    })

    # This can be transformed is_numeric, if we allow other numerical variables to the filtering in some point
    is_age <- shiny::reactive({
      shiny::req(input$filter_var_sel)
      input$filter_var_sel == "age"
    })

    # filter value
    output$filter_val_ui <- shiny::renderUI({
      shiny::req(input$filter_var_sel != "None")
      shiny::req(is_date)

      if (is_date()) {
        value_ui <- shiny::tagList(
          shiny::dateInput(
            inputId = ns("filter_min_val_sel"),
            label = "Minimum Date",
            value = min(data()[[input$filter_var_sel]]),
            min = min(data()[[input$filter_var_sel]]),
            max = max(data()[[input$filter_var_sel]])
          ),
          shiny::dateInput(
            inputId = ns("filter_max_val_sel"),
            label = "Maximum Date",
            min = min(data()[[input$filter_var_sel]]),
            max = max(data()[[input$filter_var_sel]]),
            value = max(data()[[input$filter_var_sel]])
          ),
        )
      } else if(is_age()) {

        value_ui <- shiny::tagList(
          shiny::div(
            shiny::numericInput(
              inputId = ns("filter_min_val_sel"),
              label = "Minimum Age",
              value = min(data()[[input$filter_var_sel]], na.rm = T),
              min = min(data()[[input$filter_var_sel]], na.rm = T),
              max = max(data()[[input$filter_var_sel]], na.rm = T)
              ),
            style = "display: inline-block; width: 45%; vertical-align: top;"),
          shiny::div(
            shiny::numericInput(
              inputId = ns("filter_max_val_sel"),
              label = "Maximum Age",
              min = min(data()[[input$filter_var_sel]], na.rm = T),
              max = max(data()[[input$filter_var_sel]], na.rm = T),
              value = max(data()[[input$filter_var_sel]], na.rm = T)
            ),
            style = "display: inline-block; width: 45%; vertical-align: top;")
        )
      } else {
        if (class(data()[[input$filter_var_sel]]) == "factor") {
          filter_choices <- levels(data()[[input$filter_var_sel]]) # keep level ordering if factor
          # without transforming NA to unknown in the data as we want to keep NA as long as possible
          # for easy computation
          if(any(is.na(data()[[input$filter_var_sel]]))){
            filter_choices <- c(filter_choices,"unknown")
          }
        } else {
          filter_choices <- data() %>%
            dplyr::pull(input$filter_var_sel) %>%
            as.character() %>%
            tidyr::replace_na("N/A") %>%
            unique()
          filter_choices
        }
        value_ui <- shiny::selectInput(
          inputId = ns("filter_val_sel"),
          multiple = TRUE,
          label = "Choose values to filter for",
          choices = filter_choices
          # width = "40%"
        )
      }
      value_ui
    })

    # return values to filter for
    filter_val <- shiny::reactive({
      shiny::req(is_date)
      shiny::req(is_age)
      if (is_date()) {
        values <- c(input$filter_min_val_sel, input$filter_max_val_sel)
      } else if(is_age()){
        values <- c(input$filter_min_val_sel, input$filter_max_val_sel)
      } else {
        values <- input$filter_val_sel
      }
      values
    })

    # Logic for filtering for age
    shiny::observeEvent(input$filter_min_val_sel, {
      # Ensure the input is not empty or NULL
      if (!is.null(input$filter_min_val_sel) && !is.null(input$filter_max_val_sel)) {
        min_val <- input$filter_min_val_sel
        max_val <- input$filter_max_val_sel

        # If the minimum value exceeds the maximum, reset it to the maximum
        if (!is.na(min_val) && min_val > max_val) {
          updateNumericInput(session, "filter_min_val_sel", value = max_val)
        } else {
          # Otherwise, just update the max limit for the minimum input
          updateNumericInput(session, "filter_max_val_sel", min = min_val)
        }
      }
    })

    shiny::observeEvent(input$filter_max_val_sel, {
      # Ensure the input is not empty or NULL
      if (!is.null(input$filter_min_val_sel) && !is.null(input$filter_max_val_sel)) {
        min_val <- input$filter_min_val_sel
        max_val <- input$filter_max_val_sel

        # If the maximum value is less than the minimum, reset it to the minimum
        if (!is.na(max_val) && max_val < min_val) {
          updateNumericInput(session, "filter_max_val_sel", value = min_val)
        } else {
          # Otherwise, just update the min limit for the maximum input
          updateNumericInput(session, "filter_min_val_sel", max = max_val)
        }
      }
    })

    # update filter_var choices depending on all_filters (n_filters is required
    # to trigger event when a filter is removed from all_filters)
    shiny::observeEvent(
      {
        c(lapply(shiny::reactiveValuesToList(all_filters), function(x) {
          do.call(x[["filter_var"]], list())
        }), n_filters())
      },
      {
        shiny::req(input$filter_var_sel)
        taken_vars <- lapply(shiny::reactiveValuesToList(all_filters), function(x) {
          do.call(x[["filter_var"]], list())
        }) %>% unlist()
        taken_vars <- taken_vars[!taken_vars %in% c("None", input$filter_var_sel)]
        remaining_vars <- filter_opts[!filter_opts %in% taken_vars]
        shiny::updateSelectInput(
          inputId = "filter_var_sel",
          selected = input$filter_var_sel,
          choices = remaining_vars
        )
      }
    )

    # return
    list(
      "filter_var" = shiny::reactive(input$filter_var_sel),
      "filter_val" = filter_val
    )
  })
}
