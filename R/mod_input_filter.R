#' input_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_filter_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::div(
      id = id,
      shiny::uiOutput(ns("filter_var_ui")),
      shiny::uiOutput(ns("filter_val_ui")),
      tags$style(shiny::HTML(paste0("#", id, "-filter_var_ui{display:inline-block}"))),
      tags$style(shiny::HTML(paste0("#", id, "-filter_val_ui{display:inline-block; vertical-align: top;}"))),
      tags$style(shiny::HTML(paste0("#", id, "-filter_min_val_sel{display:inline-block}"))), # TODO: make these conditional html output
      tags$style(shiny::HTML(paste0("#", id, "-filter_max_val_sel{display:inline-block}"))),
    )
  )
}

#' input_filter Server Functions
#'
#' @noRd
mod_input_filter_server <- function(id, data, filter_opts){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # add date options
    date_opts <- shiny::reactive({
      date_opts <- intersect(names(data()), c("date_report")) # TODO: add other options
    })

    # filter variable
    output$filter_var_ui <- shiny::renderUI({
      shiny::selectInput(inputId = ns("filter_var_sel"),
                         multiple = FALSE,
                         label = "Choose variable to filter",
                         selected = "None",
                         choices = c("None", c(date_opts(), filter_opts())))
    })


    is_date <- shiny::reactive({
      shiny::req(input$filter_var_sel)
      class(data()[[input$filter_var_sel]]) == "Date"
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
      } else {
        if (class(data()[[input$filter_var_sel]]) == "factor") {
          filter_choices <- levels(data()[[input$filter_var_sel]]) # keep level ordering if factor
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
        )
      }
      value_ui
    })

    # return values to filter for
    filter_val <- shiny::reactive({
      shiny::req(is_date)
      if (is_date()) {
        values <- c(input$filter_min_val_sel, input$filter_max_val_sel)
      } else {
        values <- input$filter_val_sel
      }
      values
    })

    # return
    list(
      "filter_var" = shiny::reactive(input$filter_var_sel),
      "filter_val" = filter_val
    )

  })
}
