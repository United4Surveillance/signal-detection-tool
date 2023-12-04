#' tabpanel "input" UI Function
#'
#' @description A shiny Module for a tab to select input parameters for analyses in the shiny app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Input parameters",
    # Horizontal line ----
    tags$hr(),

    # Input date range
    # Input: Specify dates?
    shiny::checkboxInput(ns("dates_bin"), "Limit date interval"),
    # Input: Select minimum date
    shiny::dateInput(ns("min_date"), "Minimum date:",
                     value = "2023-01-01"),
    # Input: Select maximum date
    shiny::dateInput(ns("max_date"), "Maximum date:"),

    h2("Choose which pathogen in the dataset to check for aberrations"),
    br(),

    shiny::uiOutput(ns("pathogen_choices")),

    h2("Choose stratification parameters"),
    br(),

    shiny::uiOutput(ns("strat_choices")),

    icon = icon("viruses")
  )
}

#' tabpanel "input" Server Functions
#'
#' @noRd
mod_tabpanel_input_server <- function(id, indata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_sub <- shiny::reactive({
      req(indata)
      dat <- indata()
      # data range limit or pick everything
      dat <- dplyr::mutate(dat, subset = TRUE)
      if (input$dates_bin) {
        dat <- dplyr::mutate(dat,
                             subset = dplyr::between(as.Date(date_report),
                                                     input$min_date,
                                                     input$max_date))
      }

      print(head(dat))

      # add subset indicator for selected pathogens
      dat <- dplyr::mutate(dat,
                           subset = subset &
                             (pathogen %in% input$pathogen_vars))
      return(dat)
    })

    shiny::observe({ print("input:"); print(head(data_sub())) })

    ## showing options in ui
    output$pathogen_choices <- shiny::renderUI({
      return(shiny::selectInput(inputId = ns("pathogen_vars"),
                                       label = "Choose pathogen:",
                                       choices = unique(indata()$pathogen))
             )
    })

    output$strat_choices <- shiny::renderUI({
      return(shiny::selectInput(inputId = ns("strat_vars"),
                                label = "Parameters to stratify by:",
                                choices = c("None",
                                            # "All", # not sensible?
                                            names(indata())),
                                # needs robustness!!
                                selected = "None",
                                multiple = TRUE)
             )
      print(c("input-strat_vars", input$strat_vars))
    })

    # Return list of subsetted data and parameters
    return(
      list(data = reactive({ dplyr::filter(data_sub(), subset == TRUE) }),
           strat_vars = reactive({ input$strat_vars }))
      )

  })

}
