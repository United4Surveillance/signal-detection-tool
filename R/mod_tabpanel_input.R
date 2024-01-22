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
    # Horizontal line ----
    tags$hr(),

    h2("Choose the number of weeks to generate signals for"),
    shiny::uiOutput(ns("weeks_slider")),


    h2("Choose which pathogen in the dataset to check for aberrations"),
    br(),

    shiny::uiOutput(ns("pathogen_choices")),

    h2("Choose stratification parameters (max. 3)"),
    br(),

    shiny::uiOutput(ns("strat_choices")),

    icon = icon("viruses")
  )
}


#' tabpanel "input" Server Functions
#' @param id,input,output,session standard \code{shiny} boilerplate
#' @param data reactive input dataset preprocessed if no errors
#' @param errors_detected reactive boolean, when TRUE errors on mandatory variables where detected
mod_tabpanel_input_server <- function(id, data, errors_detected){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$weeks_slider <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::sliderInput(inputId = ns("n_weeks"),
                         label = "",
                         value = 6,
                         min = 1,
                         max = 52) #TODO: make this dynamic
    })

    # shiny::observe({ print("input:"); print(head(data())) })


    ## showing options in ui
    output$pathogen_choices <- shiny::renderUI({
      shiny::req(!errors_detected())
      return(shiny::selectInput(inputId = ns("pathogen_vars"),
                                label = "Choose pathogen:",
                                choices = unique(data()$pathogen))
      )

    })


    # strata
    strata_var_opts <- shiny::reactive({
      shiny::req(data)
      shiny::req(!errors_detected())
      available_vars <- intersect(c("state",
                                    "county",
                                    "regional_level1",
                                    "regional_level2",
                                    "regional_level3",
                                    "subtype",
                                    "age_group",
                                    "sex"),
                                  names(data())) %>%
        sort()
      available_vars
    })

    output$strat_choices <- shiny::renderUI({
      req(!errors_detected())

      shiny::selectizeInput(inputId = ns("strat_vars"),
                            label = "Parameters to stratify by:",
                            choices = c("None",
                                        strata_var_opts()),
                            selected = "None",
                            multiple = TRUE,
                            options = list(maxItems = 3))
    })

    # Return list of subsetted data and parameters
    return(
      # list(data = reactive({ dplyr::filter(data(), subset == TRUE) }),
      list(data = shiny::reactive(data()),
           n_weeks = shiny::reactive(input$n_weeks),
           strat_vars = shiny::reactive(input$strat_vars),
           pathogen_vars = shiny::reactive(input$pathogen_vars))
    )

  })

}
