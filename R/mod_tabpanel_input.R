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

    # Input date range
    # Input: Specify dates?
    shiny::checkboxInput(ns("dates_bin"), "Limit date interval"),
    # Input: Select minimum date
    shiny::uiOutput(ns("min_date_choice")),
    # Input: Select maximum date
    shiny::uiOutput(ns("max_date_choice")),

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

    # date uiOutputs default choices from data
    output$min_date_choice <- shiny::renderUI({
      return(shiny::dateInput(inputId = ns("min_date"), label = "Minimum date:",
                              value = min(data()$date_report),
                              min = min(data()$date_report),
                              max = max(data()$date_report),
                              weekstart = 1)
      )
    })
    output$max_date_choice <- shiny::renderUI({
      return(shiny::dateInput(inputId = ns("max_date"), label = "Maximum date:",
                              value = max(data()$date_report),
                              min = min(data()$date_report),
                              max = max(data()$date_report),
                              weekstart = 1)
      )
    })

    data_sub <- shiny::reactive({
      req(data)
      req(!errors_detected())
      dat <- data()
      # data range limit or pick everything
      dat <- dplyr::mutate(dat, subset = TRUE)
      if (input$dates_bin) {
        dat <- dplyr::mutate(dat,
                             subset = dplyr::between(as.Date(date_report),
                                                     input$min_date,
                                                     input$max_date))
      }

      # add subset indicator for selected pathogens
      dat <- dplyr::mutate(dat,
                           subset = subset &
                             (pathogen %in% input$pathogen_vars))
      return(dat)
    })

    ## showing options in ui
    output$pathogen_choices <- shiny::renderUI({
      req(!errors_detected())
      return(shiny::selectInput(inputId = ns("pathogen_vars"),
                                label = "Choose pathogen:",
                                choices = unique(data()$pathogen))
      )

    })


    # strata
    strata_var_opts <- shiny::reactive({
      shiny::req(data_sub)
      shiny::req(!errors_detected())
      available_vars <- intersect(c("state",
                                    "county",
                                    "regional_level1",
                                    "regional_level2",
                                    "regional_level3",
                                    "subtype",
                                    "age_group",
                                    "sex"),
                                  names(data_sub())) %>%
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

    # tracks the last selection made (starts as NULL)
    last_selection <- shiny::reactiveValues(d = NULL)

    # updating stratification choices, removing 'None' if any is chosen
    shiny::observeEvent(input$strat_vars, {
      Selected = input$strat_vars

      # finding lastest selection change
      new_selection <- setdiff(Selected, last_selection$d)

      if (length(new_selection) > 0) {
        # if lastest selection is 'None', only keep 'None'
        if (new_selection == 'None') {
          Selected = 'None'
        # if latest selection is not 'None', keep everything except 'None'
        } else {
          Selected = Selected[Selected != 'None']
        }
      }

      # updating UI component
      shiny::updateSelectizeInput(session = session,
                               inputId = 'strat_vars',
                               selected = Selected)

      # updating last selection
      last_selection$d <<- Selected

    }, ignoreNULL = F)

    # Return list of subsetted data and parameters
    return(
      list(data = reactive({ dplyr::filter(data_sub(), subset == TRUE) }),
           strat_vars = reactive({ input$strat_vars }),
           pathogen_vars = reactive({ input$pathogen_vars }))
    )

  })

}
