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

    h2("Filter dataset"),
    br(),
    shiny::uiOutput(ns("filter_variables")),
    shiny::uiOutput(ns("filter_values")),
    tags$style(shiny::HTML(paste0("#", id, "-filter_variables{display:inline-block}"))),
    tags$style(shiny::HTML(paste0("#", id, "-filter_values{display:inline-block}"))),

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
      return(shiny::selectInput(inputId = ns("pathogen_vars"),
                                label = "Choose pathogen:",
                                choices = unique(data()$pathogen))
      )

    })


    # variable options for filter ui and strata selection
    available_var_opts <- shiny::reactive({
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

    output$filter_variables <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::req(available_var_opts)
      shiny::selectInput(inputId = ns("filter_variable"),
                         multiple = FALSE,
                         label = "Choose variable to filter",
                         selected = "None",
                         choices = c("None", available_var_opts()))
    })

    output$filter_values <- shiny::renderUI({
      shiny::req(!errors_detected())
      shiny::req(input$filter_variable != "None")

      # keep level ordering if factor
      if (class(data_sub()[[input$filter_variable]]) == "factor") {
        filter_choices <- levels(data_sub()[[input$filter_variable]])
      } else {
        filter_choices <- data_sub() %>%
          dplyr::pull(input$filter_variable) %>%
          as.character() %>%
          tidyr::replace_na("N/A") %>%
          unique()
        filter_choices
      }

      shiny::selectInput(
        inputId = ns("filter_values"),
        multiple = TRUE,
        label = "Choose values to filter for",
        choices = filter_choices
      )

    })

    filtered_data <- shiny::reactive({
      shiny::req(input$filter_variable)

      if (input$filter_variable == "None" | is.null(input$filter_values)) {
        df <- data_sub()
      } else {
        filter_var <- rlang::sym(input$filter_variable)
        if ("N/A" %in% input$filter_values) {
          df <- data_sub() %>%
            dplyr::filter(
              is.na(!!filter_var) |
                !!filter_var %in% input$filter_values[input$filter_values != "N/A"])
        } else {
          df <- data_sub() %>%
            dplyr::filter(!!filter_var %in% input$filter_values)
        }
      }
      df
    })

    output$strat_choices <- shiny::renderUI({
      req(!errors_detected())
      shiny::req(available_var_opts)

      shiny::selectizeInput(inputId = ns("strat_vars"),
                            label = "Parameters to stratify by:",
                            choices = c("None",
                                        available_var_opts()),
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

    }, ignoreNULL = FALSE)

    # Return list of subsetted data and parameters
    return(
        list(data = reactive({ dplyr::filter(filtered_data(), subset == TRUE) }),
             n_weeks = shiny::reactive(input$n_weeks),
             strat_vars = reactive({ input$strat_vars }),
             pathogen_vars = reactive({ input$pathogen_vars }))
    )

  })

}
