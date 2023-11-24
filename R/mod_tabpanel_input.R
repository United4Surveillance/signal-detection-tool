mod_tabpanel_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Input parameters"

    ,h2("Choose which pathogen in the dataset to check for aberrations")
    ,br()

    ,shiny::uiOutput(NS(id,"pathogen_choices"))

    ,h2("Choose stratification parameters")
    ,br()

    ,shiny::uiOutput(NS(id,"strat_choices"))

    ,icon = icon("viruses")
  )
}


mod_tabpanel_input_server <- function(id, indata) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## showing options in ui
    output$pathogen_choices <- shiny::renderUI({

      return(shiny::checkboxGroupInput(inputId = NS(id,"pathogen_vars"),
                                       label = "Choose pathogen:",
                                       choices = unique(indata$pathogen),
                                       selected = unique(indata$pathogen))
             )

    })

    output$strat_choices <- shiny::renderUI({

      return(shiny::selectInput(inputId = NS(id,"strat_vars"),
                                label = "Parameters to stratify by:",
                                choices = c("None","All",names(indata)), # needs robustness!!
                                selected = NULL,
                                multiple = TRUE)
             )
    })

    # reacting based on decisions done in ui
    # from 'pathogen_vars' and 'strat_vars'
    # shiny::eventReactive(eventExpr = input$pathogen_vars, {
    #   path_vars <<- input$pathogen_vars
    #   indata <<- indata %>% filter(pathogen %in% input$pathogen_vars)
    # })

    shiny::observeEvent(input$pathogen_vars, {
      tmp <- values$df_data %>% filter(pathogen %in% input$pathogen_vars)
      values$df_data <- tmp
    })

    # shiny::eventReactive(eventExpr = input$strat_vars, {
    #   strat_vars <- input$strat_vars
    #   if ("None" %in% input$strat_vars) {
    #     strat_vars <<- c()
    #   }
    #
    #   # 'None' takes precendence to 'All'
    #   else if ("All" %in% input$strat_vars) {
    #     strat_vars <<- names(indata)
    #   }
    # })

  })

}
