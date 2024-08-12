library(shiny)


ui <- fluidPage(
  # Add a selectInput for algorithm choice
  selectInput("algorithm_choice", "Select Algorithm", choices = c("None", "algorithm_glm", "other")),
  conditionalPanel(
    condition = "input.algorithm_choice == 'algorithm_glm'",
    checkboxInput("pandemic_correction", "Covid19 Pandemic Correction", value = FALSE)
  ),
  uiOutput("conditional_date_input"),
  textOutput("intervention_display")
)


server <- function(input, output, session) {
  ns <- session$ns

  # Observe changes in algorithm_choice to reset pandemic_correction checkbox
  observeEvent(input$algorithm_choice, {
    if (input$algorithm_choice != "algorithm_glm") {
      updateCheckboxInput(session, "pandemic_correction", value = FALSE)
    }
  })

  # Conditional UI for date input
  output$conditional_date_input <- renderUI({
    if (isTRUE(input$pandemic_correction)) {
      dateInput(ns("intervention_start_date"), "Select a Date when the pandemic started", value = NULL)
    } else {
      NULL
    }
  })

  # Reactive expression for intervention start date
  intervention_start_date <- shiny::reactive({
    print("inside intervention start")
    if (!is.null(input$pandemic_correction) && input$pandemic_correction && input$algorithm_choice == "algorithm_glm") {
      print("inside we give it the start value")
      req(input$intervention_start_date)
      input$intervention_start_date
      #print(intervention_start_date)
    } else {
      print("inside return NULL")
      NULL
    }
  })

  # Display the intervention start date
  output$intervention_display <- renderText({
    date <- intervention_start_date()
    if (is.null(date)) {
      "No intervention start date selected."
    } else {
      paste("Intervention starts on:", date)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

