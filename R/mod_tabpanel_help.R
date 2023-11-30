

mod_tabpanel_help_ui <- function(id) {
  ns <- shiny::NS(id)
  
    shiny::tabPanel(
      "Help",
      shiny::h1("HowTo Use This App"),
      shiny::p("Follow the data flow in the tabs - from Data input to the Report on signal detections."),
      shiny::br(),
      shiny::p("Descriptions of required inputs etc."),
      icon = shiny::icon("question")
      )
    
}


mod_tabpanel_help_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

