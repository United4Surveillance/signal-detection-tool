mod_tabpanel_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Input parameters",

    h2("Choose which pathogen in the dataset to check for aberrations"),
    br(),
    #selectInput("pathogen", "Choose pathogen", c("-", "covid-19", "E-coli", "Salmonella", "Pertussis"), selected = "-"),
    uiOutput(NS(id,"path_checkbox")),
    icon = icon("viruses")
  )
}


mod_tabpanel_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$path_checkbox <- renderUI({
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)

      if (input$dates_bin) {
        df <- subset(df, between(as.Date(date_report), input$min_date, input$max_date))
      }


      return(Map(function(x) checkboxGroupInput(NS(id,"pathogen"),"Choose pathogen:",x, selected = x), unique(df$pathogen)))

    })
  })
}
