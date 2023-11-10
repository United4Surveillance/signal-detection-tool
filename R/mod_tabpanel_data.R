

mod_tabpanel_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Data",
    # From runExample("09_upload")
    h2("Load Data"),
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        fileInput(NS(id,"file1"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        checkboxInput(NS(id,"header"), "Header", TRUE),

        # Input: Select separator ----
        radioButtons(NS(id,"sep"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        radioButtons(NS(id,"quote"), "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),

        # Horizontal line ----
        tags$hr(),

        # Input: Select number of rows to display ----
        radioButtons(NS(id,"disp"), "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),

        # Horizontal line ----
        tags$hr(),

        # Input: Specify dates?
        checkboxInput(NS(id,"dates_bin"), "Limit date interval"),

        # Input: Select minimum date
        dateInput(NS(id,"min_date"),"Minimum date:", value = "2023-01-01"),

        # Input: Select maximum date
        dateInput(NS(id,"max_date"),"Maximum date:"),

        # Horizontal line ----
        tags$hr(),

        # Input: Sort data according to date_report
        checkboxInput(NS(id,"sort_bin"), "Sort data according to date")

      ),

      # Main panel for displaying outputs ----
      mainPanel(

          # Output: Data file ----
          tableOutput(NS(id,"contents"))

      )
    ),
    icon = icon("file")
  )
}


mod_tabpanel_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data - from runExample("09_upload")
    output$contents <- renderTable({

      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      req(input$file1)

      print(input$file1)

      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)

      if (input$dates_bin) {
        df <- subset(df, dplyr::between(as.Date(date_report), input$min_date, input$max_date))
      }

      if (input$sort_bin) {
        df <- df %>% dplyr::arrange(date_report)
      }

      if(input$disp == "head") {
        return(head(df, 10))
      }
      else {
        return(df)
      }

    })
  })
}

