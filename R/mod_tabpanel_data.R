

mod_tabpanel_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Data",
    # From runExample("09_upload")
    shiny::h2("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Input: Select a file ----
        shiny::fileInput(NS(id,"file1"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        shiny::checkboxInput(NS(id,"header"), "Header", TRUE),

        # Input: Select separator ----
        shiny::radioButtons(NS(id,"sep"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        shiny::radioButtons(NS(id,"quote"), "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),

        # Horizontal line ----
        tags$hr(),

        # Input: Select number of rows to display ----
        shiny::radioButtons(NS(id,"disp"), "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"),

        # Horizontal line ----
        tags$hr(),

        # Input: Specify dates?
        shiny::checkboxInput(NS(id,"dates_bin"), "Limit date interval"),

        # Input: Select minimum date
        shiny::dateInput(NS(id,"min_date"),"Minimum date:", value = "2023-01-01"),

        # Input: Select maximum date
        shiny::dateInput(NS(id,"max_date"),"Maximum date:"),

        # Horizontal line ----
        tags$hr(),

        # Input: Sort data according to date_report
        shiny::checkboxInput(NS(id,"sort_bin"), "Sort data according to date")

      ),

      # Main panel for displaying outputs ----
      mainPanel(

          # Output: Data file ----
          shiny::tableOutput(NS(id,"contents"))

      )
    ),
    icon = icon("file")
  )
}


mod_tabpanel_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data
    values <- shiny::reactiveValues(df_data = NULL)

    shiny::observe({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      req(input$file1)

      print(input$file1)

      file_type <- tools::file_ext(input$file1$name)

      if (tolower(file_type) == "csv") {
        # OBS: need to include possibility to read Excel (and .R?)
        indata <- read.csv(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
      }

      ## NOTE - example, to be removed! ###
      indata$pathogen[1] <- "E. coli"
      indata$pathogen[2] <- "Campylobacter"
      #####################################

      # preprocess
      indata <- indata %>% dplyr::mutate(date_onset = ifelse(is.na(date_onset) | date_onset=="", date_report, date_onset))
      indata$age_group <- factor(indata$age_group, levels = stringr::str_sort(unique(indata$age_group), numeric = TRUE))
      indata$sex <- factor(indata$sex)

      if (input$dates_bin) {
        indata <- subset(indata, dplyr::between(as.Date(date_report), input$min_date, input$max_date))
      }

      values$df_data <- indata

    })

    output$contents <- shiny::renderTable({
      tmp_data <- values$df_data

      if (input$sort_bin) {
        tmp_data <- tmp_data %>% dplyr::arrange(date_report)
      }

      if(input$disp == "head") {
        head(tmp_data, 10)
      }
      else {
        tmp_data
      }
    })
  })
  return(values)

}

