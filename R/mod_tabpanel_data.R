#' tabpanel "data" UI Function
#'
#' @description A shiny Module for a tab to load in data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabpanel_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Data",
    # From runExample("09_upload")
    shiny::h2("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Input: Select a file ----
        shiny::fileInput(ns("file1"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        shiny::checkboxInput(ns("header"), "Header", TRUE),

        # Input: Select separator ----
        shiny::radioButtons(ns("sep"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        shiny::radioButtons(ns("quote"), "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
      ),

      # Main panel for displaying outputs ----
      mainPanel(
          # Output: Data file ----
          DT::dataTableOutput(ns("contents"))
      )
    ),
    icon = icon("file")
  )
}

#' tabpanel "data" Server Functions
#'
#' @noRd
mod_tabpanel_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data and preprocess data
    data <- shiny::reactive({
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

      # preprocess
      indata <- indata %>%
        dplyr::mutate(date_onset =
                        ifelse(is.na(date_onset) | date_onset == "",
                                          date_report, date_onset))
      indata$age_group <- factor(indata$age_group, levels = stringr::str_sort(unique(indata$age_group), numeric = TRUE))
      indata$sex <- factor(indata$sex)

      return(indata)
    })

    # Data preview table ----
    output$contents <- DT::renderDataTable({
      req(data)
      data()
    })

    # Return a reactive data set from this server that can be passed
    # along to subsequent tab modules
    return(data)

  })

  # data_out <- dplyr::filter(data(), subset == TRUE)



}

