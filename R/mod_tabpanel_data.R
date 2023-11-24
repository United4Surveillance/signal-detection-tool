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
                     selected = '"'),

        # Horizontal line ----
        tags$hr(),

        # Input: Specify dates?
        shiny::checkboxInput(ns("dates_bin"), "Limit date interval"),

        # Input: Select minimum date
        shiny::dateInput(ns("min_date"), "Minimum date:",
                         value = "2023-01-01"),

        # Input: Select maximum date
        shiny::dateInput(ns("max_date"), "Maximum date:"),

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

    # Load data
    # values <- shiny::reactiveValues(df_data = NULL)

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
        dplyr::mutate(date_onset = ifelse(is.na(date_onset) | date_onset == "",
                                          date_report, date_onset))
      indata$age_group <- factor(indata$age_group, levels = stringr::str_sort(unique(indata$age_group), numeric = TRUE))
      indata$sex <- factor(indata$sex)

      indata$subset <- TRUE # data subset to consider

      return(indata)
    })

    # if (input$dates_bin) {
    #   req(data)
    #   data_sub <- dplyr::mutate(data(),
    #                             subset = dplyr::between(as.Date(date_report),
    #                                                     input$min_date,
    #                                                     input$max_date))
    #   data(data_sub)
    # }

    output$contents <- DT::renderDataTable({
      req(data)
      data()
    })
  })

  return(reactive({dplyr::filter(data(), subset == TRUE)}))

}

