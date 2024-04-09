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

  # custom JS/CSS
  shiny::tags$head(
    shiny::tags$style(src = "www/addMoreBtn.css"),
    shiny::tags$script(src = "www/addMoreBtn.js")
  )

  shiny::tabPanel(
    "Data",
    # From runExample("09_upload")
    shiny::h2("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Input: Select a file ----
        shiny::fileInput(ns("file1"), "Choose CSV or Excel File",
                         multiple = TRUE,
                         accept = c(
                           ".xlsx",
                           ".xls",
                           ".csv"
                         )
        ),
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        h3("Data check results"),
        h4("Correct type and values of the columns in your data"),
        span("In this section you receive feedback about the correct type and values of all columns in your data where the column name was written correctly. Columns which are not written in the way it is required by the SOP are not checked for correctness. If there is an error in this section you need to first fix it to be able to use the tool."),
        hr(),
        div(
          style = "border: 2px solid black; padding: 10px;",
          htmlOutput(ns("errors"))
        ), # Display errors output
        hr(), # Horizontal line for visual separation
        h4("Columns in your dataset which were not checked for correctness and will not be used by the Signal Detection Tool"),
        span("In this section you receive feedback about columns which are not recognised/are currently not used by this tool. These can be columns with a wrong column name which does not match the predefined names in the SOP but also additional columns you provided which are not part of the SOP. You can still continue using the app even when there are notifications in this section."),
        hr(),
        span("Please check the column names shown and correct those which should match column names defined in the SOP. The additional columns you provided can stay."),
        span("After you corrected the columns please upload your data again for verification."),
        hr(),
        div(
          style = "border: 2px solid black; padding: 10px;",
          htmlOutput(ns("unused_vars"))
        ),
        hr(),
        h4("Cases which have missing data"),
        span(paste0("In this section you receive feedback about cases for which data required for the computations is missing and will thus be removed for all analysis. The following variables are checked for missingness: ",check_for_missing_values(), ". If any of these variables have a missing value the case is removed for all analysis.")),
        hr(),
        span("Please check if values are missing on purpose, otherwise please correct the data uploaded. Case IDs listed below are excluded from analysis."),
        hr(),
        div(
          style = "border: 2px solid black; padding: 10px;",
          shiny::uiOutput(ns("missing_vals"))
        ),
        hr(),
        h3("Uploaded dataset"),
        hr(),
        # Output: Data file ----
        DT::dataTableOutput(ns("contents"))
      )
    ),
    icon = icon("file")
  )
}

#' tabpanel "data" Server Functions
#' Reading csv or Excel data
#' Checking data for errors
#' Preprocessing the data when no errors on mandatory variables present
#' displaying the data
#' @returns list of two reactive objects, data_processed reactive preprocessed dataset when no errors_detected() is FALSE otherwise raw dataset, errors_detected reactive boolean specifying whether errors in the mandatory variables where detected
#'
#' @noRd
mod_tabpanel_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set maximum file size to 50MB
    options(shiny.maxRequestSize = 50 * 1024^2)

    # Load data
    data <- shiny::reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      req(input$file1)

      # check on the filetype
      ext <- tools::file_ext(input$file1$name)
      validate(need(ext == "csv" | ext == "xlsx" | ext == "xls", "Please upload a csv, xlsx or xls file"))
      # read data
      read_csv_or_excel(input$file1$name, input$file1$datapath)
    })

    errors <- shiny::reactive({
      # run data checks on recognised mandatory and optional variables
      check_raw_surveillance_data(data())
    })

    errors_detected <- shiny::reactive({
      length(errors()) != 0
    })

    # when there were no errors apply preprocessing to data
    data_preprocessed <- shiny::reactive({
      if (!errors_detected()) {
        preprocess_data(data())
      } else {
        data()
      }
    })

    output$errors <- renderText({
      if (!errors_detected()) {
        "All columns with correct column name have the right type and values."
      } else {
        format_html_list(errors())
      }
    })
    output$unused_vars <- renderText({
      # get additional variables
      unused_vars <- get_unused_variables(data())
      if (length(unused_vars) == 0) {
        "All columns in your dataset are recognised by the tool."
      } else {
        format_html_list(unused_vars)
      }
    })
    output$missing_vals <- shiny::renderUI({
      missing_data <- get_missing_data(data())
      if (length(missing_data) == 0) {
        "There are no cases showing missing entries in the data."
      } else {
        shiny::tagList(
          shiny::tags$ul(missing_data), # list of spans of class 'more'
          shiny::tags$script("addMoreBtn();") # custom JS for class 'more'
        )
      }
    })

    # data preview table ----
    output$contents <- DT::renderDataTable({
      req(data)
      DT::datatable(
        data(),
        options = list(
          scrollX = TRUE
        )
      )
    })

    # Return a reactive preprocessed data from this server that can be passed
    # along to subsequent tab modules
    return(list(data = data_preprocessed, errors_detected = errors_detected))
  })
}
