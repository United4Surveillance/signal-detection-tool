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
        span("In this section you receive feedback about the correct type and values of all columns in your data where the column name was written correctly. Columns which are not written in the way it is required by the SOP are not checked for correctness."),
        hr(),
        div(
          style = "border: 2px solid black; padding: 10px;",
          htmlOutput(ns("errors"))
        ), # Display errors output
        hr(), # Horizontal line for visual separation
        h4("Columns in your dataset which were not checked for correctness and will not be used by the Signal Detection Tool"),
        span("In this section you receive feedback about columns which are not recognised by this tool. These can be columns which have a wrong column name as they do not match the predefined names in the SOP. Furthermore additional columns you provided in your dataset which we did not predefine are shown here."),
        hr(),
        span("Please check the column names shown and correct them where necessary."),
        span("After you corrected the columns please upload your data again for verification."),
        hr(),
        div(
          style = "border: 2px solid black; padding: 10px;",
          htmlOutput(ns("unused_vars"))
        ),
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
        "There are zero variables which are not recognised/known by the tool."
      } else {
        format_html_list(unused_vars)
      }
    })

    # data preview table ----
    output$contents <- DT::renderDataTable({
      req(data)
      data()
    })

    # Return a reactive preprocessed data from this server that can be passed
    # along to subsequent tab modules
    return(list(data = data_preprocessed, errors_detected = errors_detected))
  })
}
