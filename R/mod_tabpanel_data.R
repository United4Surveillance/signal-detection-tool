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
    title = "Data",
    icon = icon("file"),
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#304794",
      position = "full-page",
      height = "100px",
      width = "100px"
    ),
    shiny::div(
      class = "content-container",
      shiny::div(
        class = "card-container",
        bslib::layout_columns(
          col_widths = c(3, 9, 12),
          bslib::card(
            bslib::card_title("Load Data", container = shiny::h1),
            shiny::fileInput(
              inputId = ns("file1"),
              label = "Choose CSV or Excel File",
              multiple = TRUE,
              accept = c(
                ".xlsx",
                ".xls",
                ".csv"
              )
            ),
          ),
          bslib::card(
            bslib::card_title("Data check results", container = shiny::h1),
            shiny::h2("Correct type and values of the columns in your data"),
            shiny::span("In this section you receive feedback about the correct type and values of all columns in your data where the column name was written correctly. Columns which are not written in the way it is required by the SOP are not checked for correctness. If there is an error in this section you need to first fix it to be able to use the tool."),
            shiny::div(
              class = "check-result",
              shiny::htmlOutput(ns("errors"))
            ),
            # Display errors output
            shiny::h2("Columns in your dataset which were not checked for correctness"),
            shiny::span("In this section you receive feedback about columns which are not checked by the tool. These can be columns with a wrong column name that does not match the predefined names in the SOP, as well as additional columns you provided which are not part of the SOP. You can still continue using the app even when there are notifications in this section."),
            shiny::span("Please check the column names shown and correct those which should match column names defined in the SOP. The additional columns you provided can stay and can be used in stratification and filters."),
            shiny::span("After you corrected the columns please upload your data again for verification."),
            shiny::div(
              class = "check-result",
              shiny::htmlOutput(ns("unused_vars"))
            ),
            shiny::h2("Data Quality"),
            shiny::span("In this section you receive feedback about implausible or inconsistent values and missing data."),
            shiny::span(paste0("The following variables are checked for missingness: ", check_for_missing_values(), ".")),
            shiny::div(
              class = "check-result",
              shiny::uiOutput(ns("missing_vals")),
            ),
            shiny::span(paste0("The following variables are checked for inconsistent/implausible values: age")),
            shiny::div(
              class = "check-result",
              shiny::uiOutput(ns("negative_ages")),
            )
          ),
          shiny::uiOutput(ns("data_dt"))
        )
      ),
      footer_text
    )
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
      shiny::req(input$file1)

      # check on the filetype
      ext <- tools::file_ext(input$file1$name)
      shiny::validate(need(ext == "csv" | ext == "xlsx" | ext == "xls", "Please upload a csv, xlsx or xls file"))
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
        shiny::div(
          "Please check if values are missing on purpose, otherwise please correct the data uploaded. Case IDs listed below are excluded from analysis.",
          shiny::br(),
          shiny::tagList(
            shiny::tags$ul(missing_data), # list of spans of class 'more'
            shiny::tags$script("addMoreBtn();") # custom JS for class 'more'
          )
        )
      }
    })
    # Notify user about negative ages in the data
    output$negative_ages <- shiny::renderText({
      if ("age" %in% names(data())) {
        data_negative_ages <- data() %>%
          dplyr::filter(age < 0)
        if (nrow(data_negative_ages) != 0) {
          "There are some ages with negative values in the data. These values will be replaced with NA in the analysis."
        } else {
          "There are no cases with negative ages."
        }
      }
    })

    # data preview table ----
    output$data_dt <- shiny::renderUI({
      shiny::req(data())
      shiny::tagList(
        bslib::card(
          min_height = "775px",
          shiny::h1("Uploaded Data"),
          DT::renderDataTable({
            DT::datatable(
              data(),
              options = list(
                scrollX = TRUE
              )
            )
          })
        )
      )
    })

    # Return a reactive preprocessed data from this server that can be passed
    # along to subsequent tab modules
    list(data = data_preprocessed, errors_detected = errors_detected)
  })
}
