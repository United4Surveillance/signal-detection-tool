#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Leave this function for adding external resources
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    golem_add_external_resources(),
    # Application title
    shiny::div(
      id = "title-panel",
      class = "title-panel",
      shiny::titlePanel(
        shiny::tagList(
          shiny::br(),
          # Logo and headertext
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::div(
                style = "text-align: left; margin-left: 20px; font-size: 50px;",
                "Signal Detection Tool"
              )
            ),
            shiny::column(width = 2, shiny::tags$img(src = "www/img/U4S-BLUE-200x87.png", style = "padding-bottom: 20px;")),
            shiny::column(width = 2, shiny::tags$img(src = "www/img/EN_V_Co-funded_by_POS.png", height = 95, width = "auto"))
          )
        ),
        windowTitle = "U4S Signal Detection"
      )
    ),

    # Content
    shiny::div(
      class = "content-container",
      shiny::tabsetPanel(
        mod_tabpanel_help_ui("help"),
        mod_tabpanel_data_ui("data"),
        mod_tabpanel_input_ui("input"),
        mod_tabpanel_signals_ui("signals"),
        mod_tabpanel_report_ui("report"),
        selected = "Data"
      )
    ),

    # EU Disclaimer
    shiny::div(
      id = "footer",
      class = "footer",
      p("Co-funded by the European Union. Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union. Neither the European Union nor the granting authority can be held responsible for them.")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  golem::bundle_resources(
    path = app_sys("app/www"),
    app_title = "SignalDetectionTool"
  )
  shiny::tags$head(
    golem::favicon(
      ico = "https://united4surveillance.eu/wp-content/uploads/2023/03/FAV-150x107.png",
      ext = "png"
    )
  )
  # TODO: move to css file
  # shiny::tags$head(
  shiny::tags$style(shiny::HTML("
      * {
        box-sizing: border-box;
      }

      .title-panel {
        text-align: center;
      }

      .content-container {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        overflow: hidden;  /* Ensure no scrollbars on outer container */
        background-color: #f8f9fa;
      }

      .tab-content {
        padding: 10px;
        flex: 1;
        overflow-y: auto;
        height: 100%;
      }

      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        text-align: center;
        font-style: italic;
        font-size: 12px;
        padding: 10px;
        background-color: #ffffff; /* this is important for height calculation */
        z-index: 100;  /* Ensure footer stays at the bottom */
      }

      .bslib-value-box .value-box-title {
        font-size: 15px !important; /* Adjust the font size */
        font-weight: bold !important; /* Make the title text bold */
        padding-top: -20px !important; /* Reduce space above the title */
        margin-top: -20px !important; /* Reduce space above the title */
      }

      .bslib-value-box .value-box-value {
        font-size: 13px !important; /* Adjust the font size */
        margin-bottom: -20px !important; /* Reduce space below the value */
        padding-bottom: -20px !important; /* Reduce space below the value */
      }
    "))
  # )
}
