#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # styling
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "div.tab-content {
            height: 80vh;
            overflow-y: auto;
            overflow-x: hidden;
            padding: 10px;
          }"
        )
      )
    ),

    # Your application UI logic
    bslib::page_fluid(
      # theme = bslib::bs_theme(
      #   bg = "#101010",
      #   fg = "#FFF",
      #   primary = "#E69F00",
      #   secondary = "#0072B2",
      #   success = "#009E73"
      # ),
      shinyjs::useShinyjs(),
      # Application title
      ## Logo and headertext
      shiny::titlePanel(
        shiny::div(
          style = "height: 10vh;",
          shiny::br(),
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
          ),
        ),
        windowTitle = "U4S Signal Detection"
      ),
      # shiny::fluidRow(
      # shiny::column(
      # width = 12,
      shiny::tabsetPanel(
        mod_tabpanel_help_ui("help"),
        mod_tabpanel_data_ui("data"),
        mod_tabpanel_input_ui("input"),
        mod_tabpanel_signals_ui("signals"),
        mod_tabpanel_report_ui("report"),
        selected = "Data"
        # )
        # )
      ),

      # EU Disclaimer
      shiny::tags$footer(
        shiny::p("Co-funded by the European Union. Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union. Neither the European Union nor the granting authority can be held responsible for them."),
        style = "text-align: center; font-style: italic; font-size: 12px; padding-top: 20px; padding-bottom: 20px;"
      )
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

  tags$head(
    golem::favicon(
      ico = "https://united4surveillance.eu/wp-content/uploads/2023/03/FAV-150x107.png",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SignalDetectionTool"
    ),
    tags$style(HTML("
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
