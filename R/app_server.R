#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  mod_tabpanel_help_server("help")
  mod_tabpanel_data_server("data")
  mod_tabpanel_input_server("input")

}
