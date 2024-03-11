format_html_list <- function(char, ordered = FALSE){

  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")

  bullets <- paste0(seps[1], char, seps[2], collapse = "")

  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])

  return(html_list)
}


datacheck_error_message <- shiny::tagList(
  shiny::br(),
  shiny::h2("Data Format Check Failed"),
  shiny::p("Unfortunately, the selected data does not meet the required format."),
  shiny::p("Please make sure the data follows the correct structure and try again."),
  shiny::br(),
  shiny::hr(),
  shiny::p("You can check the data in the 'Data' tab for more details on the issue.")
)


algorithm_error_message <- shiny::tagList(
  shiny::br(),
  shiny::h3("There is no outbreak detection algorithm which can be applied to your current settings, please change your selected settings in the input tab and try again."),
  shiny::br()
)
