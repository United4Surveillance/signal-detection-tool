format_html_list <- function(char, ordered = FALSE){

  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")

  bullets <- paste0(seps[1], char, seps[2], collapse = "")

  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])

  return(html_list)
}
