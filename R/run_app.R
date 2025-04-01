#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(path_to_yaml = NULL,
                    config_set = "default",
                    onStart = function() {
                      cat("Warnings are turned off\n")
                      options(warn = -1)

                      onStop(function() {
                        options(warn = 0)
                        cat("Warnings turned on\n")
                      })
                    },
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    ...) {
  # read yaml file
  if (!is.null(path_to_yaml)) {
    app_cache_env$DATA_CONFIG <- config::get(file = path_to_yaml, config = config_set)
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
