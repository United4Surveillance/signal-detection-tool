#' Renders signal detection report
#'
#' If executed as a standalone function, all filtering must
#' be performed beforehand.
#' This function is also invoked within the app.
#'
#' @param data data.frame containing surveillance data in linelist format following the format specified in `input_metadata`
#' @param report_format character, format of the report: "HTML" or "DOCX"
#' @param method a character string, signal detection method to be used. One of "FarringtonFlexible", "EARS" , "CUSUM", "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend", "Step harmonic", "Step harmonic with timetrend".
#'   You can retrieve the full list using [names(available_algorithms())].
#'
#' @seealso [names(available_algorithms())]
#' @param number_of_weeks integer, number of weeks for which signals are generated
#' @param pathogens A character vector specifying which pathogens should be included in report.
#' @param strata A character vector specifying the columns to stratify. If `NULL` no strata are used.
#'   the analysis. Default is NULL.
#' @param tables Logical. True if tables should be included in report.
#' @param output_file A character string specifying the name of the output file (without directory path). If `NULL` (default), the file name is automatically generated to be SignalDetectionReport. See \link[rmarkdown]{render} for more details.
#' @param output_dir A character string specifying the output directory for the rendered output file (default is ".", which means the rendered file will be saved in the current working directory. See \link[rmarkdown]{render} for more details. `NULL` is used when running the report from shiny app which will take the Downloads folder as default option for saving.
#' @param signals_padded Tibble of calculated and padded signals (for use within the app, default is NULL)
#' @param signals_agg Tibble of aggregated signals  (for use within the app, default is NULL)
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention. This can be used for interrupted timeseries analysis. It only works with the following methods: "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend", "Step harmonic", "Step harmonic with timetrend". Default is NULL which indicates that no intervention is done.
#'
#' @return the compiled document is written into the output file, and the path of the output file is returned; see \link[rmarkdown]{render}
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Run report with specified parameters and HTML format output
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example,
#'   method = "FarringtonFlexible",
#'   strata = c("county", "sex"),
#'   tables = TRUE,
#'   number_of_weeks = 6
#' )
#' # Example 2: An example output directory specified
#' run_report(
#'   method = "EARS",
#'   output_dir = "C:/Users/SmithJ/Documents"
#' )
#' # Example 3: An example output file name is speficied
#' run_report(
#'   method = "EARS",
#'   output_file = "My Signal Report"
#' )
#'
#' # Example 4: No strata are used
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example,
#'   method = "EARS",
#'   strata = NULL
#' )
#' }
run_report <- function(
    data = SignalDetectionTool::input_example,
    report_format = "HTML",
    method = "FarringtonFlexible",
    number_of_weeks = 6,
    pathogens = c("Pertussis"),
    strata = c("county", "age_group"),
    tables = TRUE,
    output_file = NULL,
    output_dir = ".",
    signals_padded = NULL,
    signals_agg = NULL,
    intervention_date = NULL) {
  # Check that package ggforce is installed as it is required for running the report
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    stop("The 'ggforce' package is required to generate the report. Please install it using install.packages('ggforce')")
  }
  # This needs to be checked as flexdashboard is only in Suggests
  # ToDo: return this message to the user in the shiny app in the report tab
  if (report_format == "HTML" & !rlang::is_installed("flexdashboard")) {
    stop("The 'flexdashboard' package is required to generate the HTML report. Please install it using install.packages('flexdashboard')")
  }


  # Check inputs ---------------------------------------------------------------
  checkmate::assert_choice(report_format,
    choices = c("HTML", "DOCX"),
    null.ok = FALSE
  )
  checkmate::assert_data_frame(data)
  # Validate strata
  checkmate::assert_character(strata, null.ok = TRUE, min.len = 1)
  # check that all columns are present in the data
  if ("None" %in% strata) {
    strata <- NULL
  }
  for (col in strata) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }
  checkmate::assert(
    checkmate::check_choice(method, choices = names(available_algorithms()))
  )
  checkmate::assert_logical(tables)
  # Validate `output_dir`
  checkmate::assert_string(output_dir, null.ok = TRUE)
  # Validate `output_file`
  checkmate::assert_character(output_file, null.ok = TRUE, len = 1)
  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )


  if (is.character(intervention_date)) {
    intervention_date <- as.Date(intervention_date)
  }
  # transform the method name used in the app to the method names in the background
  method <- available_algorithms()[method]

  # assert pathogens exist in dataframe or padded signals
  checkmate::assert(
    checkmate::check_subset(pathogens, choices = unique(data$pathogen)),
    checkmate::check_subset(pathogens, choices = unique(signals_padded$pathogen)),
    combine = "or"
  )

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = pathogens,
    number_of_weeks = number_of_weeks,
    method = method,
    strata = strata,
    tables = tables,
    signals_padded = signals_padded,
    signals_agg = signals_agg,
    intervention_date = intervention_date
  )

  if (report_format == "HTML") {
    rmd_path <- system.file("report/html_report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
  } else {
    rmd_path <- system.file("report/word_report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
  }

  rmarkdown::render(rmd_path,
    params = report_params,
    output_file = output_file,
    output_dir = output_dir
  )
}
