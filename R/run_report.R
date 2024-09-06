#' Renders signal detection report
#'
#' If executed as a standalone function, all filtering must
#' be performed beforehand.
#' This function is also invoked within the app.
#'
#' @param report_format format of the report: HTML or DOCX
#' @param data data.frame containing surveillance data in linelist format
#' @param method algorithm to be used
#' @param number_of_weeks number of weeks for which signals are generated
#' @param strata A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param interactive Logical (only applicable to HTML report)
#' @param tables Logical. True if tables should be included in report.
#' @param output_file The name of the output file \link[rmarkdown]{render}
#' @param signals_padded calculated and padded signals (for use within the app, default is NULL)
#' @param signals_agg aggregated signals  (for use within the app, default is NULL)
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention in the pandemic correction models. Default is NULL which indicates that no intervention is done.
#'
#' @return the compiled document is written into the output file, and the path of the output file is returned; see \link[rmarkdown]{render}
#' @export
#'
#' @examples
#' \dontrun{
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example,
#'   method = "farrington",
#'   strata = c("county", "community", "sex", "age_group"),
#'   interactive = TRUE,
#'   tables = TRUE,
#'   number_of_weeks = 6
#' )
#' }
run_report <- function(
    report_format = "HTML",
    data = SignalDetectionTool::input_example,
    method = "farrington",
    number_of_weeks = 6,
    strata = c("county", "community", "sex", "age_group"),
    interactive = TRUE,
    tables = TRUE,
    output_file = NULL,
    signals_padded = NULL,
    signals_agg = NULL,
    intervention_date = NULL) {
  # Check inputs ---------------------------------------------------------------
  checkmate::assert_choice(report_format,
    choices = c("HTML", "DOCX"),
    null.ok = FALSE
  )

  checkmate::assert_data_frame(data)
  checkmate::assert_logical(interactive)
  checkmate::assert_logical(tables)

  # render report --------------------------------------------------------------
  if ("None" %in% strata) {
    strata <- NULL
  }

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = unique(data$pathogen),
    number_of_weeks = number_of_weeks,
    method = method,
    strata = strata,
    interactive = ifelse(report_format != "HTML", FALSE, interactive),
    tables = tables,
    signals_padded = signals_padded,
    signals_agg = signals_agg,
    intervention_date = intervention_date
  )

  report_f <- dplyr::case_when(
    report_format == "HTML" ~ "html_document",
    report_format == "DOCX" ~ "word_document",
    report_format == "PDF" ~ "pdf_document",
    TRUE ~ NA_character_
  )
  rmd_path <- system.file("report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
  rmarkdown::render(rmd_path,
    output_format = report_f,
    params = report_params,
    output_file = output_file
  )
}
