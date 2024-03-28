#' Renders signal detection report
#'
#' @param report_format format of the report: HTML or DOCX
#' @param data data.frame containing surveillance data in linelist format
#' @param algo algorithm to be used
#' @param strata A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param interactive Logical (only applicable to HTML report)
#' @param tables Logical. True if tables should be included in report.
#' @param number_of_weeks number of weeks for which signals are generated
#' @param output_file The name of the output file \link[rmarkdown]{render}
#' @param signal_results calculated signals (for use within the app, default is NULL)
#' @param signals_agg aggregated signals  (for use within the app, default is NULL)
#'
#' @return the compiled document is written into the output file, and the path of the output file is returned; see \link[rmarkdown]{render}
#' @export
#'
#' @examples
#' \dontrun{
#' run_report(report_format = "HTML",
#'            data = SignalDetectionTool::input_example,
#'            algo = "farrington",
#'            strata = c("county_id", "community_id", "sex", "age_group"),
#'            interactive = TRUE,
#'            tables = TRUE,
#'            number_of_weeks = 6)
#' }


run_report <- function(
    report_format = "HTML",
    data = SignalDetectionTool::input_example,
    algo = "farrington",
    strata = c("county_id", "community_id", "sex", "age_group"),
    interactive = TRUE,
    tables = TRUE,
    number_of_weeks = 6,
    output_file  = NULL,
    signal_results = NULL,
    signals_agg = NULL) {
  # browser()

  # Check inputs ---------------------------------------------------------------
  checkmate::assert_choice(report_format,
                           choices = c("HTML", "DOCX"),
                           null.ok = FALSE)

  checkmate::assert_choice(algo,
                           choices = c("farrington", "aeddo", "cusum", "ears"),
                           null.ok = FALSE)

  checkmate::assert_data_frame(data)
  checkmate::assert_logical(interactive)
  checkmate::assert_logical(tables)
  checkmate::assert_integerish(number_of_weeks)

  # render report --------------------------------------------------------------
  if ("None" %in% strata) {
    strata <- NULL
  }

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = unique(data$pathogen),
    algo = algo,
    strata = strata,
    interactive = ifelse(report_format != "HTML", FALSE, interactive),
    tables = tables,
    number_of_weeks = number_of_weeks,
    signal_results = signal_results,
    signals_agg = signals_agg
    )


  report_f <- dplyr::case_when(
    report_format == "HTML" ~ "html_document",
    report_format == "DOCX" ~ "word_document",
    report_format == "PDF" ~ "pdf_document",
    TRUE ~ NA_character_)

  rmarkdown::render("inst/report/SignalDetectionReport.Rmd",
                    output_format = report_f,
                    params = report_params,
                    output_file = output_file)


}
