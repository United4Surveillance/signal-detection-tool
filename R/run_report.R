#' Renders signal detction report
#'
#' @param report_format format of the report: HTML, DOCX or PDF
#' @param data data.frame containing surveillance data in linelist format
#' @param algo algorithm to be used
#' @param strata A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param interactive Logical (only applicable to HTML report)
#' @param tables Logical. True if tables should be included in report.
#' @param training_range vector of date range for training period
#' @param analysis_range vector of date range for analysis period
#' @param output_file The name of the output file \link[rmarkdown]{render}
#'
#' @return renders report see \link[rmarkdown]{render}
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
#'            training_range = c(as.Date("1900-01-01"), as.Date("1901-01-01")),
#'            analysis_range = c(as.Date("1901-01-02"), as.Date("1902-01-01")))
#' }


run_report <- function(
    report_format = "HTML",
    data = SignalDetectionTool::input_example,
    algo = "farrington",
    strata = c("county_id", "community_id", "sex", "age_group"),
    interactive = TRUE,
    tables = TRUE,
    training_range = c(as.Date("1900-01-01"), as.Date("1901-01-01")),
    analysis_range = c(as.Date("1901-01-02"), as.Date("1902-01-01")),
    output_file  = NULL) {

  # Check inputs ---------------------------------------------------------------
  checkmate::assert_choice(report_format,
                           choices = c("HTML", "DOCX", "PDF"),
                           null.ok = FALSE)

  checkmate::assert_choice(algo,
                           choices = c("farrington", "aeddo"),
                           null.ok = FALSE)

  checkmate::assert_data_frame(data)
  checkmate::assert_logical(interactive)
  checkmate::assert_logical(tables)
  checkmate::assert_date(c(training_range, analysis_range))

  # render report --------------------------------------------------------------
  if ("None" %in% strata) {
    strata <- NULL
  }

  report_params <- list(
    data = data,
    algo = algo,
    strata = strata,
    interactive = ifelse(report_format != "HTML", FALSE, interactive),
    tables = tables,
    training_range = training_range,
    analysis_range = analysis_range)


  report_f <- dplyr::case_when(
    report_format == "HTML" ~ "SignalDetectionReport.Rmd",
    report_format == "DOCX" ~ "SignalDetectionReportWord.Rmd",
    report_format == "PDF" ~ "SignalDetectionReportPDF.Rmd",
    TRUE ~ NA_character_)

  rmarkdown::render(paste0("inst/report/", report_f),
                    params = report_params,
                    output_file = output_file)


}
