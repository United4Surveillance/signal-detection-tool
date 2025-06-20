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
#' @param interactive Logical, indicating whether interactive elements should be included in the report. Only applicable when `report_format = "HTML"`. Defaults to `TRUE`.
#' @param tables Logical. True if tables should be included in report.
#' @param output_file A character string specifying the name of the output file (without directory path). If `NULL` (default), the file name is automatically generated to be SignalDetectionReport. See \link[rmarkdown]{render} for more details.
#' @param output_dir A character string specifying the output directory for the rendered output file (default is ".", which means the rendered file will be saved in the current working directory. See \link[rmarkdown]{render} for more details. `NULL` is used when running the report from shiny app which will take the Downloads folder as default option for saving.
#' @param signals_padded List of calculated and padded signals (for use within the app, default is NULL)
#' @param signals_agg List of aggregated signals  (for use within the app, default is NULL)
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention. This can be used for interrupted timeseries analysis. It only works with the following methods: "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend", "Step harmonic", "Step harmonic with timetrend". Default is NULL which indicates that no intervention is done.
#' @param custom_logo A character string with a path to a png or svg logo, to replace the default United 4 Surveillance logo.
#' @param custom_css A character string with a path to a css, to replace the default United 4 Surveillance css.
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
#'   interactive = TRUE,
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
    interactive = TRUE,
    tables = TRUE,
    output_file = NULL,
    output_dir = ".",
    signals_padded = NULL,
    signals_agg = NULL,
    intervention_date = NULL,
    custom_logo = NULL,
    custom_css = NULL) {
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
  checkmate::assert_logical(interactive)
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
    checkmate::check_subset(pathogens, choices = names(signals_padded_list)),
    combine = "or"
  )

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = pathogens,
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
    report_format == "HTML" ~ "flex_dashboard",
    report_format == "DOCX" ~ "word_document",
    TRUE ~ NA_character_
  )

  rmd_path <- system.file("report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
  rmd_dir <- dirname(normalizePath(rmd_path))
  if(is.null(custom_css)){
    css_name = "style.css"
  } else{
    css_abs  <- normalizePath(custom_css,  mustWork = TRUE)
    css_dst  <- file.copy(css_abs,  rmd_dir, overwrite = TRUE)
    css_name <- basename(css_abs)
  }

  if(is.null(custom_logo)){
    logo_abs <- normalizePath(system.file("report/logo.png", package = "SignalDetectionTool"))
    logo_name <- basename(logo_abs)
  } else{
    logo_abs <- normalizePath(custom_logo, mustWork = TRUE)
    file.copy(logo_abs, rmd_dir, overwrite = TRUE)
    logo_name <- basename(logo_abs)
  }

  # encode the logo directly in the HTML for standalone file
  mime_type <- if (grepl("\\.svg$", logo_abs, ignore.case = TRUE)){
    "image/svg+xml"} else {"image/png"}
  logo_data <- base64enc::dataURI(file = logo_abs, mime = mime_type)

  js_code <- sprintf(
    '<script>
    document.addEventListener("DOMContentLoaded", function () {
      var nav = document.querySelector(".navbar.navbar-inverse .container-fluid") ||
                document.querySelector(".navbar.navbar-fixed-top");
      if (!nav) return;

      var img = document.createElement("img");
      img.src   = "%s";            // one data-URI, no file needed
      img.alt   = "logo";
      img.title = "logo";
      img.style.cssText = "height:46px;position:absolute;right:16px;top:50%%;transform:translateY(-50%%);";
      nav.appendChild(img);
    });
    </script>',
    logo_data
    )
  logo_html <- file.path(rmd_dir, "injected_logo.html")
  writeLines(js_code, logo_html)

  output_format <- flexdashboard::flex_dashboard(
    orientation = "rows",
    css         = css_name,
    includes    = rmarkdown::includes(after_body = basename(logo_html)),
    theme       = bslib::bs_theme(version = 5)
  )

  rmarkdown::render(rmd_path,
    output_format = output_format,
    params = report_params,
    output_file = output_file,
    output_dir = output_dir
  )
}

