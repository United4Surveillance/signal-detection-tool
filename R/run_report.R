#' Renders signal detection report
#'
#' The function supports the generation of a single-pathogen and multi-pathogen report when `report_format = "HTML"`.
#' For Word (DOCX) output, only single-pathogen reports are currently supported.
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
#' @param pathogens A character vector specifying which pathogens to include in the report.
#'   If `NULL` (default), all pathogens present in `data`, `signals_padded`, or `signals_agg` are used.
#'   Multi-pathogen reports are supported only for HTML output.
#' @param strata A character vector specifying the columns to stratify. If `NULL` no strata are used. If precomputed signals are provided  this argument is ignored and strata are inferred from the provided signals. Defaults to c("county", "age_group") when no precomputed signals were provided.
#' @param tables Logical, default TRUE. True if Signal Detection Tables should be included in report. Only used for DOCX reports, the parameter is ignored for HTML reports.
#' @param output_file A character string specifying the name of the output file (without directory path). If `NULL` (default), the file name is automatically generated to be SignalDetectionReport. See \link[rmarkdown]{render} for more details.
#' @param output_dir A character string specifying the output directory for the rendered output file (default is ".", which means the rendered file will be saved in the current working directory. See \link[rmarkdown]{render} for more details. `NULL` is used when running the report from shiny app which will take the Downloads folder as default option for saving.
#' @param signals_padded A tibble of precomputed and padded signals containing a `pathogen` column.
#'   If multiple pathogens are present, the tibble should represent all of them stacked together
#'   (e.g., using `dplyr::bind_rows()`).
#'   Defaults to `NULL`, in which case the signal data will be computed from the linelist
#'   within the `run_report()` function.
#'   If not `NULL`, the provided `signals_padded` is used as-is, and signals are not recomputed.
#' @param signals_agg A tibble of aggregated signals containing a `pathogen` column.
#'   If multiple pathogens are included, the tibble should represent all of them stacked together
#'   (e.g., using `dplyr::bind_rows()`).
#'   Defaults to `NULL`, in which case the aggregated signals are computed from the linelist
#'   within the `run_report()` function.
#'   If not `NULL`, the provided `signals_agg` is used directly and signals are not recomputed.
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention. This can be used for interrupted timeseries analysis. It only works with the following methods: "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend", "Step harmonic", "Step harmonic with timetrend". Default is NULL which indicates that no intervention is done.
#' @param custom_logo A character string with a path to a png or svg logo, to replace the default United4Surveillance logo. Only used when `report_format` is `"HTML"`.
#' @param custom_theme A bslib::bs_theme() to replace the default United4Surveillance theme. This is mainly used to change colors. See the bslib documentation for all parameters. Use version = "3" to keep the navbar intact. Only used when `report_format` is `"HTML"`.
#' @param min_cases_signals integer, minimum number of cases a signal must have. All signals with case counts smaller than this will be filtered in a post-processing step. This parameter is only applied when precomputed signals_agg and signals_padded are not given. If you want to post-process your precomputed signals you need to do that before generating the report.
#' @param title NULL or a character string. Specifies the title of the report that is to be created
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
#'   number_of_weeks = 6
#' )
#' # Example 2: An example output directory specified
#' run_report(
#'   method = "EARS",
#'   output_dir = "C:/Users/SmithJ/Documents"
#' )
#' # Example 3: An example output file name is specified
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
#'
#' # Example 5: HTML report for multiple pathogens
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example_multipathogen,
#'   method = "Harmonic"
#' )
#'
#' Example 6: HTML report for a subset of pathogens in a multi-pathogen dataset
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example_multipathogen,
#'   pathogens = c("Enterobacter","Salmonella"),
#'   method = "Harmonic"
#' )
#'
#' }
run_report <- function(
    data,
    report_format = "HTML",
    method = "FarringtonFlexible",
    number_of_weeks = 6,
    pathogens = NULL,
    strata = c("county", "age_group"),
    tables = TRUE,
    output_file = NULL,
    output_dir = ".",
    signals_padded = NULL,
    signals_agg = NULL,
    intervention_date = NULL,
    custom_logo = NULL,
    custom_theme = NULL,
    min_cases_signals = 1,
    title = NULL) {

  # Currently multi pathogen report is only supported for HTML
  if (report_format == "DOCX" & length(unique(data$pathogen)) > 1) {
    stop("Currently the Multi-Pathogen Report functionality is only supported for HTML Reports. In case you want to get a Word report, please generate reports seperately for each pathogen by using a dataset containing only one pathogen.")
  }

  # Check inputs ---------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(report_format,
    choices = c("HTML", "DOCX"),
    null.ok = FALSE
  )
  checkmate::assert(
    checkmate::check_choice(method, choices = names(available_algorithms()))
  )
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks, lower = 1)
  )
  # assert pathogens is NULL (default includes all pathogens) or exist in dataframe or padded signals
  checkmate::assert(
    checkmate::check_null(pathogens),
    checkmate::check_subset(pathogens, choices = unique(data$pathogen)),
    checkmate::check_subset(pathogens, choices = unique(signals_padded$pathogen)),
    combine = "or"
  )
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
  checkmate::assert_logical(tables)
  checkmate::assert_character(output_file, null.ok = TRUE, len = 1)
  checkmate::assert_string(output_dir, null.ok = TRUE)

  # give default name if none is supplied
  if(is.null(output_file)){
    output_file <- paste0(
      "SignalDetectionReport.",
      switch(report_format, HTML = "html", DOCX = "docx")
    )
  }

  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(signals_agg),
    checkmate::check_data_frame(signals_agg, col.names = "named"),
    combine = "or"
  )
  # additional checks specific to the data frame
  # pathogen needs to be a column of signals_agg
  if(!is.null(signals_agg)){
    checkmate::assert_true("pathogen" %in% names(signals_agg))
  }
  checkmate::assert(
    checkmate::check_null(signals_padded),
    checkmate::check_data_frame(signals_padded, col.names = "named"),
    combine = "or"
  )
  # additional checks specific to the data frame
  # pathogen needs to be added to signals_padded
  if(!is.null(signals_padded)){
    checkmate::assert_true("pathogen" %in% names(signals_padded))
  }
  checkmate::assert(
    checkmate::check_null(custom_logo),
    checkmate::check_character(custom_logo, len = 1, pattern = "\\.svg$|\\.png$", ignore.case = TRUE),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(custom_theme),
    checkmate::check_class(custom_theme, "bs_theme"),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_integerish(min_cases_signals, lower=1)
  )
  checkmate::assert(
    checkmate::check_string(title, null.ok = TRUE)
  )

  # Preparation for reporting ---------------------------------------------------------------
  # transform the method name used in the app to the method names in the background
  method <- available_algorithms()[method]
  # transform intervention date
  if (is.character(intervention_date)) {
    intervention_date <- as.Date(intervention_date)
  }

  # setting of param pathogens if NULL based on data provided
  if (is.null(pathogens)) {
    # usage of linelist
    if (is.null(signals_agg) | is.null(signals_padded)) {
      pathogens <- unique(data$pathogen)
      # usage of signals_agg, signals_pad
    } else {
      pathogens <- unique(signals_padded$pathogen)
    }
  }

  # when signals_agg provided then use strata inside this dataset
  # only need to check signals_agg as when signals_padded is NULL signals are anyways recomputed
  # prevents errors when user did not specify strata and used signals_agg, signals_padded
  if(!is.null(signals_agg)){
    strata <- get_strata_from_signals_agg(signals_agg)
  }

  # compute signals if not provided to run_report by the user
  if (is.null(signals_agg) | is.null(signals_padded)) {
    preprocessed_data <- data %>% preprocess_data()

    signals_agg_list <- list()
    signals_padded_list <- list()

    for (pat in pathogens) {
      preprocessed_data_pat <- preprocessed_data %>%
        dplyr::filter(pathogen == pat)

      signals <- get_signals_all(preprocessed_data_pat,
        method = method,
        intervention_date = intervention_date,
        stratification = strata,
        date_start = NULL,
        date_end = NULL,
        date_var = "date_report",
        number_of_weeks = number_of_weeks
      ) %>%
        dplyr::mutate(pathogen = pat,
                      alarms = dplyr::if_else(alarms & cases < min_cases_signals,
                                       FALSE, alarms, missing=alarms)
                      )

      signals_agg_pad <- aggregate_pad_signals(
        signals,
        preprocessed_data_pat,
        number_of_weeks,
        method
      )

      signals_agg <- signals_agg_pad$signals_agg
      signals_padded <- signals_agg_pad$signals_padded

      signals_agg_list[[pat]] <- signals_agg %>% dplyr::mutate(pathogen = pat)
      signals_padded_list[[pat]] <- signals_padded %>% dplyr::mutate(pathogen = pat)
    }

    signals_agg <- dplyr::bind_rows(signals_agg_list)
    signals_padded <- dplyr::bind_rows(signals_padded_list)

    # Clean up as these can be large
    rm(signals_agg_list, signals_padded_list)
    gc()
  }

  title <- if(is.null(title) || trimws(title) == "") paste0("Signal Detection Report - ", unique(data$country)) else title

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = pathogens,
    number_of_weeks = number_of_weeks,
    method = method,
    strata = strata,
    signals_padded = signals_padded,
    signals_agg = signals_agg,
    intervention_date = intervention_date,
    title = title
  )

  if (report_format == "DOCX") {
    report_params$tables <- tables
  }

  if (report_format == "HTML") {
    # location to save results
    temp_dir <- tempdir()
    dir.create(file.path(temp_dir, "report_pages"))

    rmd_path <- system.file("report/html_report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
    rmd_dir <- dirname(normalizePath(rmd_path))

    if (is.null(custom_logo)) {
      logo_abs <- normalizePath(system.file("report/html_report/logo.png", package = "SignalDetectionTool"))
      logo_name <- basename(logo_abs)
    } else {
      logo_abs <- normalizePath(custom_logo, mustWork = TRUE)
      logo_name <- basename(logo_abs)
    }

    # encode the logo directly in the HTML for standalone file
    mime_type <- if (grepl("\\.svg$", logo_abs, ignore.case = TRUE)) {
      "image/svg+xml"
    } else {
      "image/png"
    }
    logo_data <- base64enc::dataURI(file = logo_abs, mime = mime_type)

    # Java Script to move the logo to the right of the navbar
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

    # use the default U4S theme if none is specified
    # primary and warning were specified by styleguide, the rest not
    if (is.null(custom_theme)) {
      custom_theme <- bslib::bs_theme(
        version = "5",
        bg = "white",
        fg = "black",
        primary = "#304898",
        success = "#579931",
        info = "#669ed4",
        warning = "#F4D015",
        danger = "#be1622"
      ) %>%
        bslib::bs_add_variables(
          # BS5 spaces the rows too far apart -> manually reduce space
          spacer = "0rem",
          # reducing the spacer makes the navbar too small -> manually increase
          "navbar-padding-y" = "0.5rem",
          "navbar-brand-padding-y" = "0.5rem",
          "navbar-toggler-padding-y" = "0.25rem",
          "dropdown-padding-y" = "0.5rem",
          "dropdown-padding-x" = "0.5rem"
        )
    }

    output_format_s <- flexdashboard::flex_dashboard(
      orientation = "rows",
      vertical_layout = "scroll",
      includes = rmarkdown::includes(after_body = basename(logo_html)),
      theme = custom_theme,
      self_contained = FALSE,
      lib_dir = file.path(normalizePath(temp_dir), "report_pages", "lib")
    )

    output_format <- flexdashboard::flex_dashboard(
      orientation = "rows",
      vertical_layout = "scroll",
      includes = rmarkdown::includes(after_body = basename(logo_html)),
      theme = custom_theme,
    )

    # rmd paths for pathogen and strata pages
    rmd_pathogen_path <- system.file("report/html_report/SignalDetectionReport_body.Rmd",
       package = "SignalDetectionTool")
    rmd_strata_path <- system.file("report/html_report/SignalDetectionReport_strata.Rmd",
       package = "SignalDetectionTool")



    for(patho in pathogens){
      # formatted pathogen name (used for links)
      patho_f <- tolower(patho)
      patho_f <- gsub("[~(),./?&!#<>\\]", "", patho_f) #remove special characters
      patho_f <- gsub("\\s", "-", patho_f) # replace space with score

      # pathogen pages parameters
      signals_pad_p <- signals_padded %>%
        dplyr::filter(.data$pathogen == patho)

      signals_agg_p <-  signals_agg %>%
        dplyr::filter(.data$pathogen == patho)

      pathogen_report_params <- list(
        data = data,
        disease = patho,
        country = unique(data$country),
        number_of_weeks = number_of_weeks,
        strata = strata,
        signals_padded = signals_pad_p,
        signals_agg = signals_agg_p,
        intervention_date = intervention_date,
        title = title
      )

      # Render Pathogen pages
      rmarkdown::render(rmd_pathogen_path,
        output_format = output_format_s,
        params = pathogen_report_params,
        output_file = patho_f,
        output_dir = file.path(temp_dir, "report_pages")
      )

      for(ctg in strata){

        # strata pages parameters
        signals_pad_c <-  signals_padded %>%
          dplyr::filter(.data$pathogen == patho, .data$category == ctg)
        signals_agg_c <- signals_agg_p %>%
          dplyr::filter(.data$category == ctg)

        strata_report_params <- list(
          disease = patho,
          country = unique(data$country),
          number_of_weeks = number_of_weeks,
          category = ctg,
          signals_agg = signals_agg_c,
          signals_padded = signals_pad_c,
          intervention_date = intervention_date,
          title = title
        )

        # Render strata pages
        rmarkdown::render(rmd_strata_path,
          output_format = output_format_s,
          params = strata_report_params,
          output_file = paste(patho_f, ctg, sep  = "-"),
          output_dir = file.path(temp_dir, "report_pages")
        )
      }
    }

    # Render Pathogen page
    rmarkdown::render(rmd_path,
      output_format = output_format,
      params = report_params,
      output_file = "SignalDetectionReport.html",
      output_dir = temp_dir
    )

    # name for zip file
    if(output_file == normalizePath(file.path(dirname(output_file), basename(output_file)))){
      z_file <- gsub(".html", ".zip", output_file) # case when complete path is given in output_file
    } else {
      z_file <- file.path(output_dir, gsub(".html", ".zip", output_file)) # case when output_dir and output_file are given separately
    }

    zip::zipr(
      zipfile = z_file,
      files = c(file.path(temp_dir, "SignalDetectionReport.html"),
                file.path(temp_dir, "report_pages/"))
    )


  } else {
    rmd_path <- system.file("report/word_report/SignalDetectionReport.Rmd", package = "SignalDetectionTool")
    output_format <- "word_document"

    rmarkdown::render(rmd_path,
      output_format = output_format,
      params = report_params,
      output_file = output_file,
      output_dir = output_dir
    )
  }
}
