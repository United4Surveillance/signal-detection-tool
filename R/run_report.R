#' Renders a signal detection report
#'
#' Generates signal detection reports from surveillance linelist data.
#' HTML output supports both single-pathogen and multi-pathogen reports,
#' including pathogen-specific stratification defined via `strata`.
#' DOCX output currently supports single-pathogen reports only.
#'
#' If executed as a standalone function, all filtering of `data` must be
#' performed beforehand. This function is also invoked within the app.
#'
#' @param data A data.frame containing surveillance data in linelist format
#'   following the structure specified in `input_metadata`.
#' @param report_format Character scalar specifying the report format.
#'   Supported values are `"HTML"` and `"DOCX"`.
#' @param method Character scalar specifying the signal detection method.
#'   Must be one of `"FarringtonFlexible"`, `"EARS"`, `"CUSUM"`, `"Mean"`,
#'   `"Timetrend"`, `"Harmonic"`, `"Harmonic with timetrend"`, `Multi-seasonal harmonic`,
#'   `"Step harmonic"`, or `"Step harmonic with timetrend"`.
#'   Use [names(available_algorithms())] to retrieve the full list.
#' @param number_of_time_units integer, number of time units for which signals are generated
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly".
#' @param pathogens Character vector specifying which pathogens to include in
#'   the report. If `NULL`, all pathogens present in `data` are used when
#'   signals are recomputed; otherwise the pathogens in `signals_padded` are
#'   used. Multi-pathogen reports are supported only for HTML output.
#' @param strata Stratification specification. Supported values are `NULL`,
#'   `"None"`, a character vector of column names in `data`, or a data
#'   frame/tibble with columns `pathogen` and `strata`, where `strata` is a
#'   list-column of character vectors. Character input applies the same strata
#'   to all pathogens. Data-frame input allows pathogen-specific strata and must
#'   contain one entry for each pathogen included in the report; all referenced
#'   columns must exist in `data`. Pathogen-specific strata are supported only
#'   for HTML output. When precomputed signals are supplied, `strata` is not
#'   inferred from them and is still used to control report rendering.
#' @param selected_filter_vars A character vector for printing the variables used for filtering. If `NULL` either no filter is used or the filter is applied outside the app. Custom strings may be included.
#' @param tables Logical scalar; if `TRUE`, include signal detection tables in
#'   the report. Used only for DOCX output and ignored for HTML output.
#' @param output_file Character scalar specifying the output file name without a
#'   directory path. If `NULL`, a default file name is generated.
#' @param output_dir Character scalar specifying the output directory. Defaults
#'   to `"."`, i.e. the current working directory. In the Shiny app, `NULL`
#'   uses the default download location.
#' @param signals_padded Tibble of precomputed padded signals containing a
#'   `pathogen` column. If multiple pathogens are included, rows for all
#'   pathogens should be stacked in a single object, for example with
#'   `dplyr::bind_rows()`. To reuse precomputed signals and skip recomputation,
#'   `signals_padded` and `signals_agg` must both be supplied. If either object
#'   is `NULL`, both are recomputed from `data`.
#' @param signals_agg Tibble of precomputed aggregated signals containing a
#'   `pathogen` column. If multiple pathogens are included, rows for all
#'   pathogens should be stacked in a single object, for example with
#'   `dplyr::bind_rows()`. To reuse precomputed signals and skip recomputation,
#'   `signals_agg` and `signals_padded` must both be supplied. If either object
#'   is `NULL`, both are recomputed from `data`.
#' @param intervention_date Date object, character string in `"yyyy-mm-dd"`
#'   format, or `NULL` specifying the intervention date for interrupted time
#'   series analysis. Supported only by the methods `"Mean"`, `"Timetrend"`,
#'   `"Harmonic"`, `"Harmonic with timetrend"`, `"Multi-seasonal harmonic"`, `"Step harmonic"`,
#'   and`"Step harmonic with timetrend"`. The default `NULL` disables the
#'   intervention analysis.
#' @param custom_logo Character scalar giving the path to a PNG or SVG logo
#'   that replaces the default United4Surveillance logo. Used only for HTML
#'   output.
#' @param custom_theme A [bslib::bs_theme()] object that replaces the default
#'   United4Surveillance theme. This is mainly used to change colors. Use
#'   `version = "3"` to keep the navbar intact. Used only for HTML output.
#' @param min_cases_signals Integer scalar giving the minimum number of cases an
#'   alarm must have to remain flagged. For observations with fewer than this
#'   number of cases, `alarms` is set to `FALSE` in a post-processing step.
#'   This is applied only when signals are recomputed inside `run_report()`,
#'   i.e. when `signals_agg` or `signals_padded` is `NULL`.
#' @param title `NULL` or a character scalar specifying the report title. If
#'   `NULL` or an empty string, a default title of the form
#'   `"Signal Detection Report - <country>"` is used.
#' @param alpha_upper Numeric between 0.001 and 0.2. Specifies the p-value cutoff used to compute the threshold; for example, a value of 0.05 corresponds to
#'   using the 0.95 quantile. `alpha_upper` is only used for methods that require it (currently "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend", "Multi-seasonal harmonic", "Step harmonic", "Step harmonic with timetrend"), for which a default value of 0.05 is applied.
#'   Ears and cusum do not use the value; for these, the argument is ignored and internally set to NULL.
#'
#' @return Returns the path to the rendered output written to disk. For DOCX
#'   output this is the Word document; for HTML output this is the ZIP archive
#'   containing the landing page and generated report pages.
#' @seealso [names(available_algorithms())]
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Run a report with specified parameters and HTML output
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example,
#'   method = "FarringtonFlexible",
#'   strata = c("county", "sex"),
#'   number_of_time_units = 6
#' )
#'
#' # Example 2: Specify an output directory
#' run_report(
#'   method = "EARS",
#'   output_dir = "C:/Users/SmithJ/Documents"
#' )
#'
#' # Example 3: Specify an output file name
#' run_report(
#'   method = "EARS",
#'   output_file = "My Signal Report"
#' )
#'
#' # Example 4: Do not use stratification
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example,
#'   method = "EARS",
#'   strata = NULL
#' )
#'
#' # Example 5: Create an HTML report for multiple pathogens
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example_multipathogen,
#'   method = "Harmonic"
#' )
#'
#' # Example 6: Restrict a multi-pathogen HTML report to a subset of pathogens
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example_multipathogen,
#'   pathogens = c("Enterobacter", "Salmonella"),
#'   method = "Harmonic"
#' )
#'
#' # Example 7: Use pathogen-specific strata in an HTML report
#' pathogen_strata <- tibble::tibble(
#'   pathogen = c("Enterobacter", "Salmonella"),
#'   strata = list(c("county", "age_group"), c("sex"))
#' )
#' run_report(
#'   report_format = "HTML",
#'   data = SignalDetectionTool::input_example_multipathogen,
#'   pathogens = c("Enterobacter", "Salmonella"),
#'   strata = pathogen_strata,
#'   method = "Harmonic"
#' )
#' }
run_report <- function(
  data,
  report_format = "HTML",
  method = "FarringtonFlexible",
  number_of_time_units = 6,
  time_unit = "weekly",
  pathogens = NULL,
  strata = NULL,
  selected_filter_vars = NULL,
  tables = TRUE,
  output_file = NULL,
  output_dir = ".",
  signals_padded = NULL,
  signals_agg = NULL,
  intervention_date = NULL,
  custom_logo = NULL,
  custom_theme = NULL,
  min_cases_signals = 1,
  title = NULL,
  alpha_upper = 0.05
) {
  # Currently multi pathogen report is only supported for HTML
  if ((report_format == "DOCX" & length(unique(data$pathogen)) > 1) | report_format == "DOCX" & is.data.frame(strata)) {
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

  if (!grepl("cusum", method, ignore.case = TRUE) && !grepl("ears", method, ignore.case = TRUE)) {
    checkmate::assert(
      checkmate::check_number(alpha_upper, lower = 0.001, upper = 0.2)
    )
  } else {
    alpha_upper <- NULL
  }

  checkmate::assert(
    checkmate::check_integerish(number_of_time_units, lower = 1)
  )

  checkmate::assert_choice(
    time_unit,
    choices = c("weekly", "biweekly", "monthly"),
    null.ok = FALSE
  )
#
#   if (grepl("farrington", method, ignore.case = TRUE) | grepl("^step harmonic$", method, ignore.case = TRUE)) {
#     checkmate::assert_choice(time_unit, choices = "weekly")
#   }

  # assert pathogens is NULL (default includes all pathogens) or exist in dataframe or padded signals
  checkmate::assert(
    checkmate::check_null(pathogens),
    checkmate::check_subset(pathogens, choices = unique(data$pathogen)),
    checkmate::check_subset(pathogens, choices = unique(signals_padded$pathogen)),
    combine = "or"
  )

  checkmate::assert_logical(tables)
  checkmate::assert_character(output_file, null.ok = TRUE, len = 1)
  checkmate::assert_string(output_dir, null.ok = TRUE)

  # give default name if none is supplied
  if (is.null(output_file)) {
    output_file <- paste0(
      "SignalDetectionReport.",
      switch(report_format,
        HTML = "html",
        DOCX = "docx"
      )
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
  if (!is.null(signals_agg)) {
    checkmate::assert_true("pathogen" %in% names(signals_agg))
  }
  checkmate::assert(
    checkmate::check_null(signals_padded),
    checkmate::check_data_frame(signals_padded, col.names = "named"),
    combine = "or"
  )
  # additional checks specific to the data frame
  # pathogen needs to be added to signals_padded
  if (!is.null(signals_padded)) {
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
    checkmate::check_integerish(min_cases_signals, lower = 1)
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

  # Validate strata
  if ("None" %in% strata) {
    strata <- NULL
  }
  # validate it here because now pathogens always have a non NULL value
  check_strata(strata, pathogens, data)

  # compute signals if not provided to run_report by the user
  if (is.null(signals_agg) | is.null(signals_padded)) {
    precomputed <- FALSE

    preprocessed_data <- data %>% preprocess_data()

    signals_agg_list <- list()
    signals_padded_list <- list()

    for (pat in pathogens) {
      preprocessed_data_pat <- preprocessed_data %>%
        dplyr::filter(pathogen == pat)

      strata_per_path <- get_strata_for_path(strata, pat)

      signals <- get_signals_all(preprocessed_data_pat,
        method = method,
        intervention_date = intervention_date,
        stratification = strata_per_path, # hier auch Erregerspezifisches Stratum verwenden
        date_start = NULL,
        date_end = NULL,
        date_var = "date_report",
        number_of_time_units = number_of_time_units,
        time_unit = time_unit,
        alpha_upper = alpha_upper
      ) %>%
        dplyr::mutate(
          pathogen = pat,
          alarms = dplyr::if_else(alarms & cases < min_cases_signals,
            FALSE, alarms, missing = alarms
          )
        )

      signals_agg_pad <- aggregate_pad_signals(
        signals,
        preprocessed_data_pat,
        number_of_time_units,
        time_unit,
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
  } else {
    precomputed <- TRUE
  }

  title <- if (is.null(title) || trimws(title) == "") paste0("Signal Detection Report - ", unique(data$country)) else title

  report_params <- list(
    data = data,
    country = unique(data$country),
    disease = pathogens,
    number_of_time_units = number_of_time_units,
    time_unit = time_unit,
    method = method,
    selected_filter_vars = selected_filter_vars,
    signals_padded = signals_padded,
    signals_agg = signals_agg,
    intervention_date = intervention_date,
    title = title,
    alpha_upper = alpha_upper
  )

  if (report_format == "DOCX") {
    report_params$tables <- tables
    report_params$strata <- get_strata_for_path(strata, NULL)
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
      package = "SignalDetectionTool"
    )
    rmd_strata_path <- system.file("report/html_report/SignalDetectionReport_strata.Rmd",
      package = "SignalDetectionTool"
    )


    for (patho in pathogens) {
      # formatted pathogen name (used for links)
      patho_f <- tolower(patho)
      patho_f <- gsub("[~(),./?&!#<>\\]", "", patho_f) # remove special characters
      patho_f <- gsub("\\s", "-", patho_f) # replace space with score

      # pathogen pages parameters
      signals_pad_p <- signals_padded %>%
        dplyr::filter(.data$pathogen == patho)

      signals_agg_p <- signals_agg %>%
        dplyr::filter(.data$pathogen == patho)

      # pathogen specific strata are obtained if given
      if (precomputed) {
        strata_per_path <- get_strata_from_signals_agg(signals_agg_p)
      } else {
        strata_per_path <- get_strata_for_path(strata, patho)
      }

      pathogen_report_params <- list(
        data = data,
        disease = patho,
        country = unique(data$country),
        number_of_time_units = number_of_time_units,
        time_unit = time_unit,
        strata = strata_per_path,
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

      for (ctg in strata_per_path) {
        # strata pages parameters
        signals_pad_c <- signals_padded %>%
          dplyr::filter(.data$pathogen == patho, .data$category == ctg)
        signals_agg_c <- signals_agg_p %>%
          dplyr::filter(.data$category == ctg)

        strata_report_params <- list(
          disease = patho,
          country = unique(data$country),
          number_of_time_units = number_of_time_units,
          time_unit = time_unit,
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
          output_file = paste(patho_f, ctg, sep = "-"),
          output_dir = file.path(temp_dir, "report_pages")
        )
      }
    }

    # Render Landing Page
    rmarkdown::render(rmd_path,
      output_format = output_format,
      params = report_params,
      output_file = "SignalDetectionReport.html",
      output_dir = temp_dir
    )

    # name for zip file
    if (output_file == normalizePath(file.path(dirname(output_file), basename(output_file)))) {
      z_file <- gsub(".html", ".zip", output_file) # case when complete path is given in output_file
    } else {
      z_file <- file.path(output_dir, gsub(".html", ".zip", output_file)) # case when output_dir and output_file are given separately
    }

    zip::zipr(
      zipfile = z_file,
      files = c(
        file.path(temp_dir, "SignalDetectionReport.html"),
        file.path(temp_dir, "report_pages/")
      )
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

#' Resolve the strata specification for a single pathogen
#'
#' Normalizes the `strata` argument used by [run_report()] to the character
#' vector required for one pathogen. `NULL` remains `NULL`, character input is
#' returned unchanged (except for `"None"`, which is converted to `NULL`), and
#' data-frame input is matched against `pat`.
#'
#' @param strata Stratification specification as accepted by [run_report()].
#' @param pat Optional character scalar giving the pathogen for which strata
#'   should be returned. Required when `strata` is a data frame/tibble with
#'   pathogen-specific definitions.
#'
#' @return `NULL` or a character vector of column names to use for
#'   stratification for the selected pathogen.
#' @noRd
get_strata_for_path <- function(strata, pat = NULL) {
  if (is.null(strata)) {
    return(NULL)
  }

  # usage from the app and previous usage of strata
  if (is.character(strata)) {
    if ("None" %in% strata) {
      return(NULL)
    }
    return(strata)
  }

  if (is.data.frame(strata)) {
    # check that pat is given to match strata to given pathogen, before pat is not needed
    checkmate::assert_string(pat)

    i <- match(pat, strata$pathogen)
    return(strata$strata[[i]])
  }
}

#' Validate a stratification specification
#'
#' Validates the `strata` argument accepted by [run_report()]. Allowed inputs
#' are `NULL`, a character vector of column names in `data`, or a data
#' frame/tibble with columns `pathogen` and `strata`, where `strata` is a
#' list-column of character vectors. For data-frame input, all pathogens in
#' `pathogens` must be covered, no unknown pathogens may be present, and every
#' referenced stratum must exist as a column in `data`.
#'
#' @param strata Stratification specification to validate.
#' @param pathogens Character vector of pathogens that will be included in the
#'   report.
#' @param data Data frame containing the source linelist data.
#'
#' @return Invisibly returns `TRUE`. An error is thrown if `strata` is invalid.
#' @noRd
check_strata <- function(strata, pathogens, data) {
  # NULL is allowed
  if (is.null(strata)) {
    return(invisible(TRUE))
  }

  # character vector erlaubt
  if (is.character(strata)) {
    checkmate::assert_character(strata, min.len = 1, any.missing = FALSE)
    for (col in strata) {
      checkmate::assert(
        checkmate::check_choice(col, choices = names(data))
      )
    }
    return(invisible(TRUE))
  }

  # 3. Must be a tibble / data frame
  if (!inherits(strata, "data.frame")) {
    stop(
      "`strata` must be NULL, \"None\", a character vector or a tibble.",
      call. = FALSE
    )
  }

  # Required columns
  required_cols <- c("pathogen", "strata")
  missing_cols <- setdiff(required_cols, names(strata))

  if (length(missing_cols) > 0) {
    stop(
      "The following columns are missing from the `strata` dataframe: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # pathogen column must be character
  if (!is.character(strata$pathogen)) {
    stop("`strata$pathogen` must be a character vector.", call. = FALSE)
  }

  # strata column must be a list of character vectors
  if (!is.list(strata$strata)) {
    stop(
      "`strata$strata` must be a list-column of character vectors.",
      call. = FALSE
    )
  }

  # Pathogen coverage checks
  unknown_pathogens <- setdiff(strata$pathogen, pathogens)
  if (length(unknown_pathogens) > 0) {
    stop(
      "There are pathogens in the stratification that were not specified in the parameter pathogens: ",
      paste(unknown_pathogens, collapse = ", "),
      call. = FALSE
    )
  }

  missing_pathogens <- setdiff(pathogens, strata$pathogen)
  if (length(missing_pathogens) > 0) {
    stop(
      "Missing strata definitions for pathogens: ",
      paste(missing_pathogens, collapse = ", "),
      call. = FALSE
    )
  }

  all_strata <- unique(unlist(strata$strata, use.names = FALSE))
  for (col in all_strata) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  invisible(TRUE)
}
