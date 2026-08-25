#' Get Signals for All Strata Including Unstratified
#'
#' This function computes signals for the provided preprocessed surveillance data,
#' using the specified detection method and optionally stratifies by given variables.
#' If stratification is applied, it also computes the unstratified signals and
#' appends them to the result, ensuring a unified output suitable for visualization.
#'
#' @param preprocessed_data A data frame that has been preprocessed using [preprocess_data()].
#' @param method A character string specifying the signal detection method to use.
#'   See [available_algorithms()] for options.
#' @param intervention_date A date or character string in yyyy-mm-dd format indicating the
#'   start of a post-intervention period for time series correction (only relevant for certain models).
#' @param stratification A character vector specifying the variables to stratify the analysis on.
#' @param date_start Optional. A date or character string in yyyy-mm-dd format indicating
#'   the beginning of the analysis period.
#' @param date_end Optional. A date or character string in yyyy-mm-dd format indicating
#'   the end of the analysis period.
#' @param date_ext A date object or character of format yyyy-mm-dd. Extends the aggregated dataset until this date. Default is NULL
#' @param date_var A character string specifying the column name of the date variable to use.
#'   Default is "date_report".
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly".
#' @param number_of_time_units Integer specifying how many time units to generate signals for.
#' @param alpha_upper numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not use the value; for these, the argument is ignored
#'   and internally set to NULL.
#'   Specifies the p-value cutoff used to compute the threshold; for example, a value of 0.05 corresponds to using the 0.95 quantile.
#' @param exclude_outbreak_cases_from_fitting A boolean specifying whether outbreak-associated case counts should be excluded only when fitting the baseline.
#'   `TRUE` can only be applied when GLM-based outbreak detection models are used. For other models only `FALSE` is a valid input.
#'   If `preprocessed_data` does not contain an `outbreak_status` column indicating the number of cases associated with outbreaks,
#'   no exclusion is applied, even when `exclude_outbreak_cases_from_fitting = TRUE`.
#'   Default is `FALSE`.
#'
#' @return A tibble with columns for signals, expected values, thresholds, and
#'   stratification information (if applicable), containing both stratified and
#'   unstratified results for comprehensive comparison or plotting.
#'
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' results_all <- get_signals_all(
#'   data_preprocessed,
#'   method = "farrington",
#'   stratification = c("sex", "age_group")
#' )
#' }
#' @export
get_signals_all <- function(preprocessed_data,
                            method = "farrington",
                            intervention_date = NULL,
                            stratification = NULL,
                            date_start = NULL,
                            date_end = NULL,
                            date_ext = NULL,
                            date_var = "date_report",
                            time_unit = "weekly",
                            number_of_time_units = 6,
                            alpha_upper = 0.05,
                            exclude_outbreak_cases_from_fitting = FALSE) {
  results <- get_signals(
    data = preprocessed_data,
    method = method,
    intervention_date = intervention_date,
    stratification = stratification,
    date_start = date_start,
    date_end = date_end,
    date_ext = date_ext,
    date_var = date_var,
    time_unit = time_unit,
    number_of_time_units = number_of_time_units,
    alpha_upper = alpha_upper,
    exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting
  )
  # when stratified signals were computed also add unstratified signals to the dataframe so that all can be visualised
  if (!is.null(stratification)) {
    results_unstratified <- get_signals(
      data = preprocessed_data,
      method = method,
      intervention_date = intervention_date,
      stratification = NULL,
      date_start = date_start,
      date_end = date_end,
      date_ext = date_ext,
      date_var = date_var,
      time_unit = time_unit,
      number_of_time_units = number_of_time_units,
      alpha_upper = alpha_upper,
      exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting
    )
    results <- dplyr::bind_rows(results, results_unstratified)
  }
  results
}


#' Get Signals Stratified
#'
#' This function stratifies and aggregates surveillance data by specified columns and analyzes
#' each stratum separately using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param fun The signal detection function to apply to each stratum.
#' @param model character, default empty string which is the choice if farrington, ears or cusum are used and if a glm method was chosen as outbreak detection method then one of c("mean","sincos", "sincos_multiS", "FN")
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param time_trend boolean default TRUE setting time_trend in the get_signals_glm(). This parameter is only used when an the glm based outbreak detection models are used, i.e. for the models c("mean","sincos", "FN")
#' @param stratification_columns A character vector specifying the columns to
#'   stratify the data by.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_ext A date object or character of format yyyy-mm-dd. Extends the aggregated dataset until this date. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly". Algorithms using the farrington framework can only be used with weekly aggregated data.
#' @param number_of_time_units integer, specifying number of time units to generate signals for.
#' @param alpha_upper numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not use the value; for these, the argument is ignored
#'   and internally set to NULL.
#'   Specifies the p-value cutoff used to compute the threshold; for example, a value of 0.05 corresponds to using the 0.95 quantile.
#' @param exclude_outbreak_cases_from_fitting A boolean specifying whether outbreak-associated case counts should be excluded only when fitting the baseline.
#'   `TRUE` can only be applied when GLM-based outbreak detection models are used. For other models only `FALSE` is a valid input.
#'   If `data` does not contain an `outbreak_status` column indicating the number of cases associated with outbreaks,
#'   no exclusion is applied, even when `exclude_outbreak_cases_from_fitting = TRUE`.
#'   Default is `FALSE`.
#' @return A tibble containing the results of the signal detection analysis
#'   stratified by the specified columns.
#'
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' categories <- c("county", "sex", "age_group") # Replace with actual column names
#' results <- get_signals_stratified(
#'   data_preprocessed,
#'   fun = get_signals_farringtonflexible,
#'   stratification_columns = categories
#' )
#' }
get_signals_stratified <- function(data,
                                   fun,
                                   model = "",
                                   intervention_date = NULL,
                                   time_trend = FALSE,
                                   stratification_columns,
                                   date_start = NULL,
                                   date_end = NULL,
                                   date_ext = NULL,
                                   date_var = "date_report",
                                   time_unit = "weekly",
                                   number_of_time_units = 6,
                                   alpha_upper = 0.05,
                                   exclude_outbreak_cases_from_fitting = FALSE) {
  # check that all columns are present in the data
  for (col in stratification_columns) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  checkmate::check_choice(model, choices = c("", "mean", "sincos", "sincos_multiS", "FN"))

  if (model != "" || identical(fun, get_signals_farringtonflexible)) {
    checkmate::assert(
      checkmate::check_number(alpha_upper, lower = 0.001, upper = 0.2)
    )
  } else {
    alpha_upper <- NULL
  }

  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )

  checkmate::check_flag(time_trend)

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_ext),
    checkmate::check_date(lubridate::date(date_ext)),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert_choice(
    time_unit,
    choices = c("weekly", "biweekly", "monthly"),
    null.ok = FALSE
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_time_units)
  )

  # accept TRUE/FALSE for GLM-based algorithms and only FALSE otherwise
  if (model != "") {
    checkmate::assert(
      checkmate::check_true(exclude_outbreak_cases_from_fitting),
      checkmate::check_false(exclude_outbreak_cases_from_fitting),
      combine = "or"
    )
  } else if (model == "") {
    checkmate::assert(
      checkmate::check_false(exclude_outbreak_cases_from_fitting)
    )
  }

  # Initialize an empty list to store results per category
  category_results <- list()

  # get min and max date of the whole dataset before stratification
  # stratified aggregated data can be filled up with 0s until min and max date
  # of the full dataset
  if (is.null(date_start)) {
    date_start <- min(data[[date_var]], na.rm = TRUE)
  }
  if (is.null(date_end)) {
    date_end <- max(data[[date_var]], na.rm = TRUE)
  }

  i <- 0
  # Loop through each category
  for (category in stratification_columns) {
    if (is.factor(data[, category])) {
      strata <- levels(droplevels(data[, category]))
    } else {
      strata <- unique(data[, category]) # character is supported as well
    }

    sub_data <- data %>%
      dplyr::mutate(
        !!rlang::sym(category) := factor(!!rlang::sym(category), levels = strata)
      )

    # adding the NAs to also calculate signals for them
    if (any(is.na(data[, category]))) {
      sub_data <- sub_data %>%
        dplyr::mutate(
          !!rlang::sym(category) := forcats::fct_na_value_to_level(!!rlang::sym(category), level = "NA")
        )
    }

    sub_data <- sub_data %>%
      # filter the data
      filter_by_date(date_var = date_var, date_start = date_start, date_end = date_end) %>%
      # aggregate data
      aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end, date_ext = date_ext, group = category, time_unit = time_unit, exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting)

    # add extension date information if available
    if (!is.null(date_ext)) {
      sub_data <- sub_data %>%
        dplyr::mutate(extension_date = date_ext)
    }

    split_list <- sub_data %>%
      dplyr::group_split(!!rlang::sym(category), .keep = FALSE)
    strata <- levels(sub_data[, category])
    names(split_list) <- strata

    # iterate over all strata and run algorithm
    for (stratum in strata) {
      i <- i + 1
      sub_data_agg <- split_list[[stratum]]
      # are there cases in the test period?
      n_cases <- sum(sub_data_agg %>% dplyr::slice_tail(n = number_of_time_units) %>% dplyr::select(cases))
      # run selected algorithm if there are cases
      if (n_cases == 0) {
        # don't run algorithm on those strata with 0 cases created by factors
        results <- sub_data_agg %>%
          # set alarms to FALSE for the timeperiod signals are generated for in the other present levels
          # logically the alarms column should also contain NA but later on computations are based on when the first alarm appears and when giving 0 timeseries to the algorithms they also put FALSE to the alarms column thus it is consistent
          # upperbound and expected to NA
          dplyr::mutate(alarms = dplyr::if_else(dplyr::row_number() >= (nrow(.) - number_of_time_units + 1), FALSE, NA)) %>%
          dplyr::mutate(
            upperbound = NA,
            expected = NA
          )
      } else {
        if (model != "") {
          results <- fun(sub_data_agg, number_of_time_units, model = model, time_trend = time_trend, intervention_date = intervention_date, alpha_upper = alpha_upper, exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting, time_unit = time_unit)
        } else if (identical(fun, get_signals_ears) || identical(fun, get_signals_cusum)) {
          results <- fun(sub_data_agg, number_of_time_units, time_unit = time_unit)
        } else if (identical(fun, get_signals_farringtonflexible)) {
          results <- fun(sub_data_agg, number_of_time_units, alpha_upper = alpha_upper)
        }
      }

      if (is.null(results)) {
        warning(paste0(
          "The stratum ", category, ":", stratum,
          " will be neglected due to lack of data."
        ))
      } else {
        # add information on stratification to results
        if (stratum == "NA") {
          stratum <- NA
        }
        results <- results %>% dplyr::mutate(
          category = category, stratum = stratum
        )
      }
      # Store the results in the list
      category_results[[i]] <- results
    }
  }

  return(dplyr::bind_rows(category_results))
}

#' Get Signals
#'
#' This function analyzes surveillance data to detect signals using the
#' specified method.
#'
#' @param data A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param method A character string specifying the signal detection method to use.
#'   Available options include:
#'   `"farrington"`, `"ears"`, `"cusum"`, `"glm mean"`, `"glm timetrend"`,
#'   `"glm harmonic"`, `"glm harmonic with timetrend"`, `"glm harmonic multi"`,
#'   `"glm farrington"`, `"glm farrington with timetrend"`.
#'   You can retrieve the full list using [available_algorithms()].
#'
#' @seealso [available_algorithms()]
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param stratification A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_ext A date object or character of format yyyy-mm-dd. Extends the aggregated dataset until this date. Default is NULL
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly". Algorithms using the farrington framework can only be used with weekly aggregated data.
#' @param number_of_time_units integer, specifying number of time units to generate signals for.
#' @param alpha_upper numeric between 0.001 and 0.2 (default: 0.05). Ears and cusum do not use the value; for these, the argument is ignored
#'   and internally set to NULL.
#'   Specifies the p-value cutoff used to compute the threshold; for example, a value of 0.05 corresponds to using the 0.95 quantile.
#' @param exclude_outbreak_cases_from_fitting A boolean specifying whether outbreak-associated case counts should be excluded only when fitting the baseline.
#'   `TRUE` can only be applied when GLM-based outbreak detection models are used. For other models only `FALSE` is a valid input.
#'   If `data` does not contain an `outbreak_status` column indicating the number of cases associated with outbreaks,
#'   no exclusion is applied, even when `exclude_outbreak_cases_from_fitting = TRUE`.
#'   Default is `FALSE`.
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>%
#'   preprocess_data()
#' results <- get_signals(
#'   data_preprocessed,
#'   method = "farrington",
#'   stratification = c("county", "sex")
#' )
#' results
#' }
get_signals <- function(data,
                        method = "farrington",
                        intervention_date = NULL,
                        stratification = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        date_ext = NULL,
                        date_var = "date_report",
                        time_unit = "weekly",
                        number_of_time_units = 6,
                        alpha_upper = 0.05,
                        exclude_outbreak_cases_from_fitting = FALSE) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = available_algorithms())
  )

  if (grepl("glm", method) || grepl("farrington", method)) {
    checkmate::assert(
      checkmate::check_number(alpha_upper, lower = 0.001, upper = 0.2)
    )
  } else {
    alpha_upper <- NULL
  }

  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_null(stratification),
    checkmate::check_vector(stratification),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_ext),
    checkmate::check_date(lubridate::date(date_ext)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )
  checkmate::assert_choice(
    time_unit,
    choices = c("weekly", "biweekly", "monthly"),
    null.ok = FALSE
  )

  if (grepl("farrington", method, ignore.case = TRUE)) {
    checkmate::assert_choice(time_unit, choices = "weekly")
  }

  checkmate::assert(
    checkmate::check_integerish(number_of_time_units)
  )

  if (grepl("glm", method)) {
    checkmate::assert(
      checkmate::check_true(exclude_outbreak_cases_from_fitting),
      checkmate::check_false(exclude_outbreak_cases_from_fitting),
      combine = "or"
    )
  } else {
    checkmate::assert(
      checkmate::check_false(exclude_outbreak_cases_from_fitting)
    )
  }

  model <- ""
  time_trend <- FALSE


  if (method == "farrington") {
    fun <- get_signals_farringtonflexible
  } else if (method == "aeddo") {
    fun <- get_signals_aeddo
  } else if (method == "ears") {
    fun <- get_signals_ears
  } else if (method == "cusum") {
    fun <- get_signals_cusum
  } else if (grepl("glm", method)) {
    fun <- get_signals_glm
    if (method == "glm mean") {
      model <- "mean"
      time_trend <- FALSE
    } else if (method == "glm timetrend") {
      model <- "mean"
      time_trend <- TRUE
    } else if (method == "glm harmonic") {
      model <- "sincos"
      time_trend <- FALSE
    } else if (method == "glm harmonic with timetrend") {
      model <- "sincos"
      time_trend <- TRUE
    } else if (method == "glm harmonic multi") {
      model <- "sincos_multiS"
      time_trend <- TRUE
    } else if (method == "glm farrington") {
      model <- "FN"
      time_trend <- FALSE
    } else if (method == "glm farrington with timetrend") {
      model <- "FN"
      time_trend <- TRUE
    }
  }

  if (is.null(stratification)) {
    data_agg <- data %>%
      # filter the data
      filter_by_date(date_start = date_start, date_end = date_end, date_var = date_var) %>%
      # aggregate and complete the data
      aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end, date_ext = date_ext, time_unit = time_unit, exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting)

    if (grepl("glm", method)) {
      results <- fun(data_agg, number_of_time_units, model = model, time_trend = time_trend, intervention_date = intervention_date, alpha_upper = alpha_upper, exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting, time_unit = time_unit)
    } else if (grepl("cusum", method, ignore.case = TRUE) || grepl("ears", method, ignore.case = TRUE)) {
      results <- fun(data_agg, number_of_time_units, time_unit = time_unit)
    } else {
      results <- fun(data_agg, number_of_time_units, alpha_upper = alpha_upper)
    }
    if (!is.null(results)) {
      results <- results %>%
        dplyr::mutate(category = NA, stratum = NA)
    }
  } else {
    results <- get_signals_stratified(
      data = data,
      fun = fun,
      model = model,
      intervention_date = intervention_date,
      time_trend = time_trend,
      stratification_columns = stratification,
      date_start = date_start,
      date_end = date_end,
      date_ext = date_ext,
      date_var = date_var,
      time_unit = time_unit,
      number_of_time_units = number_of_time_units,
      alpha_upper = alpha_upper,
      exclude_outbreak_cases_from_fitting = exclude_outbreak_cases_from_fitting
    )
  }

  # add number of time units, time unit and method to the results dataframe
  if (!is.null(results)) {
    results <- results %>%
      dplyr::mutate(
        method = method,
        number_of_time_units = number_of_time_units,
        time_unit = time_unit,
        alpha_upper = alpha_upper
      )

    # add extension date information if available
    if (!is.null(date_ext)) {
      results <- results %>%
        dplyr::mutate(extension_date = date_ext)
    }
  }

  return(results)
}

#' Aggregate and Pad Signals for Reporting
#'
#' This function combines the final weekly signal results into aggregate counts
#' (e.g. total cases, any alarms, number of alarms), and conditionally pads the
#' time series with historical expected values and thresholds prior to the signal
#' generation window. This is primarily used for report generation and visualization.
#'
#' @param signal_results A tibble returned by [get_signals()], containing weekly, biweekly or monthly
#'   signal detection results (cases, alarms, upperbound, expected, etc.).
#' @param preprocessed A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param number_of_time_units Integer specifying how many time units to include in the aggregation.
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly".
#' @param method A character string specifying the method used to generate the signals.
#'   Determines whether padding is necessary. For `"glm"` methods, padding is skipped
#'   as it is assumed to be already included.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{signals_agg}{A tibble with aggregated results per stratum, including total cases,
#'     whether any alarms occurred, and the number of alarms in the last `number_of_time_units`.}
#'   \item{signals_padded}{A tibble with the original `signal_results` augmented with additional
#'     rows containing historical `expected` and `upperbound` values (if padding was applied).}
#' }
#'
#' @details
#' Padding is applied only for non-GLM methods. It reconstructs the expected trajectory
#' before the signal detection window, which is useful for plotting full time series trends.
#'
#' @seealso [get_signals()], [aggregate_signals()], [pad_signals()]
#'
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>%
#'   preprocess_data()
#' results <- get_signals(
#'   data_preprocessed,
#'   method = "farrington"
#' )
#' output <- aggregate_pad_signals(
#'   signal_results = results,
#'   preprocessed = data_preprocessed,
#'   number_of_time_units = 6,
#'   time_unit = "weekly",
#'   method = "farrington"
#' )
#' output$signals_agg
#' output$signals_padded
#' }
#' @export
aggregate_pad_signals <- function(signal_results,
                                  preprocessed,
                                  number_of_time_units,
                                  time_unit,
                                  method) {
  # aggregate signals for report
  signals_agg <- aggregate_signals(signal_results, number_of_time_units = number_of_time_units, time_unit = time_unit)

  logic_apply_padding <- function() {
    if (grepl("glm", method)) {
      return(signal_results)
    }
    pad_signals(signal_results)
  }

  signals_padded <- logic_apply_padding()

  list(
    signals_agg = signals_agg,
    signals_padded = signals_padded
  )
}

#' Aggregate cases and signals over the number of time units.

#' First the signals are filtered to obtain the signals for the last n time units
#' aggregating the number of cases observed, create variable any signal generated and the aggregate the number of signals

#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per time unit, year
#' @param number_of_time_units integer, specifying the number of time units we want to aggregate the number of cases and the generated signals
#' @param time_unit a character specifying the time unit the case aggregation is performed on. Default is "weekly".
#' @returns tibble, with one line per groups containing the number of cases, any_alarms and n_alarms
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' results <- get_signals(
#'   data_preprocessed,
#'   stratification = c("sex", "county_id")
#' )
#' results_agg <- results %>% aggregate_signals(number_of_time_units = 6, time_unit = "weekly")
#' results_agg
#' }
#' @export
aggregate_signals <- function(signals, number_of_time_units, time_unit) {
  signals %>%
    filter_data_last_n_time_units(number_of_time_units = number_of_time_units, time_unit = time_unit) %>%
    dplyr::group_by(category, stratum) %>%
    dplyr::summarise(
      cases = sum(cases, na.rm = T),
      any_alarms = any(alarms, na.rm = T),
      n_alarms = sum(alarms, na.rm = T)
    ) %>%
    dplyr::ungroup()
}

#' Extend the computed threshold and expectation of the signal detection method to the past for visualisation purposes but not for signal generation

#' Inside the function it is computed what the maximum number of timepoints is the signal detection algorithms can be applied for. This depends on the algorithm and the amount of historic data. The already generated signals dataframe is then extended with the expectation and threshold into the past
#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per week, year
#' @returns tibble, with padded signals
#' @examples
#' \dontrun{
#' data_preprocessed <- input_example %>% preprocess_data()
#' results <- data_preprocessed %>% get_signals(stratification = c("sex", "county_id"))
#' results_padded <- pad_signals(results)
#' results_padded
#' }
#' @export
pad_signals <- function(signals) {
  # get the stratification, method, time_unit and number_of_time_units from the signals data
  stratification <- if (all(is.na(signals$category))) {
    NULL
  } else {
    unique(signals$category)[!is.na(unique(signals$category))]
  }

  number_of_time_units <- unique(signals$number_of_time_units)
  method <- unique(signals$method)
  time_unit <- unique(signals$time_unit)

  if ("extension_date" %in% names(signals)) {
    date_ext <- unique(signals$extension_date)
  } else {
    date_ext <- NULL
  }

  stopifnot(length(number_of_time_units) == 1)

  if ("extension_date" %in% names(signals)) {
    date_ext <- unique(signals$extension_date)
  } else {
    date_ext <- NULL
  }

  if (grepl("farrington", method)) {
    alpha_upper <- unique(signals$alpha_upper)
  } else {
    alpha_upper <- NULL
  }

  stopifnot(length(method) == 1)
  stopifnot(length(time_unit) == 1)
  stopifnot(is.null(date_ext) || length(date_ext) == 1)

  # select time unit column
  if (time_unit %in% c("weekly", "biweekly")) {
    time_unit_column <- "week"
  } else if (time_unit %in% c("monthly")) {
    time_unit_column <- "month"
  }

  # remove test period from signals object as this is essentially the aggregated data
  data_no_signals <- signals %>%
    dplyr::filter(is.na(.data[["alarms"]])) %>%
    dplyr::select("year", time_unit_column, "cases", "cases_in_outbreak", "category", "stratum")

  # getting necessary functions and options for method
  method_list <- get_method_func_parameters(method)

  # testing paddings for unstratified agg data
  strata_in_data <- unique(data_no_signals$category)
  if (any(is.na(strata_in_data))){ # unstratified timeseries present
    data_agg <- data_no_signals %>%
      dplyr::filter(is.na(category))
  } else { # generate unstratified timeseries from first stratification in data
    strata_in_data <- strata_in_data[!is.na(strata_in_data)]

    data_agg <- data_no_signals %>% 
      dplyr::filter(.data[["category"]] == strata_in_data[1]) %>% 
      dplyr::group_by(.data[["year"]], .data[[time_unit_column]]) %>%
      dplyr::summarise(dplyr::across(dplyr::contains("cases"), sum), .groups = "drop") %>% 
      dplyr::mutate(category = NA_character_, stratum = NA_character_)
  }

  available_thresholds <- c(26, 20, 14, 8, 2)
  for (timeopt in available_thresholds) {
    max_time_opt <- timeopt

    signals_timeopt <- run_method_parameters(method_list, data_agg,
      n_time_units = timeopt + number_of_time_units,
      time_unit = time_unit,
      intervention_date = NULL, # intervention_date,
      alpha_upper = alpha_upper,
      exclude_outbreak_cases_from_fitting = FALSE # exclude_outbreak_cases_from_fitting
    )

    if (!is.null(signals_timeopt)) {
      break
    }
  }

  result_padding_unstratified <- signals_timeopt %>%
    dplyr::select("year", time_unit_column, "category", "stratum", upperbound_pad = "upperbound", expected_pad = "expected")

  # preparing dataset with padding
  if (is.null(stratification)) {
    result_padding <- result_padding_unstratified
  } else {
    # loop for each category
    signals_category <- list()
    for (category_i in stratification) {
      strata <- data_no_signals %>%
        dplyr::filter(category == category_i) %>%
        dplyr::distinct(stratum) %>%
        dplyr::pull(stratum)

      # loop for each stratum
      signals_strata <- list()
      for (stratum_i in strata) {
        # filter stratum_i (can be NA)
        if (is.na(stratum_i)) {
          data_agg <- data_no_signals %>%
            dplyr::filter(category == category_i, is.na(stratum))
        } else {
          data_agg <- data_no_signals %>%
            dplyr::filter(category == category_i, stratum == stratum_i)
        }

        # are there cases in the test period?
        n_cases <- sum(data_agg %>% dplyr::slice_tail(n = max_time_opt + number_of_time_units) %>% dplyr::select("cases"))
        if (n_cases == 0) {
          # don't run algorithm on those strata with 0 cases created by factors
          signals_stratum_i <- data_agg %>%
            # set alarms to FALSE for the timeperiod signals are generated for in the other present levels
            # logically the alarms column should also contain NA but later on computations are based on when the first alarm appears and when giving 0 timeseries to the algorithms they also put FALSE to the alarms column thus it is consistent
            # upperbound and expected to NA
            dplyr::mutate(alarms = dplyr::if_else(dplyr::row_number() >= (nrow(.) - (max_time_opt + number_of_time_units) + 1), FALSE, NA)) %>%
            dplyr::mutate(
              upperbound = NA,
              expected = NA
            )
        } else {
          # run signal method
          signals_stratum_i <- run_method_parameters(method_list, data_agg,
            n_time_units = max_time_opt + number_of_time_units,
            time_unit = time_unit,
            intervention_date = NULL, # intervention_date,
            alpha_upper = alpha_upper,
            exclude_outbreak_cases_from_fitting = FALSE # exclude_outbreak_cases_from_fitting
          )
        }

        signals_strata[[stratum_i]] <- signals_stratum_i %>%
          dplyr::select("year", time_unit_column, "category", "stratum", upperbound_pad = "upperbound", expected_pad = "expected")
      }

      # join all strata results and save in category list
      signals_category[[category_i]] <- dplyr::bind_rows(signals_strata)
    }

    # join all category results
    result_padding_stratified <- dplyr::bind_rows(signals_category)

    result_padding <- dplyr::bind_rows(
      result_padding_stratified,
      result_padding_unstratified
    )
  }

  # preparing dataset within actual signal detection period
  results <- signals %>%
    dplyr::arrange(.data[["category"]], .data[["stratum"]], .data[["year"]], .data[[time_unit_column]]) %>%
    dplyr::left_join(x = ., y = result_padding, by = c("category", "stratum", "year", time_unit_column))

  # adjusting padding that the first upperbound which is calculated in the signals is set to the last upperbound padding such that no jump in the visualisation occurs
  results <- results %>%
    dplyr::group_by(category, stratum) %>%
    dplyr::mutate(first_timepoint_alarms = min(which(!is.na(alarms)))) %>%
    dplyr::mutate(first_alarm_nonNA = dplyr::if_else(dplyr::row_number() == first_timepoint_alarms, TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-first_timepoint_alarms) %>%
    dplyr::mutate(
      upperbound_pad = dplyr::if_else(first_alarm_nonNA, upperbound, upperbound_pad),
      expected_pad = dplyr::if_else(first_alarm_nonNA, expected, expected_pad)
    )


  return(results)
}

#' Get necessary functions and parameters for selected method
#'
#' @param method String. Method for singal detection. See [available_algorithms()].
#'
#' @returns list containing function object, model specification, and time trend boolean specific for the selected method
get_method_func_parameters <- function(method) {
  fun <- switch(method,
    "farrington" = get_signals_farringtonflexible,
    "aeddo" = get_signals_aeddo,
    "ears" = get_signals_ears,
    "cusum" = get_signals_cusum,
    "glm mean" = get_signals_glm,
    "glm timetrend" = get_signals_glm,
    "glm harmonic" = get_signals_glm,
    "glm harmonic with timetrend" = get_signals_glm,
    "glm harmonic multi" = get_signals_glm,
    "glm farrington" = get_signals_glm,
    "glm farrington with timetrend" = get_signals_glm
  )

  model_sp <- switch(method,
    "glm mean" = "mean",
    "glm timetrend" = "mean",
    "glm harmonic" = "sincos",
    "glm harmonic with timetrend" = "sincos",
    "glm harmonic multi" = "sincos_multiS",
    "glm farrington" = "FN",
    "glm farrington with timetrend" = "FN"
  )

  time_trend <- switch(method,
    "glm mean" = FALSE,
    "glm timetrend" = TRUE,
    "glm harmonic" = FALSE,
    "glm harmonic with timetrend" = TRUE,
    "glm harmonic multi" = TRUE,
    "glm farrington" = FALSE,
    "glm farrington with timetrend" = TRUE
  )

  return(list(method = method, fun = fun, model = model_sp, trend = time_trend))
}

#' Run method for signal detection using aggregated data
#'
#' Wrapper for running different signal detection algorithms using already aggregated data.
#' Used in combination with [get_method_func_parameters()].
#'
#' @param method_list List with method specification generated with [get_method_func_parameters()]
#' @param data_aggregated data.frame with timeseries of cases of one single stratification.
#' @param time_unit Character indicating the time unit aggregation (weekly, biweekly, or monthly)
#' @param n_time_units Integer. Time units of the Signal detection test period.
#' @param ... additional arguments for each specific method
#'
#' @returns data.frame with signal detection results
#'
#' @export
#' @examples
#' \dontrun{
#' dat <- input_example %>% preprocess_data()
#' dat_agg <- aggregate_data(dat)
#'
#' run_method_parameters(get_method_func_parameters("farrington"),
#'   data_aggregated = dat_agg,
#'   time_unit = "weekly",
#'   n_time_units = 6,
#'   alpha_upper = 0.05
#' )
#' }
run_method_parameters <- function(method_list, data_aggregated, time_unit, n_time_units, ...) {
  # extract function, model, and timetrend parameter
  method <- method_list[["method"]]
  fun <- method_list[["fun"]]
  model <- method_list[["model"]]
  time_trend <- method_list[["trend"]]

  # extract extra parameters
  extra_params <- list(...)

  # run method
  if (grepl("glm", method)) {
    method_results <- fun(data_aggregated,
      number_of_time_units = n_time_units,
      time_unit = time_unit,
      model = model,
      alpha_upper = extra_params$alpha_upper, time_trend = time_trend,
      intervention_date = extra_params$intervention_date,
      exclude_outbreak_cases_from_fitting = extra_params$exclude_outbreak_cases_from_fitting
    )
  } else if (grepl("farrington", method)) {
    method_results <- fun(data_aggregated,
      number_of_time_units = n_time_units,
      # time_unit = time_unit, # farrington function doesn't have time_unit parameter for now
      alpha_upper = extra_params$alpha_upper
    )
  } else {
    method_results <- fun(data_aggregated,
      number_of_time_units = n_time_units,
      time_unit = time_unit
    )
  }

  return(method_results)
}

#' Extract strata from precomputed signals_agg
#'
#' This helper function returns the list of stratification variables used
#' to generate a `signals_agg` object. If no stratification was applied
#' (i.e., all values in the `category` column are `NA`), it returns `NULL`.
#'
#' @param signals_agg A data frame containing precomputed signal aggregations, produced by [aggregate_signals()].
#'   Must include a `category` column that encodes the stratification variable(s).
#'
#' @return A character vector of stratification values (e.g., `"age_group"`, `"county"`),
#'   or `NULL` if no stratification was applied.
get_strata_from_signals_agg <- function(signals_agg) {
  # no stratification
  if (all(is.na(signals_agg$category))) {
    strata <- NULL
    # stratification
  } else {
    strata <- setdiff(unique(signals_agg$category), NA)
  }
  strata
}
