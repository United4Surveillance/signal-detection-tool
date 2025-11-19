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
#' @param date_var A character string specifying the column name of the date variable to use.
#'   Default is "date_report".
#' @param number_of_weeks Integer specifying how many weeks to generate signals for.
#'
#' @return A tibble with columns for signals, expected values, thresholds, and
#'   stratification information (if applicable), containing both stratified and
#'   unstratified results for comprehensive comparison or plotting.
#'
#' @examples
#' \dontrun{
#' preprocessed <- preprocess_data(input_data)
#' results_all <- get_signals_all(
#'   preprocessed,
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
                            date_var = "date_report",
                            number_of_weeks = 6) {
  results <- get_signals(
    data = preprocessed_data,
    method = method,
    intervention_date = intervention_date,
    stratification = stratification,
    date_start = date_start,
    date_end = date_end,
    date_var = date_var,
    number_of_weeks = number_of_weeks
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
      date_var = date_var,
      number_of_weeks = number_of_weeks
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
#' @param model character, default empty string which is the choice if farrington, ears or cusum are used and if a glm method was chosen as outbreak detection method then one of c("mean","sincos", "FN")
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param time_trend boolean default TRUE setting time_trend in the get_signals_glm(). This parameter is only used when an the glm based outbreak detection models are used, i.e. for the models c("mean","sincos", "FN")
#' @param stratification_columns A character vector specifying the columns to
#'   stratify the data by.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate signals for.
#' @return A tibble containing the results of the signal detection analysis
#'   stratified by the specified columns.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("../data/input/input.csv")
#' categories <- c("county", "sex", "age_group") # Replace with actual column names
#' results <- get_signals_stratified(
#'   data,
#'   fun = get_signals_farringtonflexible,
#'   stratification_columns = categories
#' )
#' print(results)
#' }
get_signals_stratified <- function(data,
                                   fun,
                                   model = "",
                                   intervention_date = NULL,
                                   time_trend = FALSE,
                                   stratification_columns,
                                   date_start = NULL,
                                   date_end = NULL,
                                   date_var = "date_report",
                                   number_of_weeks = 6) {
  # check that all columns are present in the data
  for (col in stratification_columns) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  checkmate::check_choice(model, choices = c("", "mean", "sincos", "FN"))

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
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

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
        !!rlang::sym(category) := factor(!!rlang::sym(category), levels = strata))

    # adding the NAs to also calculate signals for them
    if (any(is.na(data[, category]))) {
      sub_data <- sub_data |>
        dplyr::mutate(
          !!rlang::sym(category) := forcats::fct_na_value_to_level(!!rlang::sym(category), level = "NA"))
    }
    sub_data <- sub_data |>
      # filter the data
      filter_by_date(date_var = date_var, date_start = date_start, date_end = date_end) %>%
      # aggregate data
      aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end, group = category)

    split_list <- sub_data %>%
      dplyr::group_split(!!rlang::sym(category), .keep = FALSE)
    strata <- levels(sub_data[, category])
    names(split_list) <- strata

    # iterate over all strata and run algorithm
    for (stratum in strata) {
      i <- i + 1
      sub_data_agg <- split_list[[stratum]]
      # are there cases in the test period?
      n_cases <- sum(sub_data_agg %>% slice_tail(n = number_of_weeks) %>% select(cases))
      # run selected algorithm if there are cases
      if (n_cases == 0) {
        # don't run algorithm on those strata with 0 cases created by factors
        results <- sub_data_agg %>%
          # set alarms to FALSE for the timeperiod signals are generated for in the other present levels
          # logically the alarms column should also contain NA but later on computations are based on when the first alarm appears and when giving 0 timeseries to the algorithms they also put FALSE to the alarms column thus it is consistent
          # upperbound and expected to NA
          dplyr::mutate(alarms = dplyr::if_else(dplyr::row_number() > (nrow(.) - number_of_weeks + 1), FALSE, NA)) %>%
          dplyr::mutate(
            upperbound = NA,
            expected = NA
          )
      } else {
        if (model != "") {
          results <- fun(sub_data_agg, number_of_weeks, model = model, time_trend = time_trend, intervention_date = intervention_date)
        } else {
          results <- fun(sub_data_agg, number_of_weeks)
        }
      }

      if (is.null(results)) {
        warning(paste0(
          "The stratum ", category, ":", stratum,
          " will be neglected due to lack of data."
        ))
      } else {
        # add information on stratification to results
        if (stratum == "NA")
          stratum <- NA
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
#'   `"glm harmonic"`, `"glm harmonic with timetrend"`,
#'   `"glm farrington"`, `"glm farrington with timetrend"`.
#'   You can retrieve the full list using [available_algorithms()].
#'
#' @seealso [available_algorithms()]
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param stratification A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate signals for.
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(
#'     method = "farrington",
#'     stratification = c("county", "sex")
#'   )
#' }
get_signals <- function(data,
                        method = "farrington",
                        intervention_date = NULL,
                        stratification = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        date_var = "date_report",
                        number_of_weeks = 6) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = available_algorithms())
  )

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
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

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
    } else if (method == "glm farrington") {
      model <- "FN"
      time_trend <- FALSE
    } else if (method == "glm farrington with timetrend") {
      model <- "FN"
      time_trend <- TRUE
    }
  }

  data <- data |>
    add_cw_iso(date_start = date_start, date_end = date_end, date_var = date_var)


  if (is.null(stratification)) {
    data_agg <- data %>%
      # filter the data
      filter_by_date(date_start = date_start, date_end = date_end, date_var = date_var) %>%
      # aggregate and complete the data
      aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end)

    if (grepl("glm", method)) {
      results <- fun(data_agg, number_of_weeks, model = model, time_trend = time_trend, intervention_date = intervention_date)
    } else {
      results <- fun(data_agg, number_of_weeks)
    }
    if (!is.null(results)) {
      results <- results %>%
        dplyr::mutate(category = NA, stratum = NA)
    }
  } else {
    results <- get_signals_stratified(
      data,
      fun,
      model = model,
      intervention_date = intervention_date,
      time_trend = time_trend,
      stratification,
      date_start,
      date_end,
      date_var,
      number_of_weeks
    )
  }

  # add number of weeks and method to the results dataframe
  if (!is.null(results)) {
    results <- results %>%
      dplyr::mutate(
        method = method,
        number_of_weeks = number_of_weeks
      )
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
#' @param signal_results A tibble returned by [get_signals()], containing weekly
#'   signal detection results (cases, alarms, upperbound, expected, etc.).
#' @param preprocessed A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param number_of_weeks Integer specifying how many weeks to include in the aggregation.
#' @param method A character string specifying the method used to generate the signals.
#'   Determines whether padding is necessary. For `"glm"` methods, padding is skipped
#'   as it is assumed to be already included.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{signals_agg}{A tibble with aggregated results per stratum, including total cases,
#'     whether any alarms occurred, and the number of alarms in the last `number_of_weeks`.}
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
#' results <- get_signals(preprocessed_data, method = "farrington")
#' output <- aggregate_pad_signals(results, number_of_weeks = 6, method = "farrington")
#' output$signals_agg
#' output$signals_padded
#' }
#' @export
aggregate_pad_signals <- function(signal_results,
                                  preprocessed,
                                  number_of_weeks,
                                  method) {
  # aggregate signals for report
  signals_agg <- aggregate_signals(signal_results, number_of_weeks = number_of_weeks)


  # padd timeseries so it also has information before detection period
  logic_apply_padding <- function() {
    if (grepl("glm", method)) {
      # for those the results are already padded
      return(signal_results)
    }
    pad_signals(preprocessed, signal_results)
  }

  signals_padded <- logic_apply_padding()

  list(
    signals_agg = signals_agg,
    signals_padded = signals_padded
  )
}

#' Aggregate cases and signals over the number of weeks.

#' First the signals are filtered to obtain the signals for the last n weeks
#' aggregating the number of cases observed, create variable any signal generated and the aggregate the number of signals

#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per week, year
#' @param number_of_weeks integer, specifying the number of weeks we want to aggregate the number of cases and the generated signals
#' @returns tibble, with one line per groups containing the number of cases, any_alarms and n_alarms
#' @examples
#' \dontrun{
#' signals <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(stratification = c("sex", "county_id"))
#' signals %>% aggregate_signals(number_of_weeks = 6)
#' }
#' @export
aggregate_signals <- function(signals, number_of_weeks) {
  signals %>%
    filter_data_last_n_weeks(number_of_weeks = number_of_weeks) %>%
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
#' @param data A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param signals tibble, output of the \code{\link{get_signals}} function with number of cases and signal per week, year
#' @returns tibble, with padded signals
#' @examples
#' \dontrun{
#' input_example_prepro <- input_example %>%
#'   preprocess_data()
#' signals <- input_example_prepro %>%
#'   get_signals(stratification = c("sex", "county_id"))
#' pad_signals(input_example_prepro, signals)
#' }
#' @export
pad_signals <- function(data,
                        signals) {
  # get the stratification, method and number_of_weeks from the signals data
  stratification <- if (all(is.na(signals$category))) {
    NULL
  } else {
    unique(signals$category)[!is.na(unique(signals$category))]
  }

  # data_signals <- signals |>
  #   dplyr::filter(!is.na(alarms)) |>
  #   dplyr::select(year, week, category, stratum, upperbound_pad = upperbound, expected_pad = expected)


  number_of_weeks <- unique(signals$number_of_weeks)
  method <- unique(signals$method)

  stopifnot(length(number_of_weeks) == 1)
  stopifnot(length(method) == 1)

  cutoff_date <- max(data$date_report, na.rm = TRUE) - lubridate::weeks(number_of_weeks)

  data_no_signals <- data |>
    dplyr::filter(date_report <= cutoff_date)

  available_thresholds <- c(26, 20, 14, 8, 2)
  for (timeopt in available_thresholds) {
    max_time_opt <- timeopt
    signals_timeopt <- get_signals(
      data_no_signals,
      method = method,
      number_of_weeks = timeopt + number_of_weeks
    )
    if (!is.null(signals_timeopt)) {
      break
    }
  }

  result_padding_unstratified <- signals_timeopt %>%
    dplyr::select(year, week, category, stratum, upperbound_pad = upperbound, expected_pad = expected)

  # preparing dataset with padding
  if (is.null(stratification)) {
    result_padding <- result_padding_unstratified
  } else {
    result_padding_stratified <- SignalDetectionTool::get_signals(
      data = data,
      method = method,
      date_var = "date_report",
      stratification = stratification,
      number_of_weeks = (max_time_opt + number_of_weeks)
    ) %>%
      dplyr::select(year, week, upperbound_pad = upperbound, expected_pad = expected, category, stratum) %>%
      dplyr::group_by(category, stratum) %>%
      dplyr::slice_head(n = -(number_of_weeks - 1)) %>%
      dplyr::ungroup()

    result_padding <- dplyr::bind_rows(
      result_padding_stratified,
      result_padding_unstratified
      )
  }

  # preparing dataset within actual signal detection period
  results <- signals %>%
    dplyr::arrange(category, stratum, year, week) %>%
    dplyr::left_join(x = ., y = result_padding, by = c("category", "stratum", "year", "week"))


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
