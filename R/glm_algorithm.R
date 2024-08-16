#' Create a data.frame with 10 seasgroups components for harmonic modeling.
#'
#' This function generates seasonal group data required for fitting a Farrington-like Generalized Linear Model (GLM). The function leverages the `surveillance` package to create a dataset that includes seasonal groupings based on historical data, which is particularly useful for modeling seasonality in time series data.
#'
#' @param ts_length integer, specifying the length of the time series for which the seasonal group data is to be generated.
#' @param freq integer, default 52, specifying the frequency of the time series (e.g., 52 for weekly data).
#' @return A data frame containing the seasonal groupings (`seasgroups`) for the specified time series length. These seasonal groupings are used as covariates in the Farrington GLM model to account for seasonality.
#' @examples
#' \dontrun{
#' # Generate seasonal group data for a time series of length 100 with weekly frequency
#' create_fn_data(100)
#' }
create_fn_data <- function(ts_length, freq = 52) {
  # *2 just making the computation of seasgroups overall more stable
  time_point_to_consider <- ts_length * 2
  survts <- surveillance::sts(rep(0, time_point_to_consider),
    start = c(2000, 1),
    frequency = freq
  )

  # Create data for Farrington GLM
  modelData <- surveillance:::algo.farrington.data.glm(
    dayToConsider = time_point_to_consider, b = floor((time_point_to_consider - 3) / freq),
    freq = freq, epochAsDate = FALSE,
    epochStr = "none", vectorOfDates = 1:time_point_to_consider,
    w = 3, noPeriods = 10, observed = survts@observed[, 1],
    population = rep(0, 1000), verbose = FALSE,
    pastWeeksNotIncluded = 0, k = time_point_to_consider
  )[, 1:4]

  subset_seasgroups <- (nrow(modelData) - ts_length + 1):nrow(modelData)
  modelData[subset_seasgroups, ] %>%
    dplyr::select(seasgroups)
}

#' Create a data.frame with sine and cosine components for harmonic modeling.
#'
#' This function generates a data.frame with sine and cosine values based on the provided time series length and frequency. It is primarily used to create harmonic models that describe periodic patterns in time series data. The dataframe will be used to fit parameters of a glm with sin and cos elements.
#'
#' @param ts_len integer, specifying the length of the time series.
#' @param freq integer, default 52, specifying the frequency of the sine and cosine waves. When the timeseries is aggregated weekly then freq = 52.
#' @param S integer, default 1, specifying the number of cycles per freq. When freq = 52 this specifies the number of cycles to have per year
#' @return A data frame with columns for the sine and cosine values over time.
create_sincos_data <- function(ts_len, freq = 52, S = 1) {
  checkmate::assert_number(S, lower = 1)

  modelData <- data.frame(
    sin = sin(2 * pi * S * (1:ts_len) / freq),
    cos = cos(2 * pi * S * (1:ts_len) / freq)
  )
  modelData
}

#' Create Model Data for Generalized Linear Modeling
#'
#' This function generates a dataset containing all the necessary columns for fitting a Generalized Linear Model (GLM) in the context of epidemiological time series analysis. The generated dataset can include components for seasonality, time trends, and baseline adjustments, depending on the specified model type and intervention parameters.
#'
#' @param ts_len integer, specifying the length of the aggregated timeseries of case counts
#' @param model character, default "mean" one of c("mean", "sincos", "FN") specifying which kind of model the glm is fitting. "mean" fits an intercept model, "sincos" a harmonic sincos model, "FN" uses the seasgroups from farrington to fit parameters for seasonality.
#' @param time_trend boolean, default TRUE, when TRUE a timetrend is fitted in the glm describing the expected number of cases.
#' @param intervention_start integer, specifying the rownumber in the aggregated timeseries which corresponds to the intervention date.
#' @param min_timepoints_baseline integer, default 12, this parameter is only used when intervention_start_date is not NULL, specifying the number of weeks at least needed for fitting a new baseline after the intervention.
#' @param min_timepoints_trend integer, default 12, this parameter is only used when intervention_start_date is not NULL, specifying the number of weeks at least needed for fitting a new timetrend after the intervention.
#' @return data.frame containing all columns needed for the glm model. These are columns for the seasonality, time_trend and intercepts. This model data is used to fit the parameters for these coviariates.
#' @examples \dontrun{
#' create_model_data(100)
#' create_model_data(100, intervention_start = 50)
#' }
create_model_data <- function(ts_len,
                              model = "mean",
                              time_trend = TRUE,
                              intervention_start = NULL,
                              min_timepoints_baseline = 12,
                              min_timepoints_trend = 12) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(model, choices = c("mean", "sincos", "FN"))
  )

  data_season <- NULL
  if (model == "sincos") {
    data_season <- create_sincos_data(ts_len)
  } else if (model == "FN") {
    data_season <- create_fn_data(ts_len)
  }
  data_time_trend <- NULL
  if (time_trend) {
    data_time_trend <- create_time_trend(
      ts_len,
      intervention_start,
      min_timepoints_trend
    )
  }

  data_baseline <- create_baseline(
    ts_len,
    intervention_start,
    min_timepoints_baseline
  )

  dplyr::bind_cols(
    data_season,
    data_time_trend,
    data_baseline
  )
}

#' Create a data.frame with variable time trend for regression modeling.
#'
#' This function generates a data.frame that contains a time trend, which can be used in modeling to account for changes over time. Based on intervention_start there can be two time trend variables. The first one stays constant from the intervention_start onwards.
#'
#' @param ts_len integer, specifying the length of the time series.
#' @param intervention_start integer, default NULL, specifying the row number in the time series corresponding to an intervention date. If NULL no intervention is modeled.
#' @param min_timepoints_trend integer, default 12, specifying the minimum number of time points required after the intervention to fit a new time trend.
#' @return A data frame with columns representing the time trend before and after the intervention (if applicable).
#' @examples
#' \dontrun{
#' create_time_trend(100)
#' create_time_trend(100, intervention_start = 50)
#' }
create_time_trend <- function(ts_len,
                              intervention_start = NULL,
                              min_timepoints_trend = 12) {
  modelData <- data.frame(wtime = 0:(ts_len - 1))

  if (!is.null(intervention_start) && intervention_start + min_timepoints_trend < ts_len) {
    modelData <- data.frame(
      wtime1 = c(
        0:(intervention_start - 1),
        rep(
          intervention_start,
          ts_len - intervention_start
        )
      ),
      wtime2 = c(
        rep(0, intervention_start),
        ((intervention_start + 1):(ts_len)) - intervention_start
      )
    )
  }
  modelData
}

#' Create a data.frame with a constant baseline for an intercept only regression model.
#'
#' This function generates a baseline data.frame for use in modeling, which can account for different baselines before and after an intervention. If no intervention is done (intervention_start = NULL) NULL is returned as the regression can just use ~1 in the formula.
#'
#' @param ts_len integer, specifying the length of the time series.
#' @param intervention_start integer, default NULL, specifying the row number in the time series corresponding to an intervention date. When NULL no intervention is modeled.
#' @param min_timepoints_baseline integer, default 12, specifying the minimum number of time points required after the intervention to fit a new baseline.
#' @return NULL when intervention_start = NULL, otherwise a data frame with a column representing the baseline before and after the intervention (if applicable).
#' @return A data frame with columns representing the time trend before and after the intervention (if applicable).
#' \dontrun{
#' create_baseline(100)
#' create_baseline(100,intervention_start = 50)
#' }
create_baseline <- function(ts_len,
                            intervention_start = NULL,
                            min_timepoints_baseline = 12) {
  modelData <- NULL
  if (!is.null(intervention_start) && intervention_start + min_timepoints_baseline < ts_len) {
    modelData <- data.frame(baseline = c(
      rep(0, intervention_start),
      rep(1, length(((intervention_start + 1):(ts_len))))
    ))
  }
  modelData
}

#' Create a model formula based on the columns in the model_data dataframe.
#'
#' It uses all columns present in the model_data data.frame and models the number of cases by an additive model of all covariates.
#' @param model_data a data.frame containing all variables and their values over the timeseries for the model
#' @return string specifying the formula for the regression model
#' @examples
#' \dontrun{
#' model_data_1 <- create_model_data(100, model = "sincos")
#' model_data_2 <- create_model_data(100, model = "sincos", intervention_start = 50)
#' create_formula(model_data_1)
#' create_formula(model_data_2)
#' }
create_formula <- function(model_data) {
  column_names <- colnames(model_data)

  # Create the formula string
  covariates <- paste(column_names, collapse = " + ")

  if (covariates == "") {
    # simplest mean model
    paste("cases ~ 1")
  } else {
    paste("cases ~", covariates)
  }
}


#' Get signals based on a weigthed GLM quasipoisson regression model for the expected case counts
#' The GLM is flexible being able to just fit a mean, add a time trend, fit a harmonic sin/cos model or the seasons from the farringtonflexible.
#' @param data_aggregated data.frame, aggregated data with case counts.
#' @param number_of_weeks integer, specifying number of weeks to generate signals for.
#' @param model character, default "mean" one of c("mean", "sincos", "FN") specifying which kind of model the glm is fitting. "mean" fits an intercept model, "sincos" a harmonic sincos model, "FN" uses the seasgroups from farrington to fit parameters for seasonality.
#' @param time_trend boolean, default TRUE, when TRUE a timetrend is fitted in the glm describing the expected number of cases.
#' @param return_full_model boolean, default TRUE, specifying whether the fitted values of the model obtained from fitting the model to the first week of number_of_weeks should be returned and attached to data_aggregated as well.
#' @param alpha_upper decimal between 0 and 1, default 0.05 specifying the pvalue cutoff used for computing the threshold, when set to 0.05 the 95 percent quantile is used.
#' @param intervention_start_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention in the pandemic correction models. Default is NULL which indicates that no intervention is done, i.e. no additional intercept and possibly new time trend is fitted. When a date is given a new intercept and possibly time_trend (if time_trend == TRUE) is fitted.
#' @param min_timepoints_baseline integer, default 12, this parameter is only used when intervention_start_date is not NULL, specifying the number of weeks at least needed for fitting a new baseline after the intervention.
#' @param min_timepoints_trend integer, default 12, this parameter is only used when intervention_start_date is not NULL, specifying the number of weeks at least needed for fitting a new timetrend after the intervention.
#' @return data.frame aggregated data with case counts and additional columns alarms, upperbound and expected obtained from the signal detection algorithm. If return_full_model == TRUE then expected_pad is also added as a column to data_aggregated.
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' results <- get_signals_glm(data_aggregated)
#' }
get_signals_glm <- function(data_aggregated,
                            number_of_weeks = 6,
                            model = "mean",
                            time_trend = TRUE,
                            return_full_model = TRUE,
                            alpha_upper = 0.05,
                            intervention_start_date = NULL,
                            min_timepoints_baseline = 12,
                            min_timepoints_trend = 12) {
  checkmate::assert(
    checkmate::check_choice(model, choices = c("mean", "sincos", "FN"))
  )

  ts_len <- nrow(data_aggregated)
  if (!is.null(intervention_start_date)) {
    intervention_start <- get_intervention_timepoint(intervention_start_date, data_aggregated)
  } else {
    intervention_start <- NULL
  }

  rev_number_weeks <- rev(seq(0, number_of_weeks - 1, 1))
  bound_results <- data.frame(
    cases = integer(),
    expectation = numeric(),
    upper = numeric()
  )

  for (k in rev_number_weeks) {
    # we start with "first" week for signal detection, i.e. the one the most far away from the recent week
    # this week is taken as well to show the full model into the past
    ts_len_curr <- ts_len - k
    model_data <- create_model_data(ts_len_curr,
      model = model,
      time_trend = time_trend,
      intervention_start = intervention_start,
      min_timepoints_baseline = min_timepoints_baseline,
      min_timepoints_trend = min_timepoints_trend
    )

    formula <- as.formula(create_formula(model_data))

    # make sure the data is in the correct order to apply tail
    data_aggregated <- data_aggregated %>%
      dplyr::arrange(year, week)
    # take those cases based on the time points which should be considered
    cases <- data_aggregated %>%
      head(ts_len_curr) %>%
      dplyr::select(cases)


    if (nrow(model_data) == 0) {
      model_data <- cases
    } else {
      model_data <- dplyr::bind_cols(cases, model_data)
    }

    # seperate the data into fitting and prediction by taking the last timepoint as prediction
    # the remaining timepoints are used for fitting
    # pred data is last
    fit_data <- model_data %>% head(ts_len_curr - 1)
    pred_data <- model_data %>% tail(1)

    # fit a glm based on formula and data provided
    fit_glm <- glm(formula,
      data = fit_data,
      family = quasipoisson()
    )

    phi <- max(summary(fit_glm)$dispersion, 1)

    s <- surveillance:::anscombe.residuals(fit_glm, phi)
    omega <- surveillance:::algo.farrington.assign.weights(s)
    # fit a weighted glm using weighted anscombe residulas
    fit_glm <- glm(formula,
      data = fit_data,
      family = quasipoisson(), weights = omega
    )
    phi <- max(summary(fit_glm)$dispersion, 1)
    # predict with the fitted glm
    pred <- predict.glm(fit_glm,
      newdata = pred_data,
      se.fit = TRUE
    )

    eta0 <- pred$fit
    mean <- exp(eta0)

    bounds <- NA
    # mu_bounds is the expected value
    if (phi > 1) {
      bounds <- data.frame(
        cases = pred_data$cases,
        expectation = mean,
        upper = qnbinom(1 - alpha_upper, mean / (phi - 1), 1 / phi)
      )
    } else {
      bounds <- data.frame(
        cases = pred_data$cases,
        expectation = mean,
        upper = qpois(1 - alpha_upper, mean)
      )
    }

    bound_results <- rbind(bound_results, bounds)
    if (k == max(rev_number_weeks) && return_full_model) {
      full_model_expectation <- fit_glm$fitted.values
    }
  }

  # generate alarms
  # here in the end give the whole dataframe back as we also do with the other algorithms
  bound_results <- bound_results %>%
    dplyr::mutate(alarms = cases > upper)


  pad <- rep(NA, nrow(data_aggregated) - number_of_weeks)
  alarms <- c(pad, bound_results$alarms)
  upperbound <- c(pad, bound_results$upper)
  expected <- c(pad, bound_results$expectation)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  data_aggregated$expected <- expected

  if (return_full_model) {
    pad_number_of_weeks <- rep(NA, number_of_weeks)
    data_aggregated$expected_pad <- c(full_model_expectation, pad_number_of_weeks)
  }

  data_aggregated
}

#' Get a default and minimum and maximum date for the intervention time point for the glm algorithms with pandemic correction. This is based on the data provided and the settings for the delays.
#' @param data data.frame, preprocessed linelist of surveillance data obtained using preprocess_data()
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate signals for
#' @param time_trend boolean, default TRUE, when TRUE a timetrend is fitted in the glm describing the expected number of cases
#' @param min_timepoints_baseline integer, default 12, specifying the number of weeks at least needed for fitting a new baseline after the intervention
#' @param min_timepoints_trend integer, default 12, specifying the number of weeks at least needed for fitting a new timetrend after the intervention
#' @return list with three dates or NULL values. valid_start_date is the first date which is valid to chose as intervention_start_date, valid_end_date is the last date which is valid to chose to chose as intervention_start_date, default_intervention is a default date which is used for the intervention_start_date and usually set to "2020-03-15" but checked whether this is possible with the data we have
#' @examples \dontrun{
#' input_prepro <- input_example %>% preprocess_data()
#' get_valid_dates_intervention_start(input_prepro) # this just gives the default date "2020-03-15" back
#' get_valid_dates_intervention_start(input_prepro %>% dplyr::filter(date_report >= "2020-04-01")) # this gives the valid_start date back as default date
#' get_valid_dates_intervention_start(input_prepro %>% dplyr::filter(date_report >= "2020-04-01") %>% dplyr::filter(date_report <= "2020-05-01")) # this gives NULL as the timeperiod of date provided is too short to do a intervention
#' }
get_valid_dates_intervention_start <- function(data,
                                               date_var = "date_report",
                                               number_of_weeks = 6,
                                               time_trend = TRUE,
                                               min_timepoints_baseline = 12,
                                               min_timepoints_trend = 12) {
  if (time_trend) {
    delay <- max(min_timepoints_baseline, min_timepoints_trend)
  } else {
    delay <- min_timepoints_baseline
  }

  min_date <- min(data[[date_var]], na.rm = TRUE)
  max_date <- max(data[[date_var]], na.rm = TRUE)

  # start after the delay to still have enough time points to fit the non intervention model
  # reality would be that the intervention is rather later in the timeseries but let's not be strict and the user decide
  min_date_plus_delay <- min_date + lubridate::weeks(delay)
  max_date_minus_delay <- max_date - lubridate::weeks(number_of_weeks + delay)

  if (min_date_plus_delay > max_date_minus_delay) {
    min_date_plus_delay <- NULL
    max_date_minus_delay <- NULL
  }

  # get a good default date for the intervention
  # if 15-03-2020 is in the valid range we take it as in most european countries measures started in middle of March so this is a good value for them to start with
  default_intervention <- as.Date("2020-03-15")
  if (is.null(min_date_plus_delay)) {
    default_intervention <- NULL
  } else if (!(default_intervention %within% lubridate::interval(min_date_plus_delay, max_date_minus_delay))) {
    default_intervention <- min_date_plus_delay
  }

  return(list(valid_start_date = min_date_plus_delay, valid_end_date = max_date_minus_delay, default_intervention = default_intervention))
}
