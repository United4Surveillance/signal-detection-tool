create_fn_data <- function(ts_length, freq = 52) {
  # Create dummy sts to calculate Farrington input
  # why this *2??? is this because of pandemic adjustment? need to understand
  time_point_to_consider <- ts_length * 2
  survts <- surveillance::sts(rep(0, time_point_to_consider),
    start = c(2000, 1),
    frequency = freq
  )

  # Create data for Farrington GLM
  modelData <- surveillance:::algo.farrington.data.glm(
    dayToConsider = time_point_to_consider, b = floor((time_point_to_consider - w) / freq),
    freq = freq, epochAsDate = FALSE,
    epochStr = "none", vectorOfDates = 1:time_point_to_consider,
    w = w, noPeriods = 10, observed = survts@observed[, 1],
    population = rep(0, 1000), verbose = FALSE,
    pastWeeksNotIncluded = 0, k = time_point_to_consider
  )[, 1:4]

  subset_seasgroups <- (nrow(modelData) - ts_length + 1):nrow(modelData)
  modelData[subset_seasgroups, ] %>%
    dplyr::select(seasgroups)
}

create_sincos_data <- function(ts_len, freq = 52, S = 1) {
  # write check for S >= 1
  modelData <- data.frame(
    sin = sin(2 * pi * S * (1:ts_len) / freq),
    cos = cos(2 * pi * S * (1:ts_len) / freq)
  )
  modelData
}
create_model_data <- function(ts_len,
                              model,
                              timeTrend = TRUE,
                              intervention_start = NULL,
                              baseline_start_delay = 52,
                              trend_start_delay = 26) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(model, choices = c("mean", "sincos", "FN"))
  )

  data_season <- NULL
  if (model == "sincos") {
    data_season <- create_sincos_data(ts_len)
  } else if (model == "FN") {
    data_season <- create_fn_data(
      freq
    )
  }
  data_time_trend <- NULL
  if (timeTrend) {
    data_time_trend <- create_time_trend(
      ts_len,
      intervention_start,
      trend_start_delay
    )
  }

  data_baseline <- create_baseline(
    ts_len,
    intervention_start,
    baseline_start_delay
  )

  dplyr::bind_cols(
    data_season,
    data_time_trend,
    data_baseline
  )
}

# with the delay same principle as with the baseline, we want at least 12 timepoints to fit another time trend
create_time_trend <- function(ts_len,
                              intervention_start = NULL,
                              trend_start_delay = 12) {
  modelData <- data.frame(wtime = 0:(ts_len - 1))

  if (!is.null(intervention_start) && intervention_start + trend_start_delay < ts_len) {
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

# basically with the delay we assure that there are at least 12 timepoints we have to fit a different baseline. If there are less than 12 timepoints then we do not fit two baselines
create_baseline <- function(ts_len,
                            intervention_start = NULL,
                            baseline_start_delay = 12) {
  modelData <- NULL
  if (!is.null(intervention_start) && intervention_start + baseline_start_delay < ts_len) {
    modelData <- data.frame(baseline = c(
      rep(0, intervention_start),
      rep(1, length(((intervention_start + 1):(ts_len))))
    ))
  }
  modelData
}


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
# time_points_to_consider: all time points we use for fitting and prediction
# what do we do with those time points we do not use for fitting? How can we cut them off? Do we need to cut them off before model building?
# fit data needs to have the model plus the observed cases inside
# think about whether we really want to have all these parameters here...
get_signals_glm <- function(data_aggregated,
                          number_of_weeks = 6,
                          model = "mean",
                          timeTrend = TRUE,
                          return_full_model = TRUE,
                          alpha_upper = 0.05,
                          intervention_start_date = NULL,
                          baseline_start_delay = 26,
                          trend_start_delay = 26) {
  checkmate::assert(
    checkmate::check_choice(model, choices = c("mean", "sincos", "FN"))
  )

  ts_len <- nrow(data_aggregated)
  if(!is.null(intervention_start_date)){
    intervention_start <- get_intervention_timepoint(intervention_start_date,data_aggregated)
  }else{
    intervention_start <- NULL
  }

  rev_number_weeks <- rev(seq(0,number_of_weeks-1,1))
  bound_results <- data.frame(cases = integer(),
                               expectation = numeric(),
                               upper = numeric())

  for (k in rev_number_weeks){
    # we start with "first" week for signal detection, i.e. the one the most far away from the recent week
    # this week is taken as well to show the full model into the past
    ts_len_curr <- ts_len - k
    model_data <- create_model_data(ts_len_curr,
      model = model,
      timeTrend = timeTrend,
      intervention_start = intervention_start,
      baseline_start_delay = baseline_start_delay,
      trend_start_delay = trend_start_delay
    )

    formula <- as.formula(create_formula(model_data))
    # add the cases from the aggregated data to the model data under a column cases
    # check how for other models it is dealt with the remaining time points which are not considered for model fitting

    # make sure the data is in the correct order to apply tail
    data_aggregated <- data_aggregated %>%
      dplyr::arrange(year, week)
    # take those cases based on the time points which should be considered
    cases <- data_aggregated %>%
      head(ts_len_curr) %>%
      dplyr::select(cases)


    if(nrow(model_data) == 0){
      model_data <- cases
    }else{
      model_data <- dplyr::bind_cols(cases, model_data)
    }

    # seperate the data into fitting and prediction by taking the last timepoint as prediction
    # the remaining timepoints are used for fitting
    # fit data until vorletzter
    # pred data is last
    fit_data <- model_data %>% head(ts_len_curr - 1)
    pred_data <- model_data %>% tail(1)

    # fit a glm based on formula and data provided
    fit_glm <- glm(formula,
                   data = fit_data,
                   family = quasipoisson()
    )
    # do we always want to fit a quasipoisson model? what does this exactly mean
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
    # @Bene: why not like in the code of farringtonflexible have dispersion = phi in this equation? because it looks like predict.glm is anyways automatically extracting it
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

    bound_results <- rbind(bound_results,bounds)
    if(k == max(rev_number_weeks) && return_full_model){
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

  if(return_full_model){
    pad_number_of_weeks <- rep(NA, number_of_weeks)
    data_aggregated$expected_pad <- c(full_model_expectation,pad_number_of_weeks)
  }

  data_aggregated

}

# get minimum and maximum dates which can be used for setting an intervention date
get_valid_dates_intervention_start <- function(){

}
