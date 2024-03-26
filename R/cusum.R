#' Get signals of CUSUM algorithm with reset
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate alarms for
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' results <- get_signals_cusum(data_aggregated)
#' }
get_signals_cusum <- function(data_aggregated,
                              number_of_weeks = 52) {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_weeks_for_calibration <- num_weeks_total - number_of_weeks

  if (num_weeks_for_calibration < 0) {
    warning(paste0(
      "The number of weeks you want to generate alarms for (n = ", number_of_weeks, ")",
      " is higher than the number of weeks you have in your data (n = ", num_weeks_total, ")."
    ))
    return(NULL)
  }

  # TODO discuss what the minimum num_weeks_for_calibration should be
  else if (num_weeks_for_calibration == 0) {
    warning(paste0(
      "Your data/stratification covers ",
      num_weeks_total,
      " number of weeks in total and you want to generate alarms for ", number_of_weeks, ". ",
      "Cusum uses 1 week for calibrating an epidemiological baseline. You have ", num_weeks_for_calibration,
      " weeks left in your data/stratification."
    ))
    return(NULL)
  }

  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    k = 1.04,
    h = 2.26,
    m = NULL
  )

  # run CUSUM on data
  results <- cusum_with_reset(sts_cases, control)

  pad <- rep(NA, num_weeks_total - number_of_weeks)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  # this algorithm does not return an expected value
  data_aggregated$expected <- NA

  # recode 0,1 in alarms to TRUE,FALSE
  data_aggregated <- data_aggregated %>%
    dplyr::mutate(alarms = dplyr::case_when(
      alarms == 1 ~ T,
      alarms == 0 ~ F,
      is.na(alarms) ~ NA
    ))

  return(data_aggregated)
}

#' Implementation of the CUSUM algorithm retrieved from the surveillance package and adapted so that after an alarm was triggered the cusum is set to 0
#' For parameter specification refer to the surveillance algo.cusum description
#' @export
algo.cusum_with_reset <- function(disProgObj, control = list(range = range, k = 1.04, h = 2.26, m = NULL, trans = "standard", alpha = NULL)) {
  # Set the default values if not yet set
  if (is.null(control$k)) {
    control$k <- 1.04
  }
  if (is.null(control$h)) {
    control$h <- 2.26
  }
  if (is.null(control$trans)) {
    control$trans <- "standard"
  }

  if (is.null(control$alpha)) {
    control$alpha <- 0.1
  }
  alpha <- control$alpha

  observed <- disProgObj$observed
  timePoint <- control$range[1]

  # Estimate m (the expected number of cases), i.e. parameter lambda of a
  # poisson distribution based on time points 1:t-1
  if (is.null(control$m)) {
    m <- mean(observed[1:(timePoint - 1)])
  } else if (is.numeric(control$m)) {
    m <- control$m
  } else if (control$m == "glm") {
    # Fit a glm to the first observations
    training <- 1:(timePoint - 1)
    # Set the time index
    t <- disProgObj$start[2] + training - 1
    # Set the observations
    x <- observed[training]
    # Set period
    p <- disProgObj$freq
    df <- data.frame(x = x, t = t)
    control$m.glm <- glm(x ~ 1 + cos(2 * pi / p * t) + sin(2 * pi / p * t), family = poisson(), data = df)

    # predict the values in range
    t.new <- disProgObj$start[2] + control$range - 1
    m <- predict(control$m.glm, newdata = data.frame(t = t.new), type = "response")
  }


  # No transformation
  # standObs <- observed[control$range]
  x <- observed[control$range]
  standObs <- switch(control$trans,
    # compute standardized variables z3 (proposed by Rossi)
    "rossi" = (x - 3 * m + 2 * sqrt(x * m)) / (2 * sqrt(m)),
    # compute standardized variables z1 (based on asympotic normality)
    "standard" = (x - m) / sqrt(m),
    # anscombe residuals
    "anscombe" = 3 / 2 * (x^(2 / 3) - m^(2 / 3)) / m^(1 / 6),

    # anscombe residuals as in pierce schafer based on 2nd order approx of E(X)
    "anscombe2nd" = (x^(2 / 3) - (m^(2 / 3) - m^(-1 / 3) / 9)) / (2 / 3 * m^(1 / 6)),

    # compute Pearson residuals for NegBin
    "pearsonNegBin" = (x - m) / sqrt(m + alpha * m^2),
    # anscombe residuals for NegBin
    "anscombeNegBin" = anscombeNB(x, mu = m, alpha = alpha),
    # don't do anything
    "none" = x,
    stop("invalid 'trans'formation")
  )

  # initialize the necessary vectors
  # start with cusum[timePoint -1] = 0, i.e. set cusum[1] = 0
  cusum <- matrix(0, nrow = (length(control$range) + 1), ncol = 1)
  alarm <- matrix(data = 0, nrow = (length(control$range) + 1), ncol = 1)

  for (t in 1:length(control$range)) {
    # compute cumulated sums of standardized observations corrected with the
    # reference value k for all time points in range
    cusum[t + 1] <- max(0, cusum[t] + (standObs[t] - control$k))

    # give alarm if the cusum is larger than the decision boundary h
    alarm[t + 1] <- cusum[t + 1] >= control$h

    # ADAPTATION resetting to 0 after alarm
    if (alarm[t + 1] == 1) {
      cusum[t + 1] <- 0
    }
  }

  # Backtransform
  h <- control$h
  k <- control$k
  Ctm1 <- cusum[1:length(control$range)]

  upperbound <- switch(control$trans,
    # standardized variables z3 (proposed by Rossi)
    "rossi" = 2 * h * m^(1 / 2) + 2 * k * m^(1 / 2) - 2 * Ctm1 * m^(1 / 2) + 5 * m - 2 * (4 * m^2 + 2 * m^(3 / 2) * h + 2 * m^(3 / 2) * k - 2 * m^(3 / 2) * Ctm1)^(1 / 2),
    # standardized variables z1 (based on asympotic normality)
    "standard" = ceiling(sqrt(m) * (h + k - Ctm1) + m),
    # anscombe residuals
    "anscombe" = ifelse(((2 / 3) * m^(1 / 6) * (h + k - Ctm1) + m^(2 / 3)) < 0,
      0,
      (2 / 3 * m^(1 / 6) * (h + k - Ctm1) + m^(2 / 3))^(3 / 2)
    ),

    # anscombe residuals ?
    "anscombe2nd" = ifelse(((2 / 3) * m^(1 / 6) * (h + k - Ctm1) + (m^(2 / 3) - m^(1 / 3) / 9)) < 0,
      0,
      (2 / 3 * m^(1 / 6) * (h + k - Ctm1) + (m^(2 / 3) - m^(1 / 3) / 9))^(3 / 2)
    ),

    # Pearson residuals for NegBin
    "pearsonNegBin" = sqrt(m + alpha * m^2) * (h + k - Ctm1) + m,
    # anscombe residuals for NegBin   ?
    "anscombeNegBin" = h - cusum[-1],
    # don't do anything
    "none" = h - cusum[-1]
  )
  # ensure upper bound is positive and not NaN
  upperbound[is.na(upperbound)] <- 0
  upperbound[upperbound < 0] <- 0

  # discard cusum[1] and alarm[1]
  cusum <- cusum[-1]
  alarm <- alarm[-1]

  # Add name and data name to control object.
  control$name <- paste("cusum:", control$trans)
  control$data <- paste(deparse(substitute(disProgObj)))
  control$m <- m

  # return alarm and upperbound vectors
  result <- list(alarm = alarm, upperbound = upperbound, disProgObj = disProgObj, control = control, cusum = cusum)

  class(result) <- "survRes" # for surveillance system result
  return(result)
}

#' Wrapper around the algo.cusum_with_reset
#' Copied from the code of the surveillance package and adapted for algo.cusum_with_reset
#' @export
cusum_with_reset <- function(sts, control = list(range = range, k = 1.04, h = 2.26, m = NULL, trans = "standard", alpha = NULL), ...) {
  surveillance::wrap.algo(sts, algo = "algo.cusum_with_reset", control = control, ...)
}
