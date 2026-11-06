testthat::test_that("validate_scorer_output() accepts valid scorer output", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE),
    x = c(10, 20, 30)
  )

  score_tbl <- tibble::tibble(
    .row_id = 1:3,
    score = c(0.1, 0.5, 1.0)
  )

  testthat::expect_invisible(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    )
  )
})

testthat::test_that("validate_scorer_output() rejects non-data-frame output", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- c(0.1, 0.2, 0.3)

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "must return a data.frame or tibble"
  )
})

testthat::test_that("validate_scorer_output() rejects wrong column names", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    row_id = 1:3,
    score = c(0.1, 0.2, 0.3)
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "must return exactly the columns `.row_id` and `score`"
  )
})

testthat::test_that("validate_scorer_output() rejects wrong number of rows", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    .row_id = 1:2,
    score = c(0.1, 0.2)
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "returned 2 rows instead of 3"
  )
})

testthat::test_that("validate_scorer_output() rejects duplicated .row_id values", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    .row_id = c(1, 1, 3),
    score = c(0.1, 0.2, 0.3)
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "contains duplicated `.row_id` values"
  )
})

testthat::test_that("validate_scorer_output() rejects mismatching .row_id values", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    .row_id = c(1, 2, 4),
    score = c(0.1, 0.2, 0.3)
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "does not contain the same `.row_id` values"
  )
})

testthat::test_that("validate_scorer_output() rejects non-numeric score", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    .row_id = 1:3,
    score = c("a", "b", "c")
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "`score` must be numeric"
  )
})


testthat::test_that("validate_scorer_output() rejects scores outside [0, 1]", {
  signal_results <- tibble::tibble(
    .row_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE)
  )

  score_tbl <- tibble::tibble(
    .row_id = 1:3,
    score = c(0.1, 1.2, 0.3)
  )

  testthat::expect_error(
    validate_scorer_output(
      score_tbl = score_tbl,
      signal_results = signal_results,
      scorer_name = "my_scorer"
    ),
    "`score` must be in the interval \\[0, 1\\]"
  )
})

testthat::test_that("aggregate_scores() computes row-wise mean", {
  score_df <- tibble::tibble(
    .row_id = 1:3,
    scorer_a = c(0.0, 0.5, 1.0),
    scorer_b = c(1.0, 0.5, 0.0)
  )

  result <- aggregate_scores(score_df, aggregation = "mean")

  testthat::expect_equal(result, c(0.5, 0.5, 0.5))
})

testthat::test_that("aggregate_scores() computes row-wise sum", {
  score_df <- tibble::tibble(
    .row_id = 1:3,
    scorer_a = c(0.0, 0.5, 1.0),
    scorer_b = c(1.0, 0.5, 0.0)
  )

  result <- aggregate_scores(score_df, aggregation = "sum")

  testthat::expect_equal(result, c(1.0, 1.0, 1.0))
})

testthat::test_that("aggregate_scores() errors if no score columns are present", {
  score_df <- tibble::tibble(
    .row_id = 1:3
  )

  testthat::expect_error(
    aggregate_scores(score_df, aggregation = "mean"),
    "No individual scores were found for aggregation"
  )
})


testthat::test_that("get_scores() returns original columns plus aggregated score", {
  signal_results <- tibble::tibble(
    signal_id = 1:3,
    alarms = c(TRUE, TRUE, TRUE),
    value = c(10, 20, 30)
  )

  scorer_low <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.1, 0.2, 0.3)
    )
  }

  scorer_high <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.9, 0.8, 0.7)
    )
  }

  result <- get_scores(
    signal_results = signal_results,
    scorers = list(low = scorer_low, high = scorer_high),
    aggregation = "mean"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_identical(names(result), c("signal_id", "alarms", "value", "score"))
  testthat::expect_equal(result$signal_id, signal_results$signal_id)
  testthat::expect_equal(result$value, signal_results$value)
  testthat::expect_equal(result$score, c(0.5, 0.5, 0.5))
})

testthat::test_that("get_scores() supports sum aggregation", {
  signal_results <- tibble::tibble(
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = 1:3
  )

  scorer_a <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.1, 0.2, 0.3)
    )
  }

  scorer_b <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.9, 0.8, 0.7)
    )
  }

  result <- get_scores(
    signal_results = signal_results,
    scorers = list(a = scorer_a, b = scorer_b),
    aggregation = "sum"
  )

  testthat::expect_equal(result$score, c(1.0, 1.0, 1.0))
})

testthat::test_that("get_scores() preserves input row order even if scorer output is shuffled", {
  signal_results <- tibble::tibble(
    signal_id = c("a", "b", "c"),
    alarms = c(TRUE, TRUE, TRUE),
    value = c(10, 20, 30)
  )

  scorer_ordered <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.0, 0.5, 1.0)
    )
  }

  scorer_shuffled <- function(signal_results) {
    tibble::tibble(
      .row_id = c(3, 1, 2),
      score = c(0.2, 0.4, 0.6)
    )
  }

  result <- get_scores(
    signal_results = signal_results,
    scorers = list(ordered = scorer_ordered, shuffled = scorer_shuffled),
    aggregation = "mean"
  )

  expected_score <- c(
    mean(c(0.0, 0.4)),
    mean(c(0.5, 0.6)),
    mean(c(1.0, 0.2))
  )

  testthat::expect_identical(result$signal_id, c("a", "b", "c"))
  testthat::expect_equal(result$score, expected_score)
})

testthat::test_that("get_scores() auto-names unnamed scorers", {
  signal_results <- tibble::tibble(
    alarms = c(TRUE, TRUE),
    signal_id = 1:2
  )

  scorer_a <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.2, 0.4)
    )
  }

  scorer_b <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.6, 0.8)
    )
  }

  result <- get_scores(
    signal_results = signal_results,
    scorers = list(scorer_a, scorer_b),
    aggregation = "mean"
  )

  testthat::expect_equal(result$score, c(0.4, 0.6))
})

testthat::test_that("get_scores() errors for empty scorer list", {
  signal_results <- tibble::tibble(
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = 1:3
  )

  testthat::expect_error(
    get_scores(
      signal_results = signal_results,
      scorers = list(),
      aggregation = "mean"
    ),
    "`scorers` must be a non-empty list of functions"
  )
})

testthat::test_that("get_scores() errors if scorers contains non-functions", {
  signal_results <- tibble::tibble(
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = 1:3
  )

  testthat::expect_error(
    get_scores(
      signal_results = signal_results,
      scorers = list(a = 123),
      aggregation = "mean"
    ),
    "All elements in `scorers` must be functions"
  )
})

testthat::test_that("get_scores() errors if a scorer returns invalid output", {
  signal_results <- tibble::tibble(
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = 1:3
  )

  bad_scorer <- function(signal_results) {
    tibble::tibble(
      .row_id = signal_results$.row_id,
      score = c(0.1, 0.2, 1.5)
    )
  }

  testthat::expect_error(
    get_scores(
      signal_results = signal_results,
      scorers = list(bad = bad_scorer),
      aggregation = "mean"
    ),
    "`score` must be in the interval \\[0, 1\\]"
  )
})


###### Tests for each score function

# helper function: artificial aggregated data between 2018 and 2022,
# 99% of cases are 0, otherwise between 1 and 3. For weeks 47 to 52 2022, 1 case
make_sparse_cases <- function(seed) {
  set.seed(seed)

  sparse_cases <- expand.grid(
    year = 2018:2022,
    week = 1:52,
    category = NA,
    time_unit = "weekly"
  ) %>%
    dplyr::arrange(year, week) %>%
    dplyr::mutate(
      cases = rbinom(260, 1, 0.01) * round(runif(260, 1, 3))
    ) %>%
    dplyr::mutate(
      cases = dplyr::case_when(
        year == 2022 & week > 46 ~ 1,
        .default = cases
      )
    )

  return(sparse_cases)
}

testthat::test_that("Score seasonal works with small case numbers", {
  # make sparse_cases
  cases_agg <- make_sparse_cases(123) # this dataset contains 0-cases for 2019, 2020, and 2021

  # signals with harmonic with time trend
  signals <- get_signals_glm(cases_agg, number_of_time_units = 52, model = "sincos") %>%
    dplyr::mutate(.row_id = dplyr::row_number(), method = "glm harmonic with timetrend")

  # no errors expected, warning is suppressed as glm does not converge
  testthat::expect_no_error(
    scores <- score_seasonal(signals)
  ) %>% suppressWarnings()

  results <- dplyr::full_join(signals, scores, by = ".row_id") %>%
    dplyr::filter(alarms == TRUE)

  # there is no NaNs in the scores
  testthat::expect_true(all(!is.nan(results$score)))

  # the scores are between 0 and 1
  testthat::expect_true(all(results$score >= 0 & results$score <= 1))
})

testthat::test_that("Score strength works with small case numbers", {
  # make sparse cases
  cases_agg <- make_sparse_cases(123)

  # signals with harmonic with time trend
  signals <- get_signals_glm(cases_agg, number_of_time_units = 52, model = "sincos") %>%
    dplyr::mutate(.row_id = dplyr::row_number(), method = "glm harmonic with timetrend")

  # no errors expected, warning is suppressed as glm does not converge
  testthat::expect_no_error(
    scores <- score_strength(signals)
  ) %>% suppressWarnings()

  results <- dplyr::full_join(signals, scores, by = ".row_id") %>%
    dplyr::filter(alarms == TRUE)

  # there is no NaNs in the scores
  testthat::expect_true(all(!is.nan(results$score)))

  # the scores are between 0 and 1
  testthat::expect_true(all(results$score >= 0 & results$score <= 1))
})

testthat::test_that("seasonal score ignores complete zero-case years", {
  # dummy dataset, contains all months of 2020 to 2022, cases are distributed
  # evenly for all months, so the score is 0.5. 2021 has no cases
  # alarms triggered for the complete 2022
  signals <- tibble::tibble(
    .row_id = 1:36,
    year = rep(2020:2022, each = 12),
    month = rep(1:12, times = 3),
    category = NA_character_,
    time_unit = "monthly",
    cases = c(rep(6, 12), rep(0, 12), rep(7, 12)),
    alarms = c(rep(NA, 24), rep(TRUE, 12))
  )

  scores <- score_seasonal(signals)

  # expected to have 0.5 as score for the last year
  expected <- c(rep(NA_real_, 24), rep(0.5, 12))

  testthat::expect_equal(scores$.row_id, signals$.row_id)
  testthat::expect_equal(scores$score, expected)
})
