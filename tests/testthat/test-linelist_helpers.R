test_that("build_signal_and_comparison_linelist deduplicates cases and combines signal ids", {
  true_signals <- data.frame(
    year = rep(2024, 3),
    week = c(1, 2, 1),
    cases = c(13, 12, 5),
    category = c(NA_character_, "sex", "sex"),
    stratum = c(NA_character_, "male", "male"),
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = c(1, 2, 3),
    time_unit = c("weekly", "weekly", "weekly")
  )

  signals_padded <- data.frame(
    year = c(rep(2023, 52), 2024, 2024),
    week = c(seq(1, 52, 1), 1, 2),
    cases = c(rep(5, 52), 18, 12),
    alarms = c(rep(NA, 52), TRUE, TRUE),
    time_unit = c("weekly", "weekly", "weekly")
  )

  filtered_data <- filtered_data <- data.frame(
    case_id = seq_len(290),
    date_report = as.Date(c(
      rep("2023-01-03", 5),
      rep("2023-01-10", 5),
      rep("2023-01-17", 5),
      rep("2023-01-24", 5),
      rep("2023-01-31", 5),
      rep("2023-02-07", 5),
      rep("2023-02-14", 5),
      rep("2023-02-21", 5),
      rep("2023-02-28", 5),
      rep("2023-03-07", 5),
      rep("2023-03-14", 5),
      rep("2023-03-21", 5),
      rep("2023-03-28", 5),
      rep("2023-04-04", 5),
      rep("2023-04-11", 5),
      rep("2023-04-18", 5),
      rep("2023-04-25", 5),
      rep("2023-05-02", 5),
      rep("2023-05-09", 5),
      rep("2023-05-16", 5),
      rep("2023-05-23", 5),
      rep("2023-05-30", 5),
      rep("2023-06-06", 5),
      rep("2023-06-13", 5),
      rep("2023-06-20", 5),
      rep("2023-06-27", 5),
      rep("2023-07-04", 5),
      rep("2023-07-11", 5),
      rep("2023-07-18", 5),
      rep("2023-07-25", 5),
      rep("2023-08-01", 5),
      rep("2023-08-08", 5),
      rep("2023-08-15", 5),
      rep("2023-08-22", 5),
      rep("2023-08-29", 5),
      rep("2023-09-05", 5),
      rep("2023-09-12", 5),
      rep("2023-09-19", 5),
      rep("2023-09-26", 5),
      rep("2023-10-03", 5),
      rep("2023-10-10", 5),
      rep("2023-10-17", 5),
      rep("2023-10-24", 5),
      rep("2023-10-31", 5),
      rep("2023-11-07", 5),
      rep("2023-11-14", 5),
      rep("2023-11-21", 5),
      rep("2023-11-28", 5),
      rep("2023-12-05", 5),
      rep("2023-12-12", 5),
      rep("2023-12-19", 5),
      rep("2023-12-26", 5),
      rep("2024-01-02", 13), # week 1, female
      rep("2024-01-03", 5), # week 1, male
      rep("2024-01-09", 12) # week 2, male
    )),
    sex = c(
      rep("female", 260),
      rep("female", 13),
      rep("male", 5),
      rep("male", 12)
    )
  )

  result <- build_signal_and_comparison_linelist(
    selected_signal_ids = c(1, 3),
    true_signals = true_signals,
    signals_padded = signals_padded,
    filtered_data = filtered_data
  )

  expect_named(result, c("cases", "cases_comparison"))

  expect_equal(nrow(result$cases), 18)

  duplicated_case <- result$cases |>
    dplyr::filter(case_id == 274)

  expect_equal(nrow(duplicated_case), 1)
  expect_equal(duplicated_case$signal_id, "1,3")

  expect_false(any(duplicated(result$cases$case_id)))
})

test_that("build_signal_and_comparison_linelist excludes selected signal cases from comparison linelist", {
  true_signals <- data.frame(
    year = rep(2024, 3),
    week = c(1, 2, 1),
    cases = c(13, 12, 5),
    category = c(NA_character_, "sex", "sex"),
    stratum = c(NA_character_, "male", "male"),
    alarms = c(TRUE, TRUE, TRUE),
    signal_id = c(1, 2, 3),
    time_unit = c("weekly", "weekly", "weekly")
  )

  signals_padded <- data.frame(
    year = c(rep(2023, 52), 2024, 2024),
    week = c(seq(1, 52, 1), 1, 2),
    cases = c(rep(5, 52), 18, 12),
    alarms = c(rep(NA, 52), TRUE, TRUE),
    number_of_time_units = rep(3, 54),
    time_unit = c("weekly", "weekly", "weekly")
  )

  filtered_data <- data.frame(
    case_id = seq_len(290),
    date_report = as.Date(c(
      rep("2023-01-03", 5),
      rep("2023-01-10", 5),
      rep("2023-01-17", 5),
      rep("2023-01-24", 5),
      rep("2023-01-31", 5),
      rep("2023-02-07", 5),
      rep("2023-02-14", 5),
      rep("2023-02-21", 5),
      rep("2023-02-28", 5),
      rep("2023-03-07", 5),
      rep("2023-03-14", 5),
      rep("2023-03-21", 5),
      rep("2023-03-28", 5),
      rep("2023-04-04", 5),
      rep("2023-04-11", 5),
      rep("2023-04-18", 5),
      rep("2023-04-25", 5),
      rep("2023-05-02", 5),
      rep("2023-05-09", 5),
      rep("2023-05-16", 5),
      rep("2023-05-23", 5),
      rep("2023-05-30", 5),
      rep("2023-06-06", 5),
      rep("2023-06-13", 5),
      rep("2023-06-20", 5),
      rep("2023-06-27", 5),
      rep("2023-07-04", 5),
      rep("2023-07-11", 5),
      rep("2023-07-18", 5),
      rep("2023-07-25", 5),
      rep("2023-08-01", 5),
      rep("2023-08-08", 5),
      rep("2023-08-15", 5),
      rep("2023-08-22", 5),
      rep("2023-08-29", 5),
      rep("2023-09-05", 5),
      rep("2023-09-12", 5),
      rep("2023-09-19", 5),
      rep("2023-09-26", 5),
      rep("2023-10-03", 5),
      rep("2023-10-10", 5),
      rep("2023-10-17", 5),
      rep("2023-10-24", 5),
      rep("2023-10-31", 5),
      rep("2023-11-07", 5),
      rep("2023-11-14", 5),
      rep("2023-11-21", 5),
      rep("2023-11-28", 5),
      rep("2023-12-05", 5),
      rep("2023-12-12", 5),
      rep("2023-12-19", 5),
      rep("2023-12-26", 5),
      rep("2024-01-02", 13),
      rep("2024-01-03", 5),
      rep("2024-01-09", 12)
    )),
    sex = c(
      rep("female", 260),
      rep("female", 13),
      rep("male", 5),
      rep("male", 12)
    )
  )

  result <- build_signal_and_comparison_linelist(
    selected_signal_ids = c(1, 3),
    true_signals = true_signals,
    signals_padded = signals_padded,
    filtered_data = filtered_data
  )

  expect_equal(nrow(result$cases), 18)
  expect_false(any(result$cases_comparison$case_id %in% result$cases$case_id))

  expect_equal(nrow(result$cases_comparison), 12)
  expect_equal(
    sort(result$cases_comparison$case_id),
    279:290
  )
})
