test_that("test basic aggregation works", {
  # Set a starting ISO week and year
  start_date <- ISOweek::ISOweek2date("2023-W10-1") # Start from ISO week 10 of 2023

  # Generate sample data
  set.seed(42) # For reproducibility
  data <- data.frame(
    date_report = seq(start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )

  data_agg <- data %>%
    preprocess_data() %>%
    aggregate_data()

  solution <- data.frame(
    year = rep(2023, 3),
    week = c(10, 11, 12),
    cases = c(7, 7, 4)
  )
  expect_equal(data_agg, solution)
})

test_that("test basic aggregation works with ISO week 53", {
  # Set a starting ISO week and year with week 53
  start_date <- ISOweek::ISOweek2date("2020-W52-1") # Start from ISO week 52 of 2020

  # Generate sample data over ISO weeks 52, 53, and 1 (transition to 2021)
  set.seed(42) # For reproducibility
  data <- data.frame(
    date_report = seq(start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )
  # duplicate the data to have cases on the same date
  data <- rbind(data, data)

  # Assuming preprocess_data() and aggregate_data() are defined functions
  data_agg <- data %>%
    preprocess_data() %>%
    aggregate_data()

  # Define the expected aggregation output including week 53 and transition to 2021
  solution <- data.frame(
    year = c(2020, 2020, 2021),
    week = c(52, 53, 1),
    cases = c(14, 14, 8)
  )

  # Compare the aggregated result with the expected solution
  expect_equal(data_agg, solution)
})

test_that("test that filling missing weeks with 0 cases work", {
  data_agg <- data.frame(
    year = c(2020, 2020, 2021, 2021, 2021),
    week = c(50, 51, 1, 2, 5),
    cases = seq(1, 5, 1)
  )
  data_agg_complete <- add_missing_isoweeks(data_agg)
  solution <- data.frame(
    year = c(rep(2020, 4), rep(2021, 5)),
    week = c(50, 51, 52, 53, 1, 2, 3, 4, 5),
    cases = c(seq(1, 2, 1), 0, 0, seq(3, 4, 1), 0, 0, 5)
  )

  expect_equal(data_agg_complete, solution)
})

test_that("test that filling extending with weeks with 0 cases work", {
  data_agg <- data.frame(
    year = c(2020, 2020, 2021, 2021, 2021),
    week = c(50, 51, 1, 2, 5),
    cases = seq(1, 5, 1)
  )
  data_agg_complete <- add_missing_isoweeks(data_agg, date_start = "2020-12-01", date_end = "2021-02-16")
  solution <- data.frame(
    year = c(rep(2020, 5), rep(2021, 7)),
    week = c(49, 50, 51, 52, 53, 1, 2, 3, 4, 5, 6, 7),
    cases = c(0, seq(1, 2, 1), 0, 0, seq(3, 4, 1), 0, 0, 5, 0, 0)
  )

  expect_equal(data_agg_complete, solution)
})

test_that("test aggregation with filling missing zeros works", {
  start_date <- ISOweek::ISOweek2date("2020-W52-1") # Start from ISO week 52 of 2020

  # Generate sample data over ISO weeks 52, 53, and 1 (transition to 2021)
  set.seed(42) # For reproducibility
  data <- data.frame(
    date_report = seq(start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )
  # Define a new start date one month after the original start_date (April 6, 2023)
  new_start_date <- start_date + months(2)

  # Generate sample data starting from the new start date
  data_one_month_later <- data.frame(
    date_report = seq(new_start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(new_start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )

  data <- rbind(data, data_one_month_later)

  data_agg <- data %>%
    preprocess_data() %>%
    aggregate_data()

  solution <- data.frame(
    year = c(rep(2020, 2), rep(2021, 10)),
    week = c(52, 53, seq(1, 10, 1)),
    cases = c(7, 7, 4, 0, 0, 0, 0, 0, 1, 7, 7, 3)
  )
  expect_equal(data_agg, solution)
})


test_that("test aggregation with filling extending with zeros and adding missing zeros works", {
  start_date <- ISOweek::ISOweek2date("2020-W52-1") # Start from ISO week 52 of 2020

  # Generate sample data over ISO weeks 52, 53, and 1 (transition to 2021)
  set.seed(42) # For reproducibility
  data <- data.frame(
    date_report = seq(start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )
  # Define a new start date one month after the original start_date (April 6, 2023)
  new_start_date <- start_date + months(2)

  # Generate sample data starting from the new start date
  data_one_month_later <- data.frame(
    date_report = seq(new_start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(new_start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )

  data <- rbind(data, data_one_month_later)

  data_agg <- data %>%
    preprocess_data() %>%
    aggregate_data(date_start = "2020-12-17", date_end = "2021-11-02")

  solution <- data.frame(
    year = c(rep(2020, 3), rep(2021, 44)),
    week = c(51, 52, 53, seq(1, 44, 1)),
    cases = c(c(0, 7, 7, 4, 0, 0, 0, 0, 0, 1, 7, 7, 3), rep(0, 34))
  )
  expect_equal(data_agg, solution)
})

test_that("test that after aggregation week and years are in the correct order", {
  start_date <- ISOweek::ISOweek2date("2020-W52-1") # Start from ISO week 52 of 2020

  # Generate sample data over ISO weeks 52, 53, and 1 (transition to 2021)
  set.seed(42) # For reproducibility
  data <- data.frame(
    date_report = seq(start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )
  # Define a new start date one month after the original start_date (April 6, 2023)
  new_start_date <- start_date + months(2)

  # Generate sample data starting from the new start date
  data_one_month_later <- data.frame(
    date_report = seq(new_start_date, by = "days", length.out = 18),
    age_group = sample(c("0-10", "11-20", "21-30", "31-40", "41-50"), 18, replace = TRUE),
    date_onset = seq(new_start_date - lubridate::days(7), by = "days", length.out = 18),
    pathogen = rep("Salmonella", 18)
  )

  data <- rbind(data, data_one_month_later)
  # shuffle order of rows to create dates which are not in order
  data <- data[sample(nrow(data)),]

  data_agg <- data %>%
    preprocess_data() %>%
    aggregate_data(date_start = "2020-12-17", date_end = "2021-11-02")

  solution <- data.frame(
    year = c(rep(2020, 3), rep(2021, 44)),
    week = c(51, 52, 53, seq(1, 44, 1)),
    cases = c(c(0, 7, 7, 4, 0, 0, 0, 0, 0, 1, 7, 7, 3), rep(0, 34))
  )
  expect_equal(data_agg, solution)
})

