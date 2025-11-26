# check that if the dataset is too small no methods are possible
test_that("6 days of historic data no method possible", {
  min_date <- as.Date("2020-01-01")
  number_of_weeks <- 1
  max_date <- as.Date("2020-01-07")

  expect_identical(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks), NULL)
})

test_that("1 week of historic data CUSUM possible, number_of_weeks = 2, no methods possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- as.Date("2020-01-08")
  number_of_weeks <- 2
  # the week we do signal detection for actually only consists of 1 day of data but this yields a week in the aggregation

  expect_identical(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), NULL)
})

test_that("1 week of historic data, number_of_weeks = 1, CUSUM possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- as.Date("2020-01-08")
  number_of_weeks <- 1
  # the week we do signal detection for actually only consists of 1 day of data but this yields a week in the aggregation

  expect_identical(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), "CUSUM")
})

test_that("2 weeks of historic data, number_of_weeks = 1  only CUSUM possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- min_date + lubridate::weeks(2)
  number_of_weeks <- 1
  # the week we do signal detection for actually only consists of 1 day of data but this yields a week in the aggregation

  expect_identical(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), "CUSUM")
})


test_that("7 weeks of historic data, number_of_weeks = 1, EARS and CUSUM possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- min_date + lubridate::weeks(7)
  number_of_weeks <- 1

  expect_setequal(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), c("EARS", "CUSUM"))
})

test_that("2 years of historic data all except glm farrington and glm farrington with timetrend possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- min_date + lubridate::years(2)
  number_of_weeks <- 1

  expect_setequal(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), c("FarringtonFlexible", "EARS", "CUSUM", "Mean", "Timetrend", "Harmonic"))
})

test_that("3 years of historic data all except glm farrington and glm farrington with timetrend possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- min_date + lubridate::years(3)
  number_of_weeks <- 1

  expect_setequal(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), c("FarringtonFlexible", "EARS", "CUSUM", "Mean", "Timetrend", "Harmonic", "Harmonic with timetrend"))
})

test_that("4 years of historic data all methods possible", {
  min_date <- as.Date("2020-01-01")
  max_date <- min_date + lubridate::years(4)
  number_of_weeks <- 1

  expect_setequal(names(get_possible_methods(min_date, max_date, number_of_weeks = number_of_weeks)), names(available_algorithms()))
})
