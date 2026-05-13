input_prepro <- input_example %>%  preprocess_data()

testthat::test_that("get_signals returns expected structure", {

  res <- get_signals(input_prepro)
  res_stratified <- get_signals(input_prepro, stratification = "age_group")

  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 0)

  expected_minimal_cols <- c(
    "year",
    "week",
    "cases",
    "alarms",
    "upperbound",
    "expected",
    "category",
    "stratum"
  )

  expect_true(all(expected_minimal_cols %in% names(res)))

  expect_true(is.data.frame(res_stratified))
  expect_true(nrow(res_stratified) >= 0)

  expected_minimal_cols <- c(
    "year",
    "week",
    "cases",
    "alarms",
    "upperbound",
    "expected",
    "category",
    "stratum"
  )

  expect_true(all(expected_minimal_cols %in% names(res_stratified)))

})

testthat::test_that("get_signals handles date_start before start date of the linelist", {

  start_date <- as.Date("2016-06-01")
  start_date_week <- lubridate::isoweek(start_date)
  start_date_year <- lubridate::isoyear(start_date)
  start_iso_week_year <- interaction(start_date_week,start_date_year)

  res <- get_signals(
    input_prepro,
    date_start = start_date
  ) %>%
    dplyr::mutate(week_year = interaction(week,year))

  res_stratified <- get_signals(
    input_prepro,
    date_start = start_date,
    stratification = "age_group"
  ) %>%
    dplyr::mutate(week_year = interaction(week,year))

  expect_true(start_iso_week_year %in% res$week_year)
  expect_true(start_iso_week_year %in% res_stratified$week_year)
  # test one individual age group with little cases
  # age_group 85-89 does not have cases in the date range of the full linelist thus good example
  expect_true(start_iso_week_year %in% (res_stratified %>%
                dplyr::filter(stratum == "85-89") %>%
                dplyr::pull(week_year)))
})

testthat::test_that("get_signals handles date_end after end date of the linelist", {

  end_date <- as.Date("2023-12-31")
  end_date_week <- lubridate::isoweek(end_date)
  end_date_year <- lubridate::isoyear(end_date)
  end_iso_week_year <- interaction(end_date_week,end_date_year)

  res <- get_signals(
    input_prepro,
    date_end = end_date
  ) %>%
    dplyr::mutate(week_year = interaction(week,year))

  res_stratified <- get_signals(
    input_prepro,
    date_end = end_date,
    stratification = "age_group"
  ) %>%
    dplyr::mutate(week_year = interaction(week,year))

  expect_true(end_iso_week_year %in% res$week_year)
  expect_true(end_iso_week_year %in% res_stratified$week_year)
  # test one individual age group with little cases
  expect_true(end_iso_week_year %in% (res_stratified %>%
                                          dplyr::filter(stratum == "85-89") %>%
                                          dplyr::pull(week_year)))
})

testthat::test_that("get_signals defaults to full date range when no bounds given", {

  date_min <- min(input_prepro$date_report)
  date_max <- max(input_prepro$date_report)

  date_min_week <- lubridate::isoweek(date_min)
  date_min_year <- lubridate::isoyear(date_min)
  min_iso_week_year <- interaction(date_min_week,date_min_year)

  date_max_week <- lubridate::isoweek(date_max)
  date_max_year <- lubridate::isoyear(date_max)
  max_iso_week_year <- interaction(date_max_week,date_max_year)

  res <- get_signals(
    input_prepro
  ) %>%
    dplyr::mutate(week_year = interaction(week,year))

  res_stratified <- get_signals(
    input_prepro,
    stratification = "age_group"
  )  %>%
    dplyr::mutate(week_year = interaction(week,year))

  expect_true(min_iso_week_year %in% res$week_year)
  expect_true(max_iso_week_year %in% res$week_year)
  expect_true(min_iso_week_year %in% res_stratified$week_year)
  expect_true(max_iso_week_year %in% res_stratified$week_year)
  # test one individual age group with little cases
  expect_true(min_iso_week_year %in% (res_stratified %>%
                                        dplyr::filter(stratum == "85-89") %>%
                                        dplyr::pull(week_year)))
  expect_true(max_iso_week_year %in% (res_stratified %>%
                                        dplyr::filter(stratum == "85-89") %>%
                                        dplyr::pull(week_year)))
})
