test_that("cusum with no cases in training period is skipped", {
  dat <- input_example %>%
    filter_by_date(date_start = as.Date("2023-01-09")) %>% 
    dplyr::filter(age_group %in% c("00-04", "15-19")) %>% 
    preprocess_data()
   
  dat_agg <- dat %>% 
    aggregate_data(group = "age_group")

  # this groups has cases
  dat_0_4 <- dat_agg %>% dplyr::filter(age_group == "00-04")
  expect_no_error(get_signals_cusum(dat_0_4, number_of_weeks = 21))
  
  # this groups has no cases between 2023-W2 to 2023-W8
  dat_15_19 <- dat_agg %>% dplyr::filter(age_group == "15-19")
  expect_null(get_signals_cusum(dat_15_19, number_of_weeks = 21))

  # warning from signal wrapper
  expect_warning(
    get_signals_all(dat, "cusum", stratification = "age_group", number_of_weeks = 21),
    "The stratum age_group:15-19"
  )
})
