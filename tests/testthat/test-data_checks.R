test_that("age group format check works as expected", {
  expect_true(is_age_group_format(c("<30", "30-35", "40-45", "45+")))
  expect_true(is_age_group_format(c("<30", "30-35", "40-45", ">45")))
  expect_false(is_age_group_format(c("below_one", "one_to_five")))
  expect_false(is_age_group_format(c("<30", "30-35", "40/45", "45+")))
  expect_false(is_age_group_format(c("<30", "30-35", "40+45", "45+")))
})

test_that("age group format check works as expected for the last age group", {
  expect_true(is_last_age_group_format(c("<30", "30_35", "40_45", "45+")))
  expect_true(is_last_age_group_format(c("<30", "30-35", "40-45", "45-50")))
  expect_true(is_last_age_group_format(c("<30", "30-35", "40-45", ">45")))
  expect_false(is_last_age_group_format(c("<30", "30-35", "40-45", "45-")))
  expect_false(is_last_age_group_format(c("<30", "30-35", "40-45", "45")))
})

test_that("date check works as expected", {
  input_example2 <- input_example %>%
    dplyr::mutate(date_report = as.Date(date_report))
  input_example3 <- input_example %>%
    dplyr::mutate(date_new = "2020/03/09")
  input_example4 <- input_example %>%
    dplyr::mutate(date_new = as.Date("2020/03/09"))
  input_example5 <- input_example %>%
    dplyr::mutate(date_new = as.Date("09-03-2020", format ="%d-%m-%Y"))



  expect_equal(length(check_type_and_value_date(input_example,"date_report")), 0)
  expect_equal(length(check_type_and_value_date(input_example2,"date_report")), 0)
  expect_equal(check_type_and_value_date(input_example3,"date_new")[[1]], "date_new is not in ISO 8601 format YYYY-MM-DD")
  expect_equal(length(check_type_and_value_date(input_example4,"date_new")), 0)
  expect_equal(length(check_type_and_value_date(input_example5,"date_new")), 0)
})
