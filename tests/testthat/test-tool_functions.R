if (!require(testthat)) install.packages("testthat")
library(testthat)

source("../../R/age_group.R") # can be deleted once project is a package


test_that("Categorization of age", {
  expect_identical(find_age_group(5, c(0, 5, 10, 99)), "05-09")
  expect_identical(find_age_group(12, c(0, 5, 15, 99)), "05-14")
  expect_identical(find_age_group(99, c(0, 5, 15, 99)), "99+")
  expect_identical(find_age_group(56, c(0, 5, 15, 99)), "15-98")
})

test_that("Age group column is added correctly", {
  input_data_correct <- read.csv(
    test_path(
      "testdata",
      "agegroup_input_noerror.csv"
    ),
    header = TRUE, sep = ","
  )
  input_data_incorrect <- read.csv(
    test_path(
      "testdata",
      "agegroup_input_error.csv"
    ),
    header = TRUE, sep = ","
  )
  output_data <- read.csv(test_path("testdata", "agegroup_output.csv"),
                          header = TRUE, sep = ","
  )

  expect_error(age_groups(input_data_incorrect)) # no age records in example file
  expect_no_error(age_groups(input_data_correct))
  expect_true(all.equal.character(age_groups(input_data_correct), output_data))
  expect_error(age_groups(input_data_correct, "string")) # invalid type
  expect_error(age_groups(input_data_correct, c(75, 4, 100, 23, 0))) # invalid order
})
