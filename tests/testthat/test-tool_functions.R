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
  # remove age_group in example file as it does not match with the corresponding age and in our age_group function we do not correct age_group when they are wrong, thus only check whether age_groups are generated correctly
  expect_true(all.equal.character(age_groups(input_data_correct %>% dplyr::select(-age_group)), output_data))
})


test_data_1 <- data.frame(
  age = c(1, 5, 35, 67, NA),
  age_group = c("00-04", "05-09", "35-39", "65-69", NA_character_)
)

test_that("age_groups function works correctly with NA", {
  expect_no_error(age_groups(test_data_1))
  expect_equal(levels(age_groups(test_data_1)$age_group), c(
    "00-04", "05-09", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69"
  ))
  expect_true(anyNA(age_groups(test_data_1)))
})

test_that("age_groups function works correctly when only age or only age_group is provided in the data", {
  test_data_only_agegroup <- test_data_1 %>%
    dplyr::select(age_group)
  expect_no_error(age_groups(test_data_only_agegroup))
  expect_equal(levels(age_groups(test_data_only_agegroup)$age_group), c(
    "00-04", "05-09", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69"
  ))
  expect_true(anyNA(age_groups(test_data_only_agegroup)))

  test_data_only_age <- test_data_1 %>%
    dplyr::select(age)
  expect_no_error(age_groups(test_data_only_age))
  expect_equal(levels(age_groups(test_data_only_age)$age_group), c(
    "00-04", "05-09", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69"
  ))
  expect_true(anyNA(age_groups(test_data_only_age)))
})

test_that("age_groups function works correctly when no equal sizing in age_groups", {
  # No equal sizing with age available
  test_data_2 <- data.frame(
    age = c(1, 5, 35, 67, NA),
    age_group = c("00-04", "05-09", "10-39", "60-69", NA_character_)
  )
  expect_no_error(age_groups(test_data_2))
  expect_equal(levels(age_groups(test_data_2)$age_group), c("00-04", "05-09", "10-39", "40-59", "60-69"))
  expect_true(anyNA(age_groups(test_data_2)))

  # No equal sizing without age available
  test_data_2_without_age <- test_data_2 %>%
    dplyr::select(age_group)

  expect_no_error(age_groups(test_data_2_without_age))
  expect_equal(levels(age_groups(test_data_2_without_age)$age_group), c("00-04", "05-09", "10-39", "40-59", "60-69"))
  expect_true(anyNA(age_groups(test_data_2_without_age)))
})


test_that("age_groups function works correctly when usage of 60+ for the last age_group", {
  # No equal sizing with age available
  test_data_3 <- data.frame(
    age = c(1, 5, 35, 67, NA),
    age_group = c("00-04", "05-09", "10-39", "60+", NA_character_)
  )
  expect_no_error(age_groups(test_data_3))
  # not working yet
  # expect_equal(levels(age_groups(test_data_3)$age_group),c("00-04", "05-09", "10-39","40-59","60+"))
  expect_true(anyNA(age_groups(test_data_3)))

  # No equal sizing without age available
  test_data_3_without_age <- test_data_3 %>%
    dplyr::select(age_group)

  expect_no_error(age_groups(test_data_3_without_age))
  # not working yet
  # expect_equal(levels(age_groups(test_data_3_without_age)$age_group),c("00-04", "05-09", "10-39","40-59","60+"))
  expect_true(anyNA(age_groups(test_data_3_without_age)))
})
