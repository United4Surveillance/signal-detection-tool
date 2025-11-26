test_that("Age group column is added correctly", {
  input_data <- read.csv(
    test_path(
      "testdata",
      "agegroup_input_noerror.csv"
    ),
    header = TRUE, sep = ","
  ) %>%
    # strip trailing or leading whitespaces
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.x)))
  input_data <- remove_empty_columns(input_data)

  input_data_noage <- read.csv(
    test_path(
      "testdata",
      "agegroup_input_noage.csv"
    ),
    header = TRUE, sep = ","
  ) %>%
    # strip trailing or leading whitespaces
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.x)))
  input_data_noage <- remove_empty_columns(input_data_noage)

  output_data <- read.csv(test_path("testdata", "agegroup_output.csv"),
    header = TRUE, sep = ","
  ) %>%
    # strip trailing or leading whitespaces
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.x)))
  output_data <- remove_empty_columns(output_data)

  expect_no_error(age_groups(input_data_noage)) # no age records in example file
  expect_no_error(age_groups(input_data))
  # remove age_group in example file as it does not match with the corresponding age and in our age_group function we do not correct age_group when they are wrong, thus only check whether age_groups are generated correctly
  expect_true(all.equal.character(age_groups(input_data %>% dplyr::select(-age_group)), output_data))
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

test_that("age_groups function works correctly when usage of >60 for the last age_group", {
  # No equal sizing with age available
  test_data_4 <- data.frame(
    age = c(1, 5, 35, 67, NA),
    age_group = c("00-04", "05-09", "10-39", ">60", NA_character_)
  )
  expect_no_error(age_groups(test_data_4))
  # not working yet
  # expect_equal(levels(age_groups(test_data_3)$age_group),c("00-04", "05-09", "10-39","40-59","60+"))
  expect_true(anyNA(age_groups(test_data_4)))

  # No equal sizing without age available
  test_data_4_without_age <- test_data_4 %>%
    dplyr::select(age_group)

  expect_no_error(age_groups(test_data_4_without_age))
  # not working yet
  # expect_equal(levels(age_groups(test_data_3_without_age)$age_group),c("00-04", "05-09", "10-39","40-59","60+"))
  expect_true(anyNA(age_groups(test_data_4_without_age)))
})

test_that("age_groups respects custom break_at lower bounds", {
  df <- data.frame(
    age = c(0L, 5L, 20L, 40L, 70L, 100L, 120L, NA_integer_)
  )

  res <- age_groups(df, break_at = c(15L, 35L, 65L, 100L))

  expect_equal(
    as.character(res$age_group),
    c("00-14", "00-14", "15-34", "35-64", "65-99", "100+", "100+", NA)
  )

  expect_equal(
    levels(res$age_group),
    c("00-14", "15-34", "35-64", "65-99", "100+")
  )
})

test_that("age_groups zero-pads existing age_group labels", {
  df <- data.frame(
    age_group = c("1-5", "6-10", "11-15", NA_character_)
  )

  res <- age_groups(df)

  expect_equal(
    as.character(res$age_group[1:3]),
    c("01-05", "06-10", "11-15")
  )
  expect_true(anyNA(res$age_group))
})


test_that("break_at must be sorted integer vector", {
  df <- data.frame(age = 1:10)

  expect_error(
    age_groups(df, break_at = c(5L, 10L, 3L)),
    "Invalid break points"
  )

  expect_error(
    age_groups(df, break_at = c(5, 10, 20.5)),
    "Input of integer type is only allowed"
  )

  expect_no_error(
    age_groups(df, break_at = c(5L, 10L, 20L))
  )
})

