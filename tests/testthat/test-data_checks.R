test_that("age group format check works as expected", {
  expect_true(is_age_group_format(c("<30","30-35","40-45","45+")))
  expect_true(is_age_group_format(c("<30","30-35","40-45",">45")))
  expect_false(is_age_group_format(c("below_one","one_to_five")))
  expect_false(is_age_group_format(c("<30","30-35","40/45","45+")))
  expect_false(is_age_group_format(c("<30","30-35","40+45","45+")))
})

test_that("age group format check works as expected for the last age group", {
  expect_true(is_last_age_group_format(c("<30","30_35","40_45","45+")))
  expect_true(is_last_age_group_format(c("<30","30-35","40-45","45-50")))
  expect_true(is_last_age_group_format(c("<30","30-35","40-45",">45")))
  expect_false(is_last_age_group_format(c("<30","30-35","40-45","45-")))
  expect_false(is_last_age_group_format(c("<30","30-35","40-45","45")))
})
