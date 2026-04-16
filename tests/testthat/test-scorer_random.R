testthat::test_that("score_randomly() returns the required structure", {
  signal_results <- tibble::tibble(
    .row_id = 1:5,
    signal_id = letters[1:5]
  )

  result <- score_randomly(signal_results)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_identical(names(result), c(".row_id", "score"))
  testthat::expect_equal(nrow(result), 5)
  testthat::expect_identical(result$.row_id, 1:5)
  testthat::expect_true(is.numeric(result$score))
  testthat::expect_false(anyNA(result$score))
  testthat::expect_true(all(result$score >= 0))
  testthat::expect_true(all(result$score <= 1))
})
