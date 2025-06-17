test_that("run_report() successfully produces an HTML", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(report_format = "HTML", method = "EARS", strata = "age_group", output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully produces a docx", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(report_format = "DOCX", method = "EARS", strata = "age_group", output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".docx")))
})


test_that("run_report() successfully works without any strata selected", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(report_format = "HTML", method = "EARS", strata = NULL, output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully works for a selected intervention_date", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(report_format = "HTML", method = "Mean", strata = NULL, output_file = output_file, output_dir = output_dir, intervention_date = "2021-03-01")

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully works for a different number of weeks", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(report_format = "HTML", method = "Mean", strata = NULL, output_file = output_file, output_dir = output_dir, number_of_weeks = 12)

  expect_true(file.exists(paste0(output_path, ".html")))
})
