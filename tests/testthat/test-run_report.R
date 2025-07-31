test_that("run_report() successfully produces an HTML", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(data = input_example, report_format = "HTML", method = "EARS", strata = "age_group", output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully produces a docx", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(data = input_example, report_format = "DOCX", method = "EARS", strata = "age_group", output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".docx")))
})


test_that("run_report() successfully works without any strata selected", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(data = input_example, report_format = "HTML", method = "EARS", strata = NULL, output_file = output_file, output_dir = output_dir)

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully works for a selected intervention_date", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(data = input_example, report_format = "HTML", method = "Mean", strata = NULL, output_file = output_file, output_dir = output_dir, intervention_date = "2021-03-01")

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() successfully works for a different number of weeks", {
  # check basics by using fast and easy algorithm EARS
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)
  output_file <- basename(output_path)
  output_dir <- dirname(output_path)

  run_report(data = input_example, report_format = "HTML", method = "Mean", strata = NULL, output_file = output_file, output_dir = output_dir, number_of_weeks = 12)

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() works with pre-computed signals_agg and signals_pad", {
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)

  disease <- unique(input_example$pathogen)

  preprocessed <- preprocess_data(
    input_example
  )

  signal_results <- get_signals_all(
    preprocessed,
    method = "ears",
    stratification = "age_group",
    number_of_weeks = 6
  )

  signals_agg_pad <- aggregate_pad_signals(
    signal_results,
    preprocessed,
    number_of_weeks = 6,
    method = "ears"
  )

  run_report(
    data = input_example,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    signals_agg = signals_agg_pad$signals_agg %>% dplyr::mutate(pathogen = disease),
    signals_pad = signals_agg_pad$signals_pad %>% dplyr::mutate(pathogen = disease)
  )

  expect_true(file.exists(paste0(output_path, ".html")))
})


test_that("run_report() works with a custom bslib theme", {
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)

  theme <- bslib::bs_theme(primary = "firebrick", bootswatch = "minty")

  run_report(
    data = input_example,
    report_format = "HTML",
    method = "EARS",
    strata = "age_group",
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    custom_theme = theme
  )

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() works with multiple pathogens", {
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)

  run_report(
    data = input_example_multipathogen,
    report_format = "HTML",
    method = "EARS",
    strata = "age_group",
    output_file = basename(output_path),
    output_dir = dirname(output_path)
  )

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() works with a subset of pathogens from a line list with multiple pathogens", {
  output_path <- tempfile()
  on.exit(unlink(output_path), add = TRUE)

  selected_pathogens <- unique(input_example_multipathogen$pathogen)[1:2]

  run_report(
    data = input_example_multipathogen %>% dplyr::filter(pathogen %in% selected_pathogens),
    report_format = "HTML",
    method = "EARS",
    strata = "age_group",
    output_file = basename(output_path),
    output_dir = dirname(output_path)
  )

  expect_true(file.exists(paste0(output_path, ".html")))
})

test_that("run_report() works with multiple pathogens using precomputed signals_agg and signals_padded", {
  output_path <- tempfile()
  on.exit(unlink(paste0(output_path, ".html")), add = TRUE)

  # Select two pathogens
  pathogens <- unique(input_example_multipathogen$pathogen)[1:2]
  preprocessed_data <- preprocess_data(input_example_multipathogen)

  signals_agg_list <- list()
  signals_padded_list <- list()

  for (pat in pathogens) {
    preprocessed_data_pat <- preprocessed_data %>%
      dplyr::filter(pathogen == pat)

    signals <- get_signals_all(
      preprocessed_data_pat,
      method = "ears",
      stratification = "age_group",
      number_of_weeks = 6
    ) %>%
      dplyr::mutate(pathogen = pat)

    signals_agg_pad <- aggregate_pad_signals(
      signals,
      preprocessed_data_pat,
      number_of_weeks = 6,
      method = "ears"
    )

    signals_agg_list[[pat]] <- signals_agg_pad$signals_agg %>% dplyr::mutate(pathogen = pat)
    signals_padded_list[[pat]] <- signals_agg_pad$signals_padded %>% dplyr::mutate(pathogen = pat)
  }

  signals_agg <- dplyr::bind_rows(signals_agg_list)
  signals_padded <- dplyr::bind_rows(signals_padded_list)
  # Clean up
  rm(signals_agg_list, signals_padded_list)
  gc()

  # Run report
  run_report(
    data = input_example_multipathogen,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    signals_agg = signals_agg,
    signals_pad = signals_padded
  )

  expect_true(file.exists(paste0(output_path, ".html")))
})
