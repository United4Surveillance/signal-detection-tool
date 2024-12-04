test_that("Aggregation of data is performed correctly", {
  linelist1 <- data.frame(
    date_report = seq.Date(from = as.Date("2024-10-01"), by = "days", length.out = 10),
    age_group = c("00-04", "05-09", "35-39", "65-69", NA_character_, rep("05-09", 5))
  )
  # check that with NAs inside the outbreak_status aggregation still works correctly
  linelist2 <- data.frame(
    date_report = seq.Date(from = as.Date("2024-10-01"), by = "days", length.out = 10),
    age_group = c("00-04", "05-09", "35-39", "65-69", NA_character_, rep("05-09", 5)),
    outbreak_status = c(rep(c("yes", "no", NA_character_), 2), rep(NA_character_, 4))
  ) %>%
    dplyr::mutate(outbreak_status = factor(outbreak_status, levels = yes_no_unknown_levels()))

  # add week and year vars
  linelist1 <- linelist1 %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOYear,
      .names = "{.col}_year"
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOWeek,
      .names = "{.col}_week"
    ))

  linelist2 <- linelist2 %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOYear,
      .names = "{.col}_year"
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
      ~ surveillance::isoWeekYear(.x)$ISOWeek,
      .names = "{.col}_week"
    ))

  linelist1_agg <- aggregate_data(linelist1)
  linelist2_agg <- aggregate_data(linelist2)

  expect_equal(data.frame(linelist1_agg), data.frame(week = c(40, 41), year = c(2024, 2024), cases = c(6, 4)))
  expect_equal(data.frame(linelist2_agg), data.frame(week = c(40, 41), year = c(2024, 2024), cases = c(6, 4), cases_in_outbreak = c(2, 0)))
})
