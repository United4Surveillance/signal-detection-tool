# pre computed datasets needed for the tests

input_prepro <- input_example %>%
  preprocess_data()

signals_agg <- input_prepro %>%
  get_signals(stratification = c("sex", "county"), number_of_weeks = 6) %>%
  aggregate_signals(number_of_weeks = 6)

# modify input example to have only one county matching our nuts_shp, all other region ids get a Z in the beginning
input_example_mod <- input_prepro %>%
  dplyr::mutate(county_id = dplyr::if_else(county_id == "AT32",county_id,paste0("Z",county_id)))

signals_agg_mod <- input_example_mod %>%
  get_signals(stratification = c("sex", "county"), number_of_weeks = 6) %>%
  aggregate_signals(number_of_weeks = 6)

# modify the data such that none of the county_id match the shapefile, i.e. attach a Z to all of them
input_example_no_match <- input_prepro %>%
  dplyr::mutate(county_id = paste0("Z",county_id))

signals_agg_no_match <- input_example_no_match %>%
  get_signals(stratification = c("sex", "county"), number_of_weeks = 6) %>%
  aggregate_signals(number_of_weeks = 6)

# These tests are testing a currently still internal function of the package thus SignalDetectionTool::: needs to be added such that R CMD Check does not fail
test_that("test map visualisation with our example data works in interactive mode", {

  map <- SignalDetectionTool:::create_map_or_table(signals_agg, input_example, "county", nuts_shp, interactive = T)
  expect_s3_class(map, "plotly")

})

test_that("test map visualisation with our example data works in non interactive mode", {

  map <- SignalDetectionTool:::create_map_or_table(signals_agg, input_example, "county", nuts_shp, interactive = F)
  expect_s3_class(map, "ggplot")

})

test_that("test map visualisation works when only one region matched the shapefile (interactive)", {

  map <- SignalDetectionTool:::create_map_or_table(signals_agg, input_example, "county", nuts_shp, interactive = F)
  expect_s3_class(map, "ggplot")

})

test_that("test table being generated when no region matched the shapefile (interactive)", {

  map <- SignalDetectionTool:::create_map_or_table(signals_agg, input_example_mod, "county", nuts_shp, interactive = T)
  expect_s3_class(map, "plotly")

})

test_that("test table being generated when no region matched the shapefile (interactive)", {

  table <- SignalDetectionTool:::create_map_or_table(signals_agg_no_match, input_example_no_match, "county", nuts_shp, interactive = T)
  expect_s3_class(table, "datatables")

})

test_that("test table being generated when no region matched the shapefile (non interactive)", {

  table <- SignalDetectionTool:::create_map_or_table(signals_agg_no_match, input_example_no_match, "county", nuts_shp, interactive = F)
  expect_s3_class(table, "flextable")

})

