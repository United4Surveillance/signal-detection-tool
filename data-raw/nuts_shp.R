## code to prepare `nuts_shp` dataset goes here
nuts_shp <- sf::st_read("dev/data/shp/NUTS_RG_03M_2021_3035.shp")
usethis::use_data(nuts_shp, overwrite = TRUE)
