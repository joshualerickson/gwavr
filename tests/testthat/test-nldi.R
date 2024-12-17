testthat::skip_on_cran()
testthat::skip_on_ci()

test_that("testing get catchment characteristics from nhdplusTools", {

  point <- dplyr::tibble(Long = -115.2312,Lat = 48.83037)

  point <- sf::st_as_sf(point, coords = c("Long", "Lat"), crs = 4326)

  comid <- nhdplusTools::discover_nhdplus_id(point)

  catchment_char <- nhdplusTools::get_catchment_characteristics("CAT_BFI", ids = comid)

  testthat::expect_equal(catchment_char$characteristic_value[[1]], 71)


  catchment_char <- nhdplusTools::get_catchment_characteristics("TOT_BFI", ids = comid)

  testthat::expect_equal(catchment_char$characteristic_value[[1]], 72.07)

  catchment_char <- nhdplusTools::get_catchment_characteristics("ACC_BFI", ids = comid)

  testthat::expect_equal(catchment_char$characteristic_value[[1]], 72.07)


})


test_that("testing upstream main and tribs from nhdplusTools", {

  point <- dplyr::tibble(Long = -115.2312,Lat = 48.83037)

  point <- sf::st_as_sf(point, coords = c("Long", "Lat"), crs = 4326)

  comid <- nhdplusTools::discover_nhdplus_id(point)

  nldiURLs <- list(site_data = paste0("https://api.water.usgs.gov/nldi/linked-data/comid/position?coords=POINT%28",
                                      point[1,1]$geometry[[1]][1],"%20", point[1,1]$geometry[[1]][2], "%29"),
                   basin_boundary = paste0("https://api.water.usgs.gov/nldi/linked-data/comid/",comid,"/basin"),
                   UT = paste0("https://api.water.usgs.gov/nldi/linked-data/comid/",comid,"/navigation/UT/flowlines?distance=999"),
                   UM = paste0("https://api.water.usgs.gov/nldi/linked-data/comid/",comid,"/navigation/UM/flowlines?distance=999"))

  nldi_data <- list()


  for(n in names(nldiURLs)) {
    nldi_data[n] <- list(sf::read_sf(nldiURLs[n][[1]]))
  }

  testthat::expect_equal(length(nldi_data), 4)
  testthat::expect_equal(length(nldi_data$site_data), 6)
  testthat::expect_equal(length(nldi_data$basin_boundary), 1)
  testthat::expect_equal(nrow(nldi_data$UT), 71)
  testthat::expect_equal(nrow(nldi_data$UM), 27)



})
