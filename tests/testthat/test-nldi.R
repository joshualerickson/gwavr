test_that("testing get catchment characteristics from nhdplusTools", {

  point <- dplyr::tibble(Long = -115.2312,Lat = 48.83037)

  point <- sf::st_as_sf(point, coords = c("Long", "Lat"), crs = 4326)

  comid <- nhdplusTools::discover_nhdplus_id(point)

  catchment_char <- nhdplusTools::get_catchment_characteristics("CAT_BFI", ids = comid)


})
