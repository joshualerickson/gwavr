
#' Get NLDI
#'
#' @description This function grabs the upstream tributaries, upstream main stream and basin boundary using
#' the NLDI API. It then combines the NLDI zonal stats to the basin boundary shape, i.e. 'TOT' is the 'total' basin zonal statistic.
#'
#' @param point A sf point.
#'
#' @return A list of UT, UM and basin boundary sf objects
#'
get_NLDI <- function(point){

  clat <- point$geometry[[1]][[2]]
  clng <- point$geometry[[1]][[1]]

  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


  nldiURLs <- list(site_data = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                                      clng,"%20", clat, "%29"),
                   basin_boundary = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin"),
                   UT = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UT/flowlines?distance=999"),
                   UM = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UM/flowlines?distance=999"))

  nldi_data <- list()


  for(n in names(nldiURLs)) {
    nldi_data[n] <- list(sf::read_sf(nldiURLs[n][[1]]))
    print(paste(n, "is of class", class(nldi_data[[n]]), "and has", nrow(nldi_data[[n]]), "features"))
  }

  total_characteristic <-  data.frame(COMID = nldi_data$site_data$comid) %>% mutate(ID = dplyr::row_number()) %>%
    group_by(ID) %>%
    tidyr::nest() %>%
    mutate(chars = purrr::map(data, ~nhdplusTools::get_nldi_characteristics(list(featureSource = "comid", featureID = as.character(.$COMID)),
                                                                            type = 'total'))) %>% tidyr::unnest(c(data, chars)) %>% tidyr::unnest(c(chars)) %>% dplyr::ungroup() %>%
    dplyr::select(COMID, characteristic_id, characteristic_value) %>%
    tidyr::pivot_wider(names_from = "characteristic_id", values_from = "characteristic_value") %>%
    dplyr::mutate(dplyr::across(is.character, as.numeric))


  nldi_data[['basin_boundary']] <- nldi_data[['basin_boundary']] %>%
    cbind(total_characteristic)
  nldi_data

}


#' Get NLDI Catchments
#'
#' @description This function grabs the 'local' zonal stats for 'all' subcatchments above a point or only for
#' the 'local' catchment using the NLDI API. This is different than get_NLDI(), which grabs the entire basin above a point.
#' @param point A sf point object.
#' @param type A \code{character} 'local' or 'all'.
#' @param method A \code{character} 'local' or 'all'.
#'
#' @note This function can be expensive when using type = 'local' and method = 'all' depending on
#' the size of the upstream area.
#'
#' @return A list of sf objects: UT and catchments.
#'
get_NLDI_catchments <- function(point, type = 'local', method = 'all'){

  clat <- point$geometry[[1]][[2]]
  clng <- point$geometry[[1]][[1]]

  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))

  if(method == 'all'){
    nldiURLs <- list(UT = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UT/flowlines?distance=999"))
  } else if (method == 'local'){
    nldiURLs <- list(UT = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigation/UT/flowlines?distance=0"))
  }

  nldi_data <- list()

  for(n in names(nldiURLs)) {
    nldi_data[n] <- list(sf::read_sf(nldiURLs[n][[1]]))
    print(paste(n, "is of class", class(nldi_data[[n]]), "and has", nrow(nldi_data[[n]]), "features"))
  }

  nldi_outlets <- nldi_data$UT$nhdplus_comid

  nldi_catch <- nhdplusTools::get_nhdplus(comid = nldi_outlets,
                                          realization = 'catchment')

  local_characteristic <-  data.frame(COMID = nldi_outlets) %>% mutate(ID = dplyr::row_number()) %>%
    group_by(ID) %>%
    tidyr::nest() %>%
    mutate(chars = purrr::map(data, ~nhdplusTools::get_nldi_characteristics(list(featureSource = "comid", featureID = as.character(.$COMID)),
                                                                            type = type))) %>% tidyr::unnest(c(data, chars)) %>% tidyr::unnest(c(chars)) %>% dplyr::ungroup() %>%
    dplyr::select(COMID, characteristic_id, characteristic_value) %>%
    tidyr::pivot_wider(names_from = "characteristic_id", values_from = "characteristic_value") %>%
    dplyr::rename(featureid = 'COMID') %>%
    dplyr::mutate(dplyr::across(is.character, as.numeric))%>%
    dplyr::mutate(featureid = as.integer(featureid))


  nldi_catch <- nldi_catch %>%
    dplyr::left_join(local_characteristic, by = c('featureid'))

  final_data <- list(nldi_data$UT, nldi_catch)
}

#' Base Map
#'
#' @description A generic leaflet base map used in the shiny apps.
#' @return A leaflet map with provider layers: \strong{"Esri.WorldImagery", "CartoDB.Positron",
#' "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"
#' "Hydrography"}
#' @export
#'
base_map <- function () {
  grp <- c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
           "OpenStreetMap", "CartoDB.DarkMatter",
           "Hydrography")
  att <- paste0("<a href='https://www.usgs.gov/'>", "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer",
            host, service)
  }
  map <- leaflet::leaflet()
  map <- leaflet::addProviderTiles(map = map, provider = grp[[1]],
                                   group = grp[[1]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[2]],
                                   group = grp[[2]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[3]],
                                   group = grp[[3]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[4]],
                                   group = grp[[4]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[5]],
                                   group = grp[[5]])
  opt <- leaflet::WMSTileOptions(format = "image/png",
                                 transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[6], options = opt, layers = "0", attribution = att)
  opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map <- leaflet::addLayersControl(map, baseGroups = grp[1:5],
                                   overlayGroups = grp[6], options = opt)
}


#' Get Basin Boundary NLDI
#' @description  This function uses the USGS water data API to link a point to a realized basin. This is
#' not the same as delineating from the exact point, rather this API uses NLDI to find the closest
#' basin downstream source point. There is a lot you can do with this API and I would recommend
#' looking at {nhdplusTools} as that has a lot of functionality and better documentation.
#' @param point A sf point object.
#'
#' @return An sf object with added \code{comid} and \code{basin}.
#' @note \code{point} needs geometry column.

get_Basin <- function(point){


  if(!'POINT' %in% sf::st_geometry_type(point)){"Need a sf POINT geometry"}

  #just added indexs to group by

  point <- point %>% dplyr::mutate(rowid = dplyr::row_number())

  final_basin <- point %>%
    split(.$rowid) %>%
    purrr::map(purrr::safely(~nldi_basin_function(.))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf() %>%
    dplyr::left_join(sf::st_drop_geometry(point), by = 'rowid') %>%
    dplyr::select(-rowid)

}


#' Calling NLDI API
#'
#' @param point sf data.frame
#'
#' @return a sf data.frame with watershed basin
nldi_basin_function <- function(point){

  clat <- point$geometry[[1]][[2]]
  clng <- point$geometry[[1]][[1]]
  rowid <- point$rowid
  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


  nldiURLs <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin")

  nldi_data <- sf::read_sf(nldiURLs)

  nldi_data <- nldi_data %>%
    dplyr::mutate(comid = nld$features$properties$identifier,
                  rowid = rowid)

}

#'
#' @title whitebox helpers for streams
#' @param aoi a sf polygon
#' @param z param for elevatr function get_elev_raster()
#' @param threshold numeric; cell threshold for stream delineation
#' @param prj A character vector with proj4string.
#'
#' @return a list of file paths to .tif files and a terra::rast object
get_whitebox_streams <- function(aoi,
                                 z,
                                 threshold,
                                 prj){


  if(missing(prj)){prj = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"}

  aoi <- aoi %>% sf::st_transform(prj)

  # download elevation
  ele <- elevatr::get_elev_raster(aoi,
                                  z = z,
                                  prj = prj,
                                  clip = 'locations')

  # write to temp file
  output <- tempfile(fileext = '.tif')
  terra::writeRaster(ele, output, overwrite = T)

  # perform smoothing and fill depressions
    whitebox::wbt_feature_preserving_smoothing(
      dem = output,
      output = output
    )

    whitebox::wbt_breach_depressions(dem = output,
                                     output = output)

  # get pointer (flow direction) and accumulation
  output_pointer <- tempfile(fileext = '.tif')

  whitebox::wbt_d8_pointer(output, output_pointer)

  output_fa <- tempfile(fileext = '.tif')
  whitebox::wbt_d8_flow_accumulation(input = output, output = output_fa, out_type = 'cells')

  # extract streams based on threshold
  output_streams <- tempfile(fileext = '.tif')

  whitebox::wbt_extract_streams(output_fa, output_streams, threshold = threshold)

  # get stream link identifier
  pour_pts <- tempfile(fileext = '.tif')
  whitebox::wbt_stream_link_identifier(output_pointer,output_streams, pour_pts)

  # get distance to outlet
  output_dist2out <- tempfile(fileext = '.tif')
  whitebox::wbt_distance_to_outlet(output_pointer, output_streams, output_dist2out)

  # get tributary identifier
  output_tribid <- tempfile(fileext = '.tif')
  whitebox::wbt_tributary_identifier(output_pointer, output_streams, output_tribid)

  # get upstream channel length
  output_upstr_channel_length <- tempfile(fileext = '.tif')
  whitebox::wbt_length_of_upstream_channels(output_pointer, output_streams, output_upstr_channel_length)

  # get max upstream distance
  output_maxdist <- tempfile(fileext = '.tif')
  whitebox::wbt_farthest_channel_head(output_pointer, output_streams, output_maxdist)

  # get horton
  output_horton <- tempfile(fileext = '.tif')
  whitebox::wbt_horton_stream_order(output_pointer, output_streams, output_horton)

  # get strahler
  output_strahler <- tempfile(fileext = '.tif')
  whitebox::wbt_strahler_stream_order(output_pointer, output_streams, output_strahler)

  # get shreve
  output_shreve <- tempfile(fileext = '.tif')
  whitebox::wbt_shreve_stream_magnitude(output_pointer, output_streams, output_shreve)

  # get hack
  output_hack <- tempfile(fileext = '.tif')
  whitebox::wbt_hack_stream_order(output_pointer, output_streams, output_hack)

  # get mainstream
  output_mainstream <- tempfile(fileext = '.tif')
  whitebox::wbt_find_main_stem(output_pointer, output_streams, output_mainstream)

  # get watershed associated with pour points
  output_ws <- tempfile(fileext = '.tif')

  whitebox::wbt_watershed(d8_pntr = output_pointer, pour_pts = pour_pts, output = output_ws)

  # convert watershed to polygons
  output_ws_poly <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_to_vector_polygons(input = output_ws, output = output_ws_poly)

  ws_poly <- sf::st_as_sf(sf::read_sf(output_ws_poly)) %>% sf::st_set_crs(prj) %>% sf::st_transform(4326)


  # generate a stream vector
  output_stream_vector <- tempfile(fileext = '.shp')

  whitebox::wbt_raster_streams_to_vector(output_streams, output_pointer, output_stream_vector)
  stream_vector <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>% sf::st_set_crs(prj) %>% sf::st_transform(4326)

#   # now get other stream vectors
#   # dist2outlet
#   output_stream_vector <- tempfile(fileext = '.shp')
#   whitebox::wbt_raster_streams_to_vector(output_dist2out, output_pointer, output_stream_vector)
#   stream_vector_dist2out <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>% sf::st_drop_geometry()
#
#   # trib id
#   output_stream_vector <- tempfile(fileext = '.shp')
#   whitebox::wbt_raster_streams_to_vector(output_tribid, output_pointer, output_stream_vector)
#   stream_vector_tribid <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>% sf::st_drop_geometry()
#
#   # upstream channel length
#   output_stream_vector <- tempfile(fileext = '.shp')
#   whitebox::wbt_raster_streams_to_vector(output_upstr_channel_length, output_pointer, output_stream_vector)
#   stream_vector_ucl <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>% sf::st_drop_geometry()

  final_data <- list(output_streams_rast = terra::rast(output_streams),
                     output_streams_path = output_streams,
                     output_pointer_path = output_pointer,
                     output_ws_poly = ws_poly,
                     output_stream_vector = stream_vector,
                     output_fa = output_fa)

}



#' @title whitbox helpers for watersheds
#' @param sf_point A point marker from leaflet map
#' @param prj A character vector with proj4string.
#' @param output_streams A stream raster generated with the initial bbox.
#' @param output_pointer A flow direction raster generated with the initial bbox.
#' @param snap_dist numeric; distance to snap marker to output_streams raster.
#'
#' @return A sf polygon of a watershed
#'
get_whitebox_ws <- function(sf_point, prj, output_streams, output_pointer, snap_dist) {

  if(missing(prj)){prj = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"}

  sf_point <- sf_point %>% sf::st_transform(prj)

  sf_pt <- tempfile(fileext = '.shp')
  sf::write_sf(sf_point, sf_pt, driver = 'ESRI Shapefile')

  output_pp <- tempfile(fileext = '.shp')
  whitebox::wbt_jenson_snap_pour_points(sf_pt, output_streams, output_pp, snap_dist = snap_dist)

  output_ws <- tempfile(fileext = '.tif')

  whitebox::wbt_watershed(d8_pntr = output_pointer,pour_pts = output_pp,output =  output_ws)

  output_ws_poly <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_to_vector_polygons(input = output_ws, output = output_ws_poly)

  ws_poly <- sf::st_as_sf(sf::read_sf(output_ws_poly)) %>% sf::st_set_crs(prj) %>% sf::st_transform(4326)

}


