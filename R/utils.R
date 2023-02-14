
#' Get NLDI
#'
#' @description This function grabs the upstream tributaries, upstream main stream and basin boundary using
#' the NLDI API. It then combines the NLDI zonal stats to the basin boundary shape, i.e. 'TOT' is the 'total' basin zonal statistic.
#'
#' @param point A sf point.
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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

  # get tributary identifier
  output_tribid <- tempfile(fileext = '.tif')
  whitebox::wbt_tributary_identifier(output_pointer, output_streams, output_tribid)

  # get slope
  output_slope <- tempfile(fileext = '.tif')
  whitebox::wbt_stream_link_slope(output_pointer, pour_pts,output, output_slope)

  # get length
  output_length <- tempfile(fileext = '.tif')
  whitebox::wbt_stream_link_length(output_pointer, pour_pts,output_length)

  # get strahler
  output_strahler <- tempfile(fileext = '.tif')
  whitebox::wbt_strahler_stream_order(output_pointer, output_streams, output_strahler)

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
  stream_vector <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj)

  # now get other stream vectors
  # trib id
  output_stream_vector <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_streams_to_vector(output_tribid, output_pointer, output_stream_vector)
  stream_vector_tribid <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj) %>%
    dplyr::select(-FID) %>%
    dplyr::rename(tribid = 'STRM_VAL') %>%
    sf::st_set_crs(prj)

  # slope
  output_stream_vector <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_streams_to_vector(output_slope, output_pointer, output_stream_vector)
  stream_vector_slope <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj) %>%
    dplyr::select(-FID) %>%
    dplyr::rename(slope = 'STRM_VAL') %>%
    sf::st_set_crs(prj)

  # slope
  output_stream_vector <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_streams_to_vector(output_length, output_pointer, output_stream_vector)
  stream_vector_length <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj) %>%
    dplyr::select(-FID) %>%
    dplyr::rename(length = 'STRM_VAL') %>%
    sf::st_set_crs(prj)

  # strahler
  output_stream_vector <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_streams_to_vector(output_strahler, output_pointer, output_stream_vector)
  stream_vector_strahler <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj) %>%
    dplyr::select(-FID) %>%
    dplyr::rename(strahler = 'STRM_VAL') %>%
    sf::st_set_crs(prj)

  # mainstream
  output_stream_vector <- tempfile(fileext = '.shp')
  whitebox::wbt_raster_streams_to_vector(output_mainstream, output_pointer, output_stream_vector)
  stream_vector_mainstem <- sf::st_as_sf(sf::read_sf(output_stream_vector)) %>%
    sf::st_set_crs(prj) %>%
    dplyr::select(-FID) %>%
    dplyr::rename(mainstem = 'STRM_VAL') %>%
    sf::st_set_crs(prj)

  stream_vect_final <- sf::st_join(stream_vector,stream_vector_tribid, sf::st_contains)
  stream_vect_final <- sf::st_join(stream_vect_final,stream_vector_strahler,sf::st_contains)
  stream_vect_final <- sf::st_join(stream_vect_final,stream_vector_slope,sf::st_contains)
  stream_vect_final <- sf::st_join(stream_vect_final,stream_vector_length,sf::st_contains)
  stream_vect_final <- sf::st_join(stream_vect_final,stream_vector_mainstem,sf::st_covered_by) %>%
                       sf::st_transform(4326)

  final_data <- list(output_streams_rast = terra::rast(output_streams),
                     output_streams_path = output_streams,
                     output_pointer_path = output_pointer,
                     output_ws_poly = ws_poly,
                     output_stream_vector = stream_vect_final,
                     output_fa = output_fa)

}



#' @title whitbox helpers for watersheds
#' @param sf_point A point marker from leaflet map
#' @param prj A character vector with proj4string.
#' @param output_streams A stream raster generated with the initial bbox.
#' @param output_pointer A flow direction raster generated with the initial bbox.
#' @param snap_dist numeric; distance to snap marker to output_streams raster.
#' @noRd
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




#' Get USGS Current Conditions
#'
#' @return a \code{tibble} with current conditions and attributes from USGS dashboard.
#' @noRd
#' @note The time zone used in the URL call is the R session time zone. Also, the time is 1-hour behind.
#' Here are the attributes that are with the data.frame: AgencyCode,SiteNumber,SiteName,SiteTypeCode,Latitude,Longitude,
#' CurrentConditionID,ParameterCode,TimeLocal,TimeZoneCode,Value,
#' ValueFlagCode,RateOfChangeUnitPerHour,StatisticStatusCode,FloodStageStatusCode.
current_conditions <- function(){

  user_date <- Sys.Date()
  user_time <- format(Sys.time(), "%H:%M:%S")

  ids <- paste0("https://dashboard.waterdata.usgs.gov/service/cwis/1.0/odata/CurrentConditions?$top=15000&$filter=(UpdatedUtc%20gt%20",
                user_date,"T",user_time,".190Z)%20and%20(AccessLevelCode%20eq%20%27P%27)%20and%20(1%20eq%201%20and%20true)%20and%20(SiteTypeCode%20in%20(%27ST%27,%27ST-TS%27,%27ST-CA%27,%27ST-DCH%27))%20and%20(ParameterCode%20in%20(%2730208%27,%2730209%27,%2750042%27,%2750050%27,%2750051%27,%2772137%27,%2772138%27,%2772139%27,%2772177%27,%2772243%27,%2774072%27,%2781395%27,%2799060%27,%2799061%27,%2700056%27,%2700058%27,%2700059%27,%2700060%27,%2700061%27))&$select=AgencyCode,SiteNumber,SiteName,SiteTypeCode,Latitude,Longitude,CurrentConditionID,ParameterCode,TimeLocal,TimeZoneCode,Value,ValueFlagCode,RateOfChangeUnitPerHour,StatisticStatusCode,FloodStageStatusCode&$orderby=SiteNumber,AgencyCode,ParameterCode,TimeLocal%20desc&caller=National%20Water%20Dashboard%20default")

  error_ids <- try(httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "cc_tmp.json"),overwrite = TRUE)))

  status_current <- jsonlite::fromJSON(file.path(tempdir(),"cc_tmp.json"))$value %>%
    dplyr::tibble()

  status_current <- status_current %>%
    dplyr::mutate(
      StatisticsStatusDescription = dplyr::case_when(
        StatisticStatusCode == "P0" ~'All-time low for this day',
        StatisticStatusCode == "P0_10"~"Much below normal",
        StatisticStatusCode == "P10_25"~"Below normal",
        StatisticStatusCode == "P25_75"~"Normal",
        StatisticStatusCode == "P75_90"~"Above normal",
        StatisticStatusCode == "P90_100"~"Much above normal",
        StatisticStatusCode == "P100" ~ 'All-time high for this day',
        StatisticStatusCode == "NR_0FLOW"~"Not flowing",
        StatisticStatusCode == "NR_REVFLOW"~"Not ranked",
        StatisticStatusCode == "NR_NOMEAS"~"Measurement flag",
        !is.na(ValueFlagCode) & is.na(StatisticStatusCode) ~ "Measurement flag",
        TRUE~"Not ranked"
      ),
      StatisticsStatusDescription = factor(StatisticsStatusDescription,
                                           levels = c("Not ranked",
                                                      "Measurement flag",
                                                      "Not flowing",
                                                      "All-time low for this day",
                                                      "Much below normal",
                                                      "Below normal",
                                                      "Normal",
                                                      "Above normal",
                                                      "Much above normal",
                                                      "All-time high for this day")),
      StatisticsStatusColorFill = dplyr::case_when(
        StatisticsStatusDescription == "Not ranked" ~ '#FFFFFF',
        StatisticsStatusDescription == "Measurement flag" ~ '#989898',
        StatisticsStatusDescription == "Not flowing" ~ '#fff',
        StatisticsStatusDescription == "All-time low for this day" ~ "#FF0000",
        StatisticsStatusDescription == "Much below normal" ~ "#BB2222",
        StatisticsStatusDescription == "Below normal" ~ "#FFAA00",
        StatisticsStatusDescription == "Normal" ~ "#00ff00",
        StatisticsStatusDescription == "Above normal" ~ "#44dddd",
        StatisticsStatusDescription == "Much above normal" ~ "#0000FF",
        StatisticsStatusDescription == "All-time high for this day" ~ "#000055",
        TRUE ~ NA_character_

      ),
      StatisticsStatusColorStroke = dplyr::case_when(
        StatisticsStatusDescription == "Not ranked" ~ '#666666',
        StatisticsStatusDescription == "Measurement flag" ~ '#996633',
        StatisticsStatusDescription == "Not flowing" ~ '#997700',
        StatisticsStatusDescription == "All-time low for this day" ~ "#990000",
        StatisticsStatusDescription == "Much below normal" ~ "#661111",
        StatisticsStatusDescription == "Below normal" ~ "#996600",
        StatisticsStatusDescription == "Normal" ~ "#009900",
        StatisticsStatusDescription == "Above normal" ~ "#11aaaa",
        StatisticsStatusDescription == "Much above normal" ~ "#000099",
        StatisticsStatusDescription == "All-time high for this day" ~ "#000000",
        TRUE ~ NA_character_
      ),
      StatisticsStatusColorFill = factor(StatisticsStatusColorFill,
                                         levels = c('#FFFFFF',
                                                    '#989898',
                                                    '#F5F5F5',
                                                    "#FF0000",
                                                    "#BB2222",
                                                    "#FFAA00",
                                                    "#00ff00",
                                                    "#44dddd",
                                                    "#0000FF",
                                                    "#000055")),
      StatisticsStatusColorStroke = factor(StatisticsStatusColorStroke,
                                           levels = c('#666666',
                                                      '#996633',
                                                      '#997700',
                                                      "#990000",
                                                      "#661111",
                                                      "#996600",
                                                      "#009900",
                                                      "#11aaaa",
                                                      "#000099",
                                                      "#000000"))
    )
}



#' @title Get Instananeous Values
#' @param sites A character vector of site ids.
#' @param period A numeric.
#' @noRd
#' @return A data.frame.
get_iv <- function(sites, period) {

  df_final <- data.frame()

  for(i in sites){
  base_url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
                       i,
                       "&period=P",period,"D&parameterCd=00060&siteStatus=all"
                     )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "usgs_tmp.csv"),
                                      overwrite = TRUE))
  # read RDB file to R
  df <- utils::read.table(file.path(tempdir(),"usgs_tmp.csv"),
                          header = TRUE,
                          sep = "\t",
                          stringsAsFactors = FALSE)

  base_url_site <- paste0("https://waterservices.usgs.gov/nwis/site/?format=rdb&sites=",
                     i,"&siteStatus=all"
  )

  # try to download the data
  error_site <- httr::GET(url = base_url_site,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "usgs_tmp.csv"),
                                      overwrite = TRUE))
  # read RDB file to R
  df_site <- utils::read.csv(file.path(tempdir(),"usgs_tmp.csv"),
                          header = TRUE,
                          comment.char = '#',
                          sep = "\t",
                          stringsAsFactors = FALSE)
  #remove excess data
  df_site <- df_site[-1,]

  df <- df[-1,-6]

  df <- df %>% dplyr::mutate(dplyr::across(dplyr::contains(c('agency_cd', 'site_no', 'datetime')), ~dplyr::na_if(.,"")))

  df <- df %>% dplyr::rename_with(~paste0('flow_chr'), dplyr::contains('_00060')) %>%
    select(site_no, datetime, flow_chr)

  df <- df %>% dplyr::mutate(flow_num = as.numeric(flow_chr),
                             datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M"))

  df <- df %>% dplyr::left_join(df_site, by = 'site_no')
  df_final <- rbind(df_final, df)

  }

  df_final

}



#' @title Get daily Values
#' @param sites A character vector of site ids.
#' @noRd
#' @return A data.frame.
get_dv <- function(sites) {

  df_final <- data.frame()

  for(i in sites){
    base_url <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=",
                       i,"&startDT=1800-02-01&siteStatus=all"
    )

    # try to download the data
    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "usgs_tmp.csv"),
                                        overwrite = TRUE))
    # read RDB file to R
    df <- utils::read.table(file.path(tempdir(),"usgs_tmp.csv"),
                            header = TRUE,
                            sep = "\t",
                            stringsAsFactors = FALSE)

    base_url_site <- paste0("https://waterservices.usgs.gov/nwis/site/?format=rdb&sites=",
                            i,"&siteStatus=all"
    )

    # try to download the data
    error_site <- httr::GET(url = base_url_site,
                            httr::write_disk(path = file.path(tempdir(),
                                                              "usgs_tmp.csv"),
                                             overwrite = TRUE))
    # read RDB file to R
    df_site <- utils::read.csv(file.path(tempdir(),"usgs_tmp.csv"),
                               header = TRUE,
                               comment.char = '#',
                               sep = "\t",
                               stringsAsFactors = FALSE)
    #remove excess data
    df_site <- df_site[-1,]

    df <- df[-1,]

    df <- df %>% dplyr::mutate(dplyr::across(dplyr::contains(c('agency_cd', 'site_no', 'datetime')), ~dplyr::na_if(.,"")))

    df <- df %>% dplyr::rename_with(~paste0('flow_chr'), dplyr::ends_with('_00060_00003')) %>%
      select(site_no, datetime, flow_chr)

    df <- df %>% dplyr::mutate(flow_num = as.numeric(flow_chr),
                               datetime = as.POSIXct(datetime, format="%Y-%m-%d"))

    df <- df %>% dplyr::left_join(df_site, by = 'site_no')
    df_final <- rbind(df_final, df)

  }

  df_final

}


#' @keywords internal
#' @description Taken from the interals of the \link{mapedit} package
add_select_script <- function(lf, styleFalse, styleTrue, ns="") {
  ## check for existing onRender jsHook?

  htmlwidgets::onRender(
    lf,
    sprintf(
      "
function(el,x) {
  var lf = this;
  var style_obj = {
    'false': %s,
    'true': %s
  }
  // define our functions for toggling
  function toggle_style(layer, style_obj) {
    layer.setStyle(style_obj);
  };
  function toggle_state(layer, selected, init) {
    if(typeof(selected) !== 'undefined') {
      layer._mapedit_selected = selected;
    } else {
      selected = !layer._mapedit_selected;
      layer._mapedit_selected = selected;
    }
    if(typeof(Shiny) !== 'undefined' && Shiny.onInputChange && !init) {
      Shiny.onInputChange(
        '%s-mapedit_selected',
        {
          'group': layer.options.group,
          'id': layer.options.layerId,
          'selected': selected
        }
      )
    }
    return selected;
  };
  // set up click handler on each layer with a group name
  lf.eachLayer(function(lyr){
    if(lyr.on && lyr.options && lyr.options.layerId) {
      // start with all unselected ?
      toggle_state(lyr, false, init=true);
      toggle_style(lyr, style_obj[lyr._mapedit_selected]);
      lyr.on('click',function(e){
        var selected = toggle_state(e.target);
        toggle_style(e.target, style_obj[String(selected)]);
      });
    }
  });
}
",
jsonlite::toJSON(styleFalse, auto_unbox=TRUE),
jsonlite::toJSON(styleTrue, auto_unbox=TRUE),
ns
    )
  )
}
