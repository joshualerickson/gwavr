
#' Get NLDI
#'
#' @description This function grabs the upstream tributaries, upstream main stream and basin boundary using
#' the NLDI API. It then combines the NLDI zonal stats to the basin boundary shape, i.e. 'TOT' is the 'total' basin zonal statistic.
#'
#' @param point A sf point.
#'
#' @return A list of UT, UM and basin boundary sf objects
#' @export
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
#' @export
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

