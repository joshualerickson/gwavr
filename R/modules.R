#' Shiny Module UI for nhdplus
#'
#' @description A shiny Module to.
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @importFrom shiny NS tagList reactiveValues observe
#' @importFrom dplyr filter select mutate slice_max ungroup rename
#' @importFrom grDevices hcl.colors
#' @importFrom sf st_area st_transform st_geometry st_as_sf
#' @importFrom scales comma
#' @importFrom leaflet addPolygons addPolylines addCircles
#' @return UI function for Shiny module
#' @export
#'
nhdplusModUI <- function(id, ...){
  ns <- shiny::NS(id)

  leaflet::leafletOutput(ns('leaf_map'), ...)
}

#' Shiny Module Server for nhdplus
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @return server function for Shiny module
#' @export
nhdplusMod <- function(input, output, session, values){
  ns <- session$ns


  values$hydro_data_list <- list()
  sf::sf_use_s2(FALSE)

  #css for pickerInput label
  css <- "
    label {background-color: rgba(255, 255, 255, 0.75);
    display: inline-block;
    max-width: 100%;
    margin-bottom: 5px;
    font-weight: 700;
    color: black;
    font-size: small;
    font-family: inherit;
    padding: 2.5px;}"

  #starting leaflet map
  output$leaf_map <- leaflet::renderLeaflet({

    base_map()  %>%
      leaflet::addControl(html = tags$div(tags$style(css),shinyWidgets::pickerInput(
        ns('location_map'), 'Select data type',
        choices = c("", c(
          `NHDPlus Catchments` = 'catchment',
          `NHDPlus Flowlines` = 'nhdplus',
          `NHDPlus Waterbodies` = 'waterbody',
          `NDHPlus Outlet` = 'outlet',
          `HUC 12` = 'huc12',
          `HUC 8` = 'huc8',
          `NWIS Sites` = 'nwis'
        )),options = shinyWidgets::pickerOptions(container = 'body'),
        width = '80%',
        choicesOpt = list(
          style = rep(("font-weight: bold;font-family: 'Montserrat', sans-serif;"),51)))),
        className = "fieldset { border: 0;}") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                     rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = F,
                                                                                             shapeOptions = leaflet.extras::drawShapeOptions(fillOpacity = 0, opacity = .75)),
                                     markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = F),
                                     polygonOptions = leaflet.extras::drawRectangleOptions(repeatMode = F,
                                                                                           shapeOptions = leaflet.extras::drawShapeOptions(fillOpacity = 0, opacity = .75)), targetGroup = 'draw') %>%
      leaflet::addControl(html = shiny::actionButton(ns("deletebtn"), "remove drawn"),
                          position = 'bottomleft',
                          className = 'fieldset {border:0;}') %>%
      # leaflet::addControl(html = shiny::actionButton("dowload", "download"),
      #                     position = 'bottomleft',
      #                     className = 'fieldset {border:0;}') %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter"),
                                overlayGroups = c("Hydrography"))
  })



  #leaflet proxy to update map with stations

  observeEvent(input$leaf_map_draw_new_feature, {

    shiny::req(nchar(input$location_map)>1)

    shiny::withProgress(

      message = paste0('Downloading ', input$location_map) ,
      detail = 'This may take a few seconds...', value = 0,{

        # this differentiates between rectangle and point draw in leaflet

        if(input$leaf_map_draw_new_feature$geometry$type != 'Point') {
          feat <- input$leaf_map_draw_new_feature
          coords <- unlist(feat$geometry$coordinates)
          coords <- matrix(coords, ncol = 2, byrow = T)

          data_sf <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>%
                                sf::st_as_sf()

        } else {

          click <- input$leaf_map_draw_new_feature
          clat <- click$geometry$coordinates[[2]]
          clng <- click$geometry$coordinates[[1]]

          data_sf <- tidyr::tibble(Lat = clat, Lon = clng)

          data_sf <- data_sf %>% sf::st_as_sf(coords = c('Lon', 'Lat')) %>%
            sf::st_set_crs(4326) %>%
            sf::st_transform(crs = 4326)
        }



        shiny::setProgress(1/2)

        if(input$location_map == "huc12"){
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_huc12(data_sf)},
                                          error = function(e) {
                                            'error'
                                          })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No HUC 12 Features Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            leaflet::addPolygons(data = values$hydro_data %>%
                                   sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                                                "<b>HUC Name: </b>",values$hydro_data$name,
                                                                                                                "<br>", "<b>HUC #: </b>", values$hydro_data$huc12,
                                                                                                                "<br>", "<b>HUC Area: </b>",scales::comma(round(values$hydro_data$areaacres,0)), " acres" ) )

          } else if (input$location_map == "huc8") {
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_huc8(data_sf)},
                                          error = function(e) {
                                            'error'
                                          })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No HUC 8 Features Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            leaflet::addPolygons(data = values$hydro_data%>%
                                   sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                                                "<b>HUC Name: </b>",values$hydro_data$name,
                                                                                                                "<br>", "<b>HUC #: </b>", values$hydro_data$huc8,
                                                                                                                "<br>", "<b>HUC Area: </b>",scales::comma(round(values$hydro_data$areaacres,0)), " acres" ) )

        } else if (input$location_map == "catchment") {
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_nhdplus(data_sf, realization = 'catchment')},
                                          error = function(e){
                                            'error'
                                          }

            )
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No Catchments Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            addPolygons(data = values$hydro_data %>%
                          sf::st_make_valid() %>%
                          sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326") , popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                                        "<b>Feature ID: </b>",values$hydro_data$featureid,
                                                                                                        "<br>", "<b>Area: </b>",scales::comma(round(values$hydro_data$areasqkm,0)*247.105), " acres" ) )

        } else if (input$location_map == "nhdplus") {
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_nhdplus(data_sf)},
                                          error = function(e){
                                            'error'
                                          })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No Flowlines Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            addPolylines(data = values$hydro_data%>%
                           sf::st_make_valid() %>%
                           st_as_sf() %>%
                           sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326") , popup = paste0("<p style=line-height:20px;margin:0px;>",
                                                                                                         "<b>Name: </b>",values$hydro_data$gnis_name,
                                                                                                         "<br>", "<b>Stream Order (strahler): </b>", values$hydro_data$streamorde,
                                                                                                         "<br>", "<b>Arbolate Sum: </b>", paste0(values$hydro_data$arbolatesu, " mi"),
                                                                                                         "<br>", "<b>Drainage Area: </b>", paste0(comma(round(values$hydro_data$totdasqkm*247.105, 0)), ' acres'),
                                                                                                         "<br>", "<b>Mean Annual Flow (MAF): </b>", paste0(values$hydro_data$qa_ma, ' cfs'),
                                                                                                         "<br>", "<b>MAF with Ref. Gage (MAFqc): </b>", paste0(values$hydro_data$qc_ma, ' cfs'),
                                                                                                         "<br>", "<b>Mean Flow (April, May, June): </b>", paste0(values$hydro_data$qa_04, ' , ', values$hydro_data$qa_05, ' , ', values$hydro_data$qa_06 ),
                                                                                                         "<br>", "<b>Slope: </b>", paste0(round(values$hydro_data$slope*100,2), " %"),
                                                                                                         "<br>", "<b>Length: </b>", paste0(round(values$hydro_data$lengthkm*0.621371,2), " mi"),
                                                                                                         "<br>", "<b>Path Length (terminal): </b>", paste0(comma(round(values$hydro_data$pathlength,0)), " mi" ),
                                                                                                         "<br>", "<b>Mean Annual Time of Travel: </b>", paste0(round(values$hydro_data$totma,3), " days" ),
                                                                                                         "<br>", "<b>COMID #: </b>", values$hydro_data$comid,
                                                                                                         "<br>", "<b>F-type: </b>",values$hydro_data$ftype,
                                                                                                         "<br>", "<b>F-code: </b>",values$hydro_data$fcode))

        } else if (input$location_map == "outlet") {
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_nhdplus(data_sf, realization = 'outlet')},
                                          error = function(e){
                                            'error'
                                          })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No Outlets Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            addCircles(data = values$hydro_data, weight = 15, popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                               "<b>Name: </b>",values$hydro_data$gnis_name,
                                                                               "<br>", "<b>Path Length (terminal): </b>", paste0(comma(round(values$hydro_data$pathlength,0)), " mi" ),
                                                                               "<br>", "<b>Mean Annual Time of Travel: </b>", paste0(round(values$hydro_data$totma,4), " days" ),
                                                                               "<br>", "<b>COMID #: </b>", values$hydro_data$comid,
                                                                               "<br>", "<b>F-type: </b>",values$hydro_data$ftype))

        } else if (input$location_map == "nwis") {
          promises::future_promise({
            values$hydro_data <-  tryCatch({nhdplusTools::get_nwis(data_sf)},
                                           error = function(e){
                                             'error'
                                           })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No NWIS Sites Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            addCircles(data = values$hydro_data %>%
                         st_as_sf() %>%
                         st_transform(crs = 4326,proj4string = "+init=epsg:4326"),radius = 15, weight = 15,
                       color = "red", popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                     "<b>Name: </b>",values$hydro_data$station_nm,
                                                     "<br>", "<b>Site #: </b>", values$hydro_data$site_no))

        } else if (input$location_map == 'waterbody'){
          promises::future_promise({
            values$hydro_data <- tryCatch({nhdplusTools::get_waterbodies(data_sf)},
                                          error = function(e){
                                            'error'
                                          })
          })
          if(class(values$hydro_data)[[1]] != 'sf'){

            shinyWidgets::show_alert('No Waterbodies  Found',
                                     'please try a new area',
                                     type = 'warning')}

          req(class(values$hydro_data)[[1]] == 'sf')

          leaflet::leafletProxy("leaf_map", session) %>%
            addPolygons(data = values$hydro_data %>%
                          sf::st_make_valid() %>%
                          sf::st_as_sf() %>%
                          sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                                       "<b>Name: </b>",values$hydro_data$gnis_name,
                                                                                                       "<br>", "<b>Area: </b>",paste(comma(round(values$hydro_data$areasqkm*247.105,0)), " acres" ),
                                                                                                       "<br>", "<b>F-type: </b>",values$hydro_data$ftype,
                                                                                                       "<br>", "<b>F-code: </b>",values$hydro_data$fcode,
                                                                                                       "<br>", "<b>Mean Depth: </b>",paste(round(values$hydro_data$meandepth*3.28084, 2), " ft"),
                                                                                                       "<br>", "<b>Lake Volume: </b>",paste(comma(round(values$hydro_data$lakevolume*35.3147,2)), " cf"),
                                                                                                       "<br>", "<b>Max Depth: </b>", paste(round(values$hydro_data$maxdepth*3.28084, 2), " ft")))

        }
      })


      values$out <- list(values$hydro_data)
      names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

      values$hydro_data_list <- append(values$hydro_data_list, values$out)


 })

  # keep track of newly drawn shapes
  values$drawnshapes <- list()

  # observe our simple little button to remove
  observeEvent(
    input$deletebtn,
    {
      lapply(
        values$drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid=paste0(session$ns('leaf_map')), layerid=todelete)
          )
        }
      )
    }
  )
  # we are fortunate here since we get an event
  #   draw_all_features
  observeEvent(
    input$leaf_map_draw_all_features,
    {
      values$drawnshapes <- lapply(
        input$leaf_map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
    }
  )

}




