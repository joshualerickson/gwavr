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
#' @importFrom promises finally "%...>%"
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
                                                                                           shapeOptions = leaflet.extras::drawShapeOptions(fillOpacity = 0, opacity = .75)), targetGroup = 'draw')%>%
      leaflet::addControl(html = shiny::actionButton(ns("deletebtn"), "remove drawn"),
                          position = 'bottomleft',
                          className = 'fieldset {border:0;}') %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter"),
                                overlayGroups = c("Hydrography"))
  })



  #below is a mess with redundant code but
  # not sure how to deal with when using futures....

  observeEvent(input$leaf_map_draw_new_feature, {

    shiny::req(nchar(input$location_map)>1)


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




        if(input$location_map == "huc12"){

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

         promises::future_promise({
           sf::sf_use_s2(FALSE)
          nhdplusTools::get_huc12(data_sf)


          }) %...>% {

          values$hydro_data <- .

          values$out <- list(values$hydro_data)
          names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

          values$hydro_data_list <- append(values$hydro_data_list, values$out)

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

          } %>%
           finally(~p$close())

          } else if (input$location_map == "huc8") {

            p <- shiny::Progress$new()
            p$set(message = "Downloading data...",
                  detail = "This may take a little bit...",
                  value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
           nhdplusTools::get_huc8(data_sf)

          }) %...>% {

            values$hydro_data <- .

            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)

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
          } %>%
            finally(~p$close())

        } else if (input$location_map == "catchment") {

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
           nhdplusTools::get_nhdplus(data_sf, realization = 'catchment')
          }) %...>% {

            values$hydro_data <- .
            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)

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
          } %>%
            finally(~p$close())

        } else if (input$location_map == "nhdplus") {

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
            nhdplusTools::get_nhdplus(data_sf)
          }) %...>% {

            values$hydro_data <- .
            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)
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

          } %>%
            finally(~p$close())

        } else if (input$location_map == "outlet") {

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
          nhdplusTools::get_nhdplus(data_sf, realization = 'outlet')
          }) %...>% {

            values$hydro_data <- .
            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)
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
          } %>%
            finally(~p$close())

        } else if (input$location_map == "nwis") {

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
          nhdplusTools::get_nwis(data_sf)
          }) %...>% {

            values$hydro_data <- .
            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)
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
          } %>%
            finally(~p$close())

        } else if (input$location_map == 'waterbody'){

          p <- shiny::Progress$new()
          p$set(message = "Downloading data...",
                detail = "This may take a little bit...",
                value = 1/2)

          promises::future_promise({
            sf::sf_use_s2(FALSE)
          nhdplusTools::get_waterbodies(data_sf)
          }) %...>% {

            values$hydro_data <- .
            values$out <- list(values$hydro_data)
            names(values$out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

            values$hydro_data_list <- append(values$hydro_data_list, values$out)

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

          } %>%
            finally(~p$close())
        }





 })

  # keep track of newly drawn shapes
  drawnshapes <- list()

  # we are fortunate here since we get an event
  #   draw_all_features
  observeEvent(
    input$leaf_map_draw_all_features,
    {
      drawnshapes <<- lapply(
        input$leaf_map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
    }
  )


  # observe our simple little button to remove
  observeEvent(
    input$deletebtn,
    {
      lapply(
        drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid=paste0(session$ns("leaf_map")), layerid=todelete)
          )
        }
      )
    }
  )

}

#' Shiny Module UI for basin generation
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
basinModUI <- function(id, ...){
  ns <- shiny::NS(id)

  leaflet::leafletOutput(ns('leaf_map'), ...)
}


#' Shiny Module Server for basin generation
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom promises finally "%...>%"
#' @export
basinMod <- function(input, output, session, values){

  ns <- session$ns

  values$basin_data_list <- list()

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

leaf_map <-
    base_map() %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
        ns('map_res'), 'Select Elevation Zoom',value = 8,min = 1, max = 14,
        width = '100%')),
        className = "fieldset { border: 0;}") %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
        ns('snap_dist'), 'Snap Distance (m)',value = 1,min = 1, max = 15000,
        width = '100%')),
        className = "fieldset { border: 0;}") %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
        ns('threshold'), 'Cell Threshold',value = 1000,min = 1, max = 15000,
        width = '100%')),
        className = "fieldset { border: 0;}") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F,
                                     circleOptions = F,
                                     circleMarkerOptions = F,
                                     rectangleOptions = T,
                                     markerOptions = T,
                                     polygonOptions = T,
                                     targetGroup = 'draw',
                                     editOptions = leaflet.extras::editToolbarOptions(F, T)) %>%
      leaflet::addControl(html = shiny::actionButton(ns("deletebtn"), "remove drawn"),
                          position = 'bottomright',
                          className = 'fieldset {border:0;}') %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter"),
                                overlayGroups = c("Hydrography"))
 # })

  output$leaf_map <- leaflet::renderLeaflet({
   leaf_map
  })

    # create a counter

    vals <- shiny::reactiveValues(count = 0)

    observeEvent(input$leaf_map_draw_new_feature, {

    if(input$leaf_map_draw_new_feature$geometry$type != 'Point') {

    # make sure counter is at zero

    vals$count <- sample(0:10000, size = 1)

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

    p <- shiny::Progress$new()
    p$set(message = "Downloading data...",
          detail = "This may take a little bit...",
          value = 1/2)

    promises::future_promise({

      if(input$leaf_map_draw_new_feature$geometry$type != 'Point') {

      req(data_sf)

      ws_dem <- get_whitebox_streams(data_sf,
                                     input$map_res,
                                     threshold = input$threshold)

      } else {

      req(values$output_streams)

      ws_poly <- get_whitebox_ws(data_sf,
                                 output_streams = values$output_streams,
                                 output_pointer =  values$output_pointer,
                                 snap_dist = input$snap_dist)

      }


    }) %...>% {

      if(input$leaf_map_draw_new_feature$geometry$type != 'Point') {

      values$streams <- .[['output_streams_rast']]
      values$output_streams <- .[[2]]
      values$output_pointer <- .[[3]]
      values$output_fa <- .[[6]]

      leaflet::leafletProxy('leaf_map', session) %>%
      leaflet::addRasterImage(x = values$streams, colors = 'blue', group = paste0('raster', vals$count))

      } else {

      values$basin <- .
      samp <- sample(1:10000,size = 1, replace = T)
      values$basin <- values$basin %>% mutate(id = samp)
      values$out <- list(values$basin)
      names(values$out) <- paste0('Basin_',samp)

      values$basin_data_list <- append(values$basin_data_list, values$out)


      leaflet::leafletProxy('leaf_map', session) %>%
      leaflet::addPolygons(data = values$basin)

      }


    } %>%
      finally(~p$close())
  })

  # now for the dynamic threshold

    observeEvent(input$threshold, ignoreInit = TRUE, {

      req(values$output_fa)

      promises::future_promise({



        # extract streams based on threshold
        output_streams <- tempfile(fileext = '.tif')

        whitebox::wbt_extract_streams(values$output_fa,
                                      output_streams,
                                      threshold = input$threshold)
        streams_rast <- list(streams = terra::rast(output_streams),
                             output_streams = output_streams)

      }) %...>% {

          values$streams <- .[[1]]
          values$output_streams <- .[[2]]

          leaf_prox <- leaflet::leafletProxy('leaf_map', session) %>%
            leaflet::clearGroup(group = paste0('raster', vals$count))

          vals$count <- sample(0:10000, size = 1)

          leaf_prox %>%
            leaflet::addRasterImage(x = values$streams, colors = 'blue', group = paste0('raster', vals$count))



        }

    })




  # keep track of newly drawn shapes
  drawnshapes <- list()

  # we are fortunate here since we get an event
  #   draw_all_features
  observeEvent(
    input$leaf_map_draw_all_features,
    {
      drawnshapes <<- lapply(
        input$leaf_map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
    }
  )


  # observe our simple little button to remove
  observeEvent(
    input$deletebtn,
    {
      lapply(
        drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid=paste0(session$ns("leaf_map")), layerid=todelete)
          )
        }
      )
    }
  )


}


#' Shiny Module UI for stream network generation
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
streamnetworkModUI <- function(id, ...){
  ns <- shiny::NS(id)

  leaflet::leafletOutput(ns('leaf_map'), ...)
}


#' Shiny Module Server for stream networks
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom promises finally "%...>%"
#' @export
streamnetworkMod <- function(input, output, session, values){

  ns <- session$ns

  values$basin_data_list <- list()

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


  # create a counter

  vals <- shiny::reactiveValues(count = 0)

  #starting leaflet map
  output$leaf_map <- leaflet::renderLeaflet({

    base_map() %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
        ns('map_res'), 'Select Elevation Zoom',value = 8,min = 1, max = 14,
        width = '100%')),
        className = "fieldset { border: 0;}") %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
        ns('threshold'), 'Cell Threshold',value = 1000,min = 1, max = 15000,
        width = '100%')),
        className = "fieldset { border: 0;}") %>%
      leaflet::addControl(html = tags$div(tags$style(css),shiny::actionButton(
        ns('submit'), 'Run',
        width = '100%'))) %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                     rectangleOptions = T,
                                     markerOptions = F,
                                     polygonOptions = T, targetGroup = 'draw') %>%
      leaflet::addControl(html = shiny::actionButton(ns("deletebtn"), "remove drawn"),
                          position = 'bottomright',
                          className = 'fieldset {border:0;}') %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter"),
                                overlayGroups = c("Hydrography"))
  })

  observeEvent(input$submit, {

    req(!is.null(input$leaf_map_draw_new_feature))
    # make sure counter is at zero

    vals$count <- sample(0:10000, size = 1)

      feat <- input$leaf_map_draw_new_feature
      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)

      values$data_sf <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>%
        sf::st_as_sf()

    p <- shiny::Progress$new()
    p$set(message = "Downloading data...",
          detail = "This may take a little bit...",
          value = 1/2)

    promises::future_promise({

        ws_poly <- get_whitebox_streams(values$data_sf,
                                        input$map_res,
                                        threshold = input$threshold)

    }) %...>% {

        values$streams <- .[[5]]
        values$output_ws <- .[[4]]

        values$out <- list(watersheds = values$output_ws, streams = values$streams)

        values$basin_data_list <- append(values$basin_data_list, list(values$out))


        leaflet::leafletProxy('leaf_map', session) %>%
          leaflet::addPolygons(data = values$output_ws, fillOpacity = 0,
                               color = 'black', weight = 3, group = paste0('poly', vals$count)) %>%
          leaflet::addPolylines(data = values$streams, color = 'blue', group = paste0('raster', vals$count))


    } %>%
      finally(~p$close())

  })


  # keep track of newly drawn shapes
  drawnshapes <- list()

  # we are fortunate here since we get an event
  #   draw_all_features
  observeEvent(
    input$leaf_map_draw_all_features,
    {
      drawnshapes <<- lapply(
        input$leaf_map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
    }
  )


  # observe our simple little button to remove
  observeEvent(
    input$deletebtn,
    {
      lapply(
        drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid=paste0(session$ns("leaf_map")), layerid=todelete)
          )
        }
      )
    }
  )


}


#' Shiny Module UI for United States Geologic Survey (USGS) instantaneous values
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
usgsinstModUI <- function(id, ...){
  ns <- shiny::NS(id)

  leaflet::leafletOutput(ns('leaf_map'), ...)
}


#' Shiny Module Server for United States Geologic Survey (USGS) instantaneous values
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param values A reactive Values list to pass
#' @return server function for Shiny module
#' @importFrom promises finally "%...>%"
#' @export
usgsinstMod <- function(input, output, session, values){

  ns <- session$ns

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

  p <- shiny::Progress$new()
  p$set(message = "Setting up dashboard...",
        detail = "This may take a few seconds...",
        value = 1/2)

  promises::future_promise({
  cc <- current_conditions()

  }) %...>% {

  values$cc <- .

  values$cc_sf <- sf::st_as_sf( values$cc, coords = c('Longitude', 'Latitude')) %>%
    dplyr::mutate(time = as.POSIXct(values$cc$TimeLocal,format="%Y-%m-%dT%H:%M:%OS"))

  values$fillColor <- leaflet::colorFactor(palette = levels(unique(values$cc_sf$StatisticsStatusColorFill)),
                           domain = values$cc_sf$StatisticsStatusDescription
  )
  values$strokeColor <- leaflet::colorFactor(palette = levels(unique(values$cc_sf$StatisticsStatusColorStroke)),
                             domain = values$cc_sf$StatisticsStatusDescription
  )



  labs <- paste0('<div class="leaflet-pane leaflet-tooltip-pane">
<div class="leaflet-tooltip leaflet-zoom-animated leaflet-tooltip-left
leaflet-interactive" style="opacity: 0.9;background:#343a40;color:#fff;font-size:14px;">','<div style="text-align:center; ">',values$cc_sf$SiteName,'
            <div style="font-size:90%;"> Monitoring location USGS ',values$cc_sf$SiteNumber,
                 '</div>
            <div style="border:1px solid #ddd; margin:3px 0; padding:3px 5px;">
                Discharge, cubic feet per second
                <div style="font-size:110%;">',values$cc_sf$Value,' ft3/s @ ',format(as.POSIXct(values$cc_sf$time), format = "%H:%M:%S"),' ', values$cc_sf$TimeZoneCode,'</div>
                <div style="font-weight:bold;"></div>
            </div>
            <div style="font-size:90%;">',ifelse(!is.na(values$cc_sf$ValueFlagCode),
                                                 paste0(values$cc_sf$StatisticsStatusDescription,' :', values$cc_sf$ValueFlagCode),
                                                 paste0(values$cc_sf$StatisticsStatusDescription)),'</div>
            <div style="font-size:90%;">Rate of Change: ', values$cc_sf$RateOfChangeUnitPerHour,' ft3/s per hour</div>
        </div></div></div>')

  leaf_map <-
  base_map() %>%
    leaflet::addControl(html = tags$div(tags$style(css),shiny::numericInput(
      ns('period'), 'Days From Now',value = 30,min = 1, max = 15000,
      width = '100%'))) %>%
    leaflet::addProviderTiles(provider = 'Esri.WorldStreetMap') %>%
    leaflet::addCircleMarkers(data = values$cc_sf,
                              fillColor = ~values$fillColor(StatisticsStatusDescription),
                              color = ~values$strokeColor(StatisticsStatusDescription),
                              radius = 5,
                              weight = 2,
                              fillOpacity = 1,
                              opacity = 1,
                              label = lapply(labs, HTML),
                              layerId = ~values$cc_sf$SiteNumber
    ) %>%
    leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
    leaflet::hideGroup(group = 'Hydrography') %>%
    leaflet::addLayersControl(baseGroups = c("Esri.WorldStreetMap","Esri.WorldImagery", "CartoDB.Positron",
                                             "OpenStreetMap", "CartoDB.DarkMatter", 'OpenTopoMap'),
                              overlayGroups = c("Hydrography"))



  output$leaf_map <- leaflet::renderLeaflet({
    add_select_script(
      leaf_map,
      styleFalse = list(fillOpacity = 1, weight = 3, opacity = 1),
      styleTrue = list(fillOpacity = .25, weight = 1, opacity = .5),
      ns = session$ns(NULL)
    )
  })

  } %>%
    finally(~p$close())


id = "mapedit"
select_evt = paste0(id, "_selected")

values$df <- data.frame()
values$period <- vector()

# a container for our selections
observeEvent(input[[select_evt]],{
  # when used in modules, we get an event with blank id
  #  on initialize so also make sure we have an id
  id = as.character(input[[select_evt]]$id)
  if(nrow(values$df) == 0 && !is.null(id)) {
    values$df <- data.frame(
      id = id,
      selected = input[[select_evt]]$selected,
      stringsAsFactors = FALSE
    )
  } else {
    # see if already exists
    loc <- which(values$df$id == id)

    if(length(loc) > 0) {
      values$df[loc, "selected"] <- input[[select_evt]]$selected
    } else {
      values$df[nrow(values$df) + 1, ] <- c(id, input[[select_evt]]$selected)
    }
  }

})

observeEvent(input$period,{

  values$period <- input$period

})

}

