



#' Get NHDPlus Interactively
#'
#' @description This function allows the user go get NHDPlus realizations interactively with a
#' shiny app.
#' @return A list of sf objects that the user collected during shiny session.
#' @export
#' @note The picker list has seven options right now: NHDPlus Catchments, NHDPlus Flowlines,
#' NHDPlus Waterbodies, NHDPlus Outlet, HUC 12, HUC 8, NWIS Site.
#' @examples \dontrun{
#'
#'
#' nhdplus_data <- get_nhdplus_interactively()
#'
#'
#'
#' }
#' #' @importFrom shiny NS tagList
#' @importFrom dplyr filter select mutate slice_max ungroup rename group_by
#' @importFrom sf st_area st_transform st_geometry st_as_sf
#' @importFrom scales comma
#' @importFrom leaflet addPolygons addPolylines addCircles
#' @importFrom shiny observeEvent reactive actionButton HTML req withProgress setProgress tags
#' @importFrom magrittr "%>%"
#'
#'
get_nhdplus_interactively <- function() {

  #spherical geometry switched off
  sf::sf_use_s2(FALSE)

  ## Some code hijacked from mapedit throughout; to get miniUI look, etc

  ## @timelyportfolio code to remove drawn features;https://github.com/bhaskarvk/leaflet.extras/issues/96
  scr <- tags$script(HTML(
    "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
  ))

    ui = miniUI::miniPage(scr,miniUI::miniContentPanel(leaflet::leafletOutput('leaf_map', height = '97%'),
                                                   height=NULL, width=NULL),
                          miniUI::gadgetTitleBar(title = '',
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    tags$script(HTML(
      "
// close browser window on session end
$(document).on('shiny:disconnected', function() {
  // check to make sure that button was pressed
  //  to avoid websocket disconnect caused by some other reason than close
  if(
    Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
    Shiny.shinyapp.$inputValues['done:shiny.action']
  ) {
    window.close()
  }
})
"
    ))
    )


    server = function(input, output, session) {

      #create reactiveValues() to hold data created
      values = shiny::reactiveValues()

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
            'location_map', 'Select data type',
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
          leaflet::addControl(html = shiny::actionButton("deletebtn", "remove drawn"),
                              position = 'bottomleft',
                              className = 'fieldset {border:0;}') %>%
          leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
          leaflet::hideGroup(group = 'Hydrography') %>%
          leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                                   "OpenStreetMap", "CartoDB.DarkMatter"),
                                    overlayGroups = c("Hydrography"))





      })

      values$hydro_data_list <- list()

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

              data_sf <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf()

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

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_huc12(data_sf))},
                                            error = function(e) {
                                              'error'
                                            })

              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No HUC 12 Features Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                leaflet::addPolygons(data = values$hydro_data()%>%
                                       sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                "<b>HUC Name: </b>",values$hydro_data()$name,
                                                                                "<br>", "<b>HUC #: </b>", values$hydro_data()$huc12,
                                                                                "<br>", "<b>HUC Area: </b>",scales::comma(round(values$hydro_data()$areaacres,0)), " acres" ) )
            } else if (input$location_map == "huc8") {

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_huc8(data_sf))},
                                            error = function(e) {
                                              'error'
                                            })
              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No HUC 8 Features Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                leaflet::addPolygons(data = values$hydro_data()%>%
                                       sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                "<b>HUC Name: </b>",values$hydro_data()$name,
                                                                                "<br>", "<b>HUC #: </b>", values$hydro_data()$huc8,
                                                                                "<br>", "<b>HUC Area: </b>",scales::comma(round(values$hydro_data()$areaacres,0)), " acres" ) )

            } else if (input$location_map == "catchment") {

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_nhdplus(data_sf, realization = 'catchment'))},
                error = function(e){
                  'error'
                }
                )
              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No Catchments Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                addPolygons(data = values$hydro_data() %>%
                              sf::st_make_valid() %>%
                              sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326") , popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                       "<b>Feature ID: </b>",values$hydro_data()$featureid,
                                                                       "<br>", "<b>Area: </b>",scales::comma(round(values$hydro_data()$areasqkm,0)*247.105), " acres" ) )

            } else if (input$location_map == "nhdplus") {

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_nhdplus(data_sf))},
                                            error = function(e){
                                              'error'
                                            })

              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No Flowlines Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                addPolylines(data = values$hydro_data()%>%
                               sf::st_make_valid() %>%
                               st_as_sf() %>%
                               sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326") , popup = paste0("<p style=line-height:20px;margin:0px;>",
                                                                        "<b>Name: </b>",values$hydro_data()$gnis_name,
                                                                        "<br>", "<b>Stream Order (strahler): </b>", values$hydro_data()$streamorde,
                                                                        "<br>", "<b>Arbolate Sum: </b>", paste0(values$hydro_data()$arbolatesu, " mi"),
                                                                        "<br>", "<b>Drainage Area: </b>", paste0(comma(round(values$hydro_data()$totdasqkm*247.105, 0)), ' acres'),
                                                                        "<br>", "<b>Mean Annual Flow (MAF): </b>", paste0(values$hydro_data()$qa_ma, ' cfs'),
                                                                        "<br>", "<b>MAF with Ref. Gage (MAFqc): </b>", paste0(values$hydro_data()$qc_ma, ' cfs'),
                                                                        "<br>", "<b>Mean Flow (April, May, June): </b>", paste0(values$hydro_data()$qa_04, ' , ', values$hydro_data()$qa_05, ' , ', values$hydro_data()$qa_06 ),
                                                                        "<br>", "<b>Slope: </b>", paste0(round(values$hydro_data()$slope*100,2), " %"),
                                                                        "<br>", "<b>Length: </b>", paste0(round(values$hydro_data()$lengthkm*0.621371,2), " mi"),
                                                                        "<br>", "<b>Path Length (terminal): </b>", paste0(comma(round(values$hydro_data()$pathlength,0)), " mi" ),
                                                                        "<br>", "<b>Mean Annual Time of Travel: </b>", paste0(round(values$hydro_data()$totma,3), " days" ),
                                                                        "<br>", "<b>COMID #: </b>", values$hydro_data()$comid,
                                                                        "<br>", "<b>F-type: </b>",values$hydro_data()$ftype,
                                                                        "<br>", "<b>F-code: </b>",values$hydro_data()$fcode))

            } else if (input$location_map == "outlet") {

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_nhdplus(data_sf, realization = 'outlet'))},
            error = function(e){
              'error'
            })

              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No Outlets Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                addCircles(data = values$hydro_data(), weight = 15, popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                                   "<b>Name: </b>",values$hydro_data()$gnis_name,
                                                                                   "<br>", "<b>Path Length (terminal): </b>", paste0(comma(round(values$hydro_data()$pathlength,0)), " mi" ),
                                                                                   "<br>", "<b>Mean Annual Time of Travel: </b>", paste0(round(values$hydro_data()$totma,4), " days" ),
                                                                                   "<br>", "<b>COMID #: </b>", values$hydro_data()$comid,
                                                                                   "<br>", "<b>F-type: </b>",values$hydro_data()$ftype))

            } else if (input$location_map == "nwis") {

              values$hydro_data <-  tryCatch({reactive(nhdplusTools::get_nwis(data_sf))},
                                             error = function(e){
                                               'error'
                                             })

              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No NWIS Sites Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                addCircles(data = values$hydro_data() %>%
                             st_as_sf() %>%
                             st_transform(crs = 4326,proj4string = "+init=epsg:4326"),radius = 15, weight = 15,
                           color = "red", popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                         "<b>Name: </b>",values$hydro_data()$station_nm,
                                                         "<br>", "<b>Site #: </b>", values$hydro_data()$site_no))

            } else if (input$location_map == 'waterbody'){

              values$hydro_data <- tryCatch({reactive(nhdplusTools::get_waterbodies(data_sf))},
                                            error = function(e){
                                              'error'
                                            })

              if(class(values$hydro_data())[[1]] != 'sf'){

                shinyWidgets::show_alert('No Waterbodies  Found',
                                         'please try a new area',
                                         type = 'warning')}

              req(class(values$hydro_data())[[1]] == 'sf')

              leaflet::leafletProxy("leaf_map", session) %>%
                addPolygons(data = values$hydro_data() %>%
                              sf::st_make_valid() %>%
                              sf::st_as_sf() %>%
                              sf::st_transform(crs = 4326,proj4string = "+init=epsg:4326"), popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                                       "<b>Name: </b>",values$hydro_data()$gnis_name,
                                                                       "<br>", "<b>Area: </b>",paste(comma(round(values$hydro_data()$areasqkm*247.105,0)), " acres" ),
                                                                       "<br>", "<b>F-type: </b>",values$hydro_data()$ftype,
                                                                       "<br>", "<b>F-code: </b>",values$hydro_data()$fcode,
                                                                       "<br>", "<b>Mean Depth: </b>",paste(round(values$hydro_data()$meandepth*3.28084, 2), " ft"),
                                                                       "<br>", "<b>Lake Volume: </b>",paste(comma(round(values$hydro_data()$lakevolume*35.3147,2)), " cf"),
                                                                       "<br>", "<b>Max Depth: </b>", paste(round(values$hydro_data()$maxdepth*3.28084, 2), " ft")))

            }
          })

        out <- list(values$hydro_data())
        names(out) <- paste0(input$location_map, '_',sample(1:10000,size = 1, replace = T))

        values$hydro_data_list <- append(values$hydro_data_list, out)


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
                list(elid="leaf_map", layerid=todelete)
              )
            }
          )
        }
      )
      #used to stop the app via button and retain selections
      observeEvent(input$done, {

        shiny::stopApp(
          values$hydro_data_list
          )

      })

      # stops app but doesn't keep selections
      shiny::observeEvent(input$cancel, {
        shiny::stopApp (NULL)
      })
    }

  return(shiny::runApp(shiny::shinyApp(ui,server),launch.browser = shiny::paneViewer()))
}
