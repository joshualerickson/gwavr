
#' Get NLDI Interactively
#' @description This function uses the NLDI API to allow the user to visually
#' select a location (point) to get numerous hydrologic realizations.
#' @return A list with sf objects.
#' @export
#'
#' @note The picker list has three options right now: Total Basin, All Local Catchments and Only Local Catchment.
#' Descriptions below:
#' \itemize{
#' \item  \strong{Total Basin}: This will return the upstream tributaries (UT), upstream main (UM),
#' basin boundary and site data above the user point.
#' \item  \strong{All Local Catchments}: This will return the upstream tributaries (UT) and all the
#' local NHDPLusV2 catchments above the user point. In addition, each catchment will contain the
#' zonal stats associated with 'CAT' in NLDI.
#' \item \strong{Only Local Catchment}: This will only return the catchment at the point and tributary.
#' In addition, it will also include the zonal stat for that catchment.
#' }
#'
#' @importFrom utils data
#'
#'
#' @examples \dontrun{
#'
#' nldi_data <- get_nldi_interactively() #select by point with 'All Local Catchments'
#'
#' #then select 'done'
#'
#' #load some extra libraries
#' library(sf)
#' library(ggplot)
#'
#' #now plot
#' ggplot() + geom_sf(data = nldi_data[[1]][[2]])
#'
#' #or view different zonal stats
#'
#' nldi_data[[1]][[2]] %>%
#' ggplot() + geom_sf(aes(fill = CAT_PPT7100_ANN))
#'
#'
#'
#' }
get_nldi_interactively <- function() {

  #spherical geometry switched off
  sf::sf_use_s2(FALSE)

  ## Some code hijacked from mapedit throughout; to get miniUI look, etc

  ## @timelyportfolio code to remove drawn features; https://github.com/bhaskarvk/leaflet.extras/issues/96
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

ui = miniUI::miniPage(scr, miniUI::miniContentPanel(leaflet::leafletOutput('leaf_map', height = '97%'),
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

server = function(input, output, session){

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
        'location_map', 'Select a NLDI option',
        choices = c("", c(`Total Basin` = 'tot',
                          `All Local Catchments` = 'catch',
                          `Only Local Catchment` = 'local')),
        options = shinyWidgets::pickerOptions(container = 'body'),
        width = '80%',
        choicesOpt = list(
          style = rep(("font-weight: bold;font-family: 'Montserrat', sans-serif;"),51)))),
    className = "fieldset { border: 0;}") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                     rectangleOptions = F,
                                     markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = F),
                                     polygonOptions = F) %>%
    leaflet::addControl(html = shiny::actionButton("deletebtn", "remove drawn"),
                        position = 'bottomleft',
                        className = 'fieldset {border:0;}') %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("OpenTopoMap","Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter"),
                                overlayGroups = c("Hydrography"))



  })

  values$nldi_data_list <- list()

  #leaflet proxy to update map with stations

  observeEvent(input$leaf_map_draw_new_feature, {
    req(nchar(input$location_map)>1)
    withProgress(

      message = 'Downloading NLDI',
      detail = 'This may take a few seconds...', value = 0,{



        click <- input$leaf_map_draw_new_feature
        clat <- click$geometry$coordinates[[2]]
        clng <- click$geometry$coordinates[[1]]

        data_sf <- tidyr::tibble(Lat = clat, Lon = clng)

        data_sf <- data_sf %>% sf::st_as_sf(coords = c('Lon', 'Lat')) %>%
          sf::st_set_crs(4326) %>%
          sf::st_transform(crs = 4326)



        setProgress(1/2)
        if(input$location_map %in% 'tot'){
          values$nldi_data <- reactive(get_NLDI(data_sf))

          map_nldi <- leaflet::leafletProxy("leaf_map", session) %>%
            leaflet::addPolygons(
              data=values$nldi_data()$basin_boundary,
              color = "black",
              fillOpacity = 0,
              weight = 3,
              opacity = 1, popup = paste0("<b>","Drainage Area (DA): ","</b>", round(units::set_units(sf::st_area(values$nldi_data()$basin_boundary), mi^2), 1), " Sq.Miles",
                                          "<br>","<b>", "DA acres: ", "</b>", scales::comma(as.numeric(round(units::set_units(sf::st_area(values$nldi_data()$basin_boundary), acres), 1)),1), " Acres",
                                          "<br>", "<b>", "Length of Main Stem: ", "</b>", round(sum(units::set_units(sf::st_length(values$nldi_data()$UM), mi)), 1), " Miles",
                                          "<br>", "<b>", "Total length of Tribs: ", "</b>", round(sum(units::set_units(sf::st_length(values$nldi_data()$UT), mi)), 1), " Miles",
                                          "<br>", "<b>", "DA/Length: ", "</b>", round((sum(units::set_units(sf::st_length(values$nldi_data()$UM), mi))+sum(units::set_units(sf::st_length(values$nldi_data()$UT), mi)))/(units::set_units(st_area(values$nldi_data()$basin_boundary), mi^2)),1), " Miles"))



          map_nldi <- leaflet::addPolylines(map_nldi,
                                            data = values$nldi_data()$UT,
                                            color = "blue",
                                            weight = 3,
                                            opacity = 1)

          map_nldi <- leaflet::addPolylines(map_nldi,
                                            data = values$nldi_data()$UM,
                                            color = "red",
                                            weight = 4,
                                            opacity = 0.5)
        } else if (input$location_map == 'catch') {

          values$nldi_data <- reactive(get_NLDI_catchments(data_sf))

          map_nldi <- leaflet::leafletProxy("leaf_map", session) %>%
            leaflet::addPolygons(
              data=values$nldi_data()[[2]],
              color = "black",
              fillOpacity = 0,
              weight = 3,
              opacity = 1)

          map_nldi <- leaflet::addPolylines(map_nldi,
                                            data = values$nldi_data()[[1]],
                                            color = "blue",
                                            weight = 3,
                                            opacity = 1)
        }else if (input$location_map == 'local') {

          values$nldi_data <- reactive(get_NLDI_catchments(data_sf,method = 'local'))

          map_nldi <- leaflet::leafletProxy("leaf_map", session) %>%
            leaflet::addPolygons(
              data=values$nldi_data()[[2]],
              color = "black",
              fillOpacity = 0,
              weight = 3,
              opacity = 1)

          map_nldi <- leaflet::addPolylines(map_nldi,
                                            data = values$nldi_data()[[1]],
                                            color = "blue",
                                            weight = 3,
                                            opacity = 1)
        }

      })

    values$nldi_data_list <- append(values$nldi_data_list, list(values$nldi_data()))
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
    values$nldi_data_list
  )

})

# stops app but doesn't keep selections
shiny::observeEvent(input$cancel, {
  shiny::stopApp (NULL)
})

}
return(shiny::runApp(shiny::shinyApp(ui,server),launch.browser = shiny::paneViewer()))
}


