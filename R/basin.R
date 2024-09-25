#' Get Watershed Basin Interactively
#'
#' @description This function allows the user to delineate watershed basins interactively with a
#' shiny app. It uses the elevatr package to acquire the Digital Elevation Model (DEM) or user inputted DEM
#' and whitebox package to delineate the basin (see details).
#' @param map a background leaflet or mapview map to be used for editing. If NULL a blank mapview canvas will be provided.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Delineate Basin".
#' @param dem A 'SpatRaster' object dem. (optional)
#' @param threshold A threshold for stream initiation. 1000 (default).
#' @param ... other arguments to \code{leafletOutput()} in module and/or wbt_* functions.
#' @return A sf object that contains watershed polygons
#'         the user collected during shiny session.
#'
#' @note The marker will only work for the most current stream raster. You can have multiple areas but
#' you need to make sure that you are on the most current raster when selecting basins or the app will crash. If
#' you add your own DEM then you don't need to draw a bounding box.
#'
#' @details
#' **This function will throw an error if you don't draw the bounding box (rectangle) first and you didn't include your own DEM.**
#' Once the user has drawn the bounding box (or added own DEM) then you can use the marker as a pour point location.
#'
#' **Steps**
#'
#' 1. Input a well-suited DEM zoom level and threshold. (skip if own DEM is inputted)
#' 2. Draw bounding box (rectangle or polygon) (skip if own DEM is inputted).
#' 3. Use marker to place pour point(s).
#' 4. If necessary, change 'Cell Threshold' to change drainage density.
#' 5. Repeat steps 1-4 if needed.
#' 6. When finished, press 'done' and basins will be saved as a list in local environment.
#'
#' In addition, this function uses both `whitebox::wbt_feature_preserving_smoothing()` and `whitebox::wbt_breach_depressions()`
#' prior to running the flow direction and flow accumulation (both d8) algorithms.
#' @export
#' @examples
#'
#' if(interactive()){
#' basin_data <- get_basin_interactively()
#' }
#'
#'
get_basin_interactively <- function(map = NULL,
                                    ns = 'basin-ui',
                                    viewer = shiny::paneViewer(),
                                    title = 'Delineate Basin',
                                    dem = NULL,
                                    threshold = 1000,
                                    ...) {

  ## Some code hijacked from mapedit throughout; to get miniUI look, etc

  ui = miniUI::miniPage(tags$script(HTML(
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
  )),miniUI::miniContentPanel(
    basinModUI(ns, height = '97%'),
    height=NULL, width=NULL
  ),
  miniUI::gadgetTitleBar(
    title = title,
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

    values <- reactiveValues()

    crud_mod <- reactive(shiny::callModule(
      map = map,
      basinMod,
      ns,
      values = values,
      dem = dem,
      threshold = threshold
    ))

    observe({crud_mod()})


    sessionEnded <- session$onSessionEnded(function() {
      # should this be a cancel where we send NULL
      #  or a done where we send crud()
      shiny::stopApp(shiny::isolate(crud_mod()))
    })


    #used to stop the app via button and retain selections
    observeEvent(input$done, {

      shiny::stopApp(
        values$basin_data %>%
        dplyr::select(geometry, id)  %>%
        sf::st_make_valid() %>%
        dplyr::mutate(area_acres = as.numeric(units::set_units(sf::st_area(.), 'acres')),
                      area_miles = as.numeric(units::set_units(sf::st_area(.), 'mi^2')),
                      area_hectares = as.numeric(units::set_units(sf::st_area(.), 'ha')),
                      area_km = as.numeric(units::set_units(sf::st_area(.), 'km^2')))%>%
          dplyr::relocate(-geometry,.before = geometry)
      )

    })

    # stops app but doesn't keep selections
    shiny::observeEvent(input$cancel, {
      shiny::stopApp (NULL)
      sessionEnded()
    })


  }

  shiny::runGadget(
    ui,
    server,
    viewer =  viewer,
    stopOnCancel = FALSE
  )
}
