#' Get Stream Network Interactively
#'
#' @description This function allows the user to get stream networks and watersheds interactively with a
#' shiny app. It uses the {elevatr} package to acquire the Digital Elevation Model (DEM) or user inputted DEM
#' and {whitebox} package to delineate the stream network and watersheds (see details).
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
#' @param ... other arguments to \code{leafletOutput()} in module.
#' @param dem A terra or raster DEM object if you want to add.
#' @note If you add your own DEM then you don't need to draw a bounding box.
#' @details This function uses the package \link{elevatr} to download the DEM (unless you provide your own).
#' Once the user has drawn the bounding box or inputed DEM and selected appropriate zoom (resolution) and threshold then
#' the app will create basins and streams.
#'
#' **Steps**
#'
#' 1. Input a well-suited DEM zoom level (or your own DEM) and stream threshold (resolution).
#' 2. Draw bounding box (rectangle or polygon) (skip if own DEM is inputted).
#' 3. Wait for layers to respond.
#' 4. when finished, press 'done' and stream network and watersheds will be saved as a list in local environment.
#'
#' In addition, this function uses both `whitebox::wbt_feature_preserving_smoothing()` and `whitebox::wbt_breach_depressions()`
#' prior to running the flow direction and flow accumulation (both d8) algorithms.
#'
#'
#'
#' @return A list of sf objects that the user collected during shiny session.
#' Each list will contain two sf objects: `watersheds` and `streams`. The `streams` object will also return these attributes:
#' `tribid`, `strahler`, `slope`, `length`, `mainstem`, `FID`, `STRM_VAL`.
#' @export
#' @examples
#'
#' if(interactive()){
#' streamnetwork <- get_stream_network_interactively()
#' }
#'
#'
#'
get_stream_network_interactively <- function(ns = 'streamnetwork-ui',
                                    viewer = shiny::paneViewer(),
                                    title = 'Streamnetwork',
                                    dem = NULL,
                                    ...) {

  #spherical geometry switched off
  sf::sf_use_s2(FALSE)

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
    streamnetworkModUI(ns, height = '97%'),
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
      streamnetworkMod,
      ns,
      values = values,
      dem = dem
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
        values$basin_data_list
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
