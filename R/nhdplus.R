



#' Get NHDPlus Interactively
#'
#' @description This function allows the user go get NHDPlus realizations interactively with a
#' shiny app.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "NHDPlus".
#' @param ... other arguments to \code{leafletOutput()} in module.
#' @return A list of sf objects that the user collected during shiny session.
#' @export
#' @note The picker list has seven options right now: NHDPlus Catchments, NHDPlus Flowlines,
#' NHDPlus Waterbodies, NHDPlus Outlet, HUC 12, HUC 8, NWIS Site.
#' @examples
#'
#' if(interactive()){
#' nhdplus_data <- get_nhdplus_interactively()
#' }
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr filter select mutate slice_max ungroup rename group_by
#' @importFrom sf st_area st_transform st_geometry st_as_sf
#' @importFrom scales comma
#' @importFrom leaflet addPolygons addPolylines addCircles
#' @importFrom shiny observeEvent reactive actionButton HTML req withProgress setProgress tags
#' @importFrom magrittr "%>%"
#'
#'
get_nhdplus_interactively <- function(ns = 'hydro-ui',
                                      viewer = shiny::paneViewer(),
                                      title = 'NHDPlus',
                                      ...) {

  #spherical geometry switched off
  sf::sf_use_s2(FALSE)

  ## Some code hijacked from mapedit throughout; to get miniUI look, etc

  ## @timelyportfolio code to remove drawn features;https://github.com/bhaskarvk/leaflet.extras/issues/96

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
                            nhdplusModUI(ns, height = '97%'),
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
        nhdplusMod,
        ns,
        values = values
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
          values$hydro_data_list
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
