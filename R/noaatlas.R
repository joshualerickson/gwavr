
#' Get National Oceanic and Atmospheric Administration (NOAA) Atlas 14 Interactively
#' @description This function uses the National Oceanic and Atmospheric Administration (NOAA) Atlas 14 API to allow the user to visually
#' select a location (point) to get numerous precipitation-frequency statistics.
#' @param map A background leaflet or mapview map to be used for editing. If NULL a blank mapview canvas will be provided.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "NOAA Atlas 14".
#'
#' @return A data.frame.
#' @examples
#'
#' if(interactive()){
#' noaatlas_data <- get_noaatlas_interactively()
#' }
get_noaatlas_interactively <- function(
    map = NULL,
    ns = 'noaatlas-ui',
    viewer = shiny::paneViewer(),
    title = 'NOAA Atlas 14') {
  #spherical geometry switched off
  sf::sf_use_s2(FALSE)

  ## Some code hijacked from mapedit throughout; to get miniUI look, etc

  ui = miniUI::miniPage(miniUI::miniContentPanel(
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
      map = map,
      noaatlasMod,
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
        dplyr::bind_rows(values$noaa_data_list)
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
