#' Get United States Geologic Survey (USGS) Daily Flow Values Interactively
#'
#' @description This function allows the user to select United States Geologic Survey (USGS) stations
#' and get back daily flow values based on station selected. It uses the USGS
#' Water Services to get the values as well as the USGS Sites.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Get USGS Instantaneous Flow Values".
#' @param ... other arguments to \code{leafletOutput()} in module.
#' @return A data.frame that contains flow values based on the station(s) selected
#'         during shiny session.
#'
#' @note You can select multiple stations. The information from the `hover` details is not included in
#' the data.frame that is returned, e.g. rate of change, percentile description.
#'
#' @details
#'
#' **Steps**
#'
#' 1. Select the state(s) that you want to get data for.
#' 2. Select the sites you want to retrieve.
#' 3. When finished, press 'done' and sites daily flow values will be saved to
#' a data.frame in local environment.
#'
#' @export
#' @examples
#'
#' if(interactive()){
#' dv_usgs <- get_usgs_dv_interactively()
#' }
#'
#'
#'
get_usgs_dv_interactively <- function(ns = 'usgsdv-ui',
                                                 viewer = shiny::paneViewer(),
                                                 title = 'Get USGS Daily Flow Values',
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
    usgsinstModUI(ns, height = '97%'),
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
      usgsdvMod,
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

      p <- shiny::Progress$new()
      p$set(message = "Downloading daily values...",
            detail = "This may take a little bit...",
            value = 1/2)

      promises::future_promise({

        sites <- values$df[as.logical(values$df$selected),1]
        list(sites = sites)

      }) %...>% {


        suppressWarnings(values$final_dv <- get_dv(.[['sites']]))



        shiny::stopApp(

          values$final_dv

        )

      }  %>%
        finally(~p$close())

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
