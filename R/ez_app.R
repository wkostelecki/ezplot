
#' ez_app
#' @param data A data frame
#' @export
#' @examples
#' \dontrun{
#' library(tsibble)
#' library(tsibbledata)
#' ez_app(ansett)
#' }
ez_app = function(data = NULL) {

  ui = ez_ui(data)

  server = ez_server(data)

  shiny::runGadget(app = shiny::shinyApp(ui = ui,
                                         server = server),
                   viewer = shiny::browserViewer())

}

