
#' ez_app
#' @param data A data frame
#' @export
#' @examples
#' \dontrun{
#' ez_app(mtcars)
#' ez_app(airquality)
#' ez_app(ez_data())
#' }
ez_app = function(data) {

  ui = ez_ui(data)

  server = ez_server(data)

  shiny::runGadget(app = shiny::shinyApp(ui = ui,
                                         server = server),
                   viewer = shiny::browserViewer())

}

#' ez_server
#' @inheritParams ez_app
#' @export
ez_server = function(data) {
  function(input, output) {

    output$plot = shiny::renderPlot({
      shiny::req(input$x, input$y, input$group, input$geom)
      plot_f = utils::getFromNamespace(paste0(input$geom, "_plot"), "ezplot")

      args = list(data = data,
                  x = input$x,
                  y = input$y,
                  group = input$group,
                  size = 20)

      args = args[intersect(names(args), names(as.list(args(plot_f))))]

      do.call(plot_f, args)

    })
  }
}

#' ez_ui
#' @inheritParams ez_app
#' @export
ez_ui = function(data) {
  shiny::fluidPage(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::selectInput("x",
                           "Select x-value",
                           c(names(data), "row_number()"))
      ),
      shiny::column(
        width = 3,
        shiny::selectInput("y",
                           "Select y-value",
                           c("1", names(data)))
      ),
      shiny::column(
        width = 3,
        shiny::selectInput("group",
                           "Select group",
                           names(data))
      ),
      shiny::column(
        width = 3,
        shiny::selectInput("geom",
                           "Select chart type",
                           c("line", "bar", "area", "pie", "waterfall"))
      )
    ),
    shiny::fluidRow(shiny::plotOutput("plot"))
  )
}
