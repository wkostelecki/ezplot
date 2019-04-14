
#' ez_app
#' @param data A data frame
#' @export
#' @examples
#' \dontrun{
#' ez_app(mtcars)
#' }
ez_app = function(data) {

  ui <- shiny::fluidPage(

    shiny::column(
      width = 12,
      shiny::fluidRow(
        shiny::selectInput("x",
                           "Select x-value",
                           names(data)),
        shiny::selectInput("y",
                           "Select y-value",
                           names(data)),
        shiny::selectInput("geom",
                           "Select chart type",
                           c("line", "bar", "pie")),
        shiny::plotOutput("plot")
      )
    )

  )

  server <- function(input, output) {

    output$plot <- shiny::renderPlot({
      shiny::req(input$x, input$y, input$geom)
      plot_f = eval(parse(text = paste0("ezplot::", input$geom, "_plot")))
      plot_f(data, input$x, input$y)
    })
  }

  shiny::runGadget(app = shiny::shinyApp(ui = ui,
                                         server = server),
                   viewer = shiny::browserViewer())

}
