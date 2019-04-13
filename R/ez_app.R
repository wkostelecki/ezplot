
#' ez_app
#' @param data A data frame
#' @export
#' @examples
#' ez_app(mtcars)
ez_app = function(data) {

  ui <- shiny::fluidPage(

    shiny::titlePanel("ezplot"),
    shiny::fluidRow(
      shiny::selectInput("x",
                         "Select x-value",
                         names(data)),
      shiny::selectInput("y",
                         "Select y-value",
                         names(data)),
      shiny::plotOutput("plot")
    )

  )

  server <- function(input, output) {

    output$plot <- shiny::renderPlot({
      ezplot::line_plot(data, input$x, input$y)
    })
  }

  shiny::runGadget(app = shinyApp(ui = ui,
                                  server = server),
                   viewer = shiny::browserViewer())

}
