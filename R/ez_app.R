
#' ez_app
#' @param data A data frame
#' @export
#' @examples
#' \dontrun{
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
                  group = if (input$group == "Select group") NULL else input$group,
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
  miniUI::miniPage(
    miniUI::gadgetTitleBar("ezplot"),
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(1,3),
        shiny::fillCol(
          shiny::selectInput(
            "data_name",
            "Select data",
            choices = c("ansett", "aus_livestock", "aus_production", "aus_retail",
                        "gafa_stock", "global_economy", "hh_budget", "nyc_bikes",
                        "olympic_running", "PBS", "pelt", "vic_elec"),
            width = "100%"
          ),
          shiny::selectInput("geom",
                             "Select chart type",
                             c("line", "bar", "area", "pie", "waterfall"),
                             width = "100%"),
          shiny::selectInput("x",
                             "Select x-value",
                             c(names(data), "row_number()"),
                             width = "100%"),
          shiny::selectInput("y",
                             "Select y-value",
                             c("1", names(data)[sapply(data, is.numeric)]),
                             width = "100%"),
          shiny::selectInput("group",
                             "Select group",
                             c("Select group", names(data)),
                             width = "100%")
        ),
        shiny::fillCol(shiny::plotOutput("plot", height = "100%"))
      )
    )
  )
}
