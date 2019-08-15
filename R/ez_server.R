#' ez_server
#' @inheritParams ez_app
#' @export
ez_server = function(data) {
  function(input, output) {

    full_data = shiny::reactive({
      shiny::req(input$selected_data)
      if (input$selected_data == "ez_app(data)") {
        output = data
      } else if (input$selected_data == "Load data ...") {
        stop("not implemented yet")
      } else {
        output = eval(parse(text = paste0("tsibbledata::", input$selected_data)))
      }
      output
    })

    output$select_x = renderUI({
      shiny::req(full_data())
      shiny::selectInput("selected_x",
                         "Select x-value",
                         c(names(full_data()), "row_number()"),
                         width = "100%")
    })

    output$select_y = shiny::renderUI({
      shiny::req(full_data())
      shiny::selectInput("selected_y",
                         "Select y-value",
                         c("1", names(full_data())[sapply(full_data(), is.numeric)]),
                         width = "100%")
    })

    output$select_group = shiny::renderUI({
      shiny::req(full_data())
      shiny::selectInput("selected_group",
                         "Select group",
                         c("<No Group>", names(full_data())),
                         width = "100%")
    })



    output$plot = shiny::renderPlot({
      shiny::req(input$selected_x, input$selected_y, input$selected_group,
                 input$geom)

      plot_f = utils::getFromNamespace(paste0(input$geom, "_plot"), "ezplot")

      args = list(data = isolate(full_data()),
                  x = input$selected_x,
                  y = input$selected_y,
                  group = if (input$selected_group == "<No Group>") NULL else input$selected_group,
                  size = 20)

      args = args[intersect(names(args), names(as.list(args(plot_f))))]

      do.call(plot_f, args)

    })

    output$data_table = DT::renderDataTable({
      shiny::req(full_data())
      full_data()
    })

  }
}
