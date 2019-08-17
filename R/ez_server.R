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

    output$select_x = shiny::renderUI({
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
                         c(names(full_data())[sapply(full_data(), is.numeric)],
                           nrow = "1"),
                         width = "100%")
    })

    output$select_group = shiny::renderUI({
      shiny::req(full_data())
      shiny::selectInput("selected_group",
                         "Select group",
                         c("[None]", names(full_data())),
                         width = "100%")
    })

    output$select_facet_x = shiny::renderUI({
      shiny::req(full_data())
      # if (!("facet_x" %in% names(as.list(args(plot_f()))))) return(NULL)
      shiny::selectInput("selected_facet_x",
                         "Select \"X\" facet value",
                         c("[None]", names(full_data())),
                         width = "100%")
    })

    output$select_facet_y = shiny::renderUI({
      shiny::req(full_data())
      # if (!("facet_y" %in% names(as.list(args(plot_f()))))) return(NULL)
      shiny::selectInput("selected_facet_y",
                         "Select \"Y\" facet value",
                         c("[None]", names(full_data())),
                         width = "100%")
    })

    plot_f = reactive({
      req(input$geom)
      utils::getFromNamespace(paste0(input$geom, "_plot"), "ezplot")
    })

    output$plot = shiny::renderPlot({
      # browser()
      shiny::req(input$selected_x,
                 input$selected_y,
                 input$selected_group,
                 input$selected_facet_x,
                 input$selected_facet_y,
                 plot_f())

      args = list(data = shiny::isolate(full_data()),
                  x = input$selected_x,
                  y = input$selected_y,
                  group = if (input$selected_group == "[None]") NULL else input$selected_group,
                  facet_x = if (input$selected_facet_x == "[None]") NULL else input$selected_facet_x,
                  facet_y = if (input$selected_facet_y == "[None]") NULL else input$selected_facet_y,
                  size = 20)

      args = args[intersect(names(args), names(as.list(args(plot_f()))))]
      do.call(plot_f(), args)
    })

    output$data_table = DT::renderDataTable({
      shiny::req(full_data())
      full_data()
    })

    observeEvent(input$done, {
      stopApp(ggplot_obj())
    })

    observeEvent(input$cancel, {
      stopApp(NULL)
    })

  }
}
