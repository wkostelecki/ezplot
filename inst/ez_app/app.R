library(ezplot)

data = tsibbledata::ansett

ui = ez_ui(data)

server = ez_server(data)

shinyApp(ui = ui,
         server = server)
