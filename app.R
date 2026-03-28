source("ui.R")      # Load the UI definition
source("server.R")   # Load the server logic
source("global.R")   # Load global definitions and modules
# Run the application
shinyApp(ui = ui, server = server)