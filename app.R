# Load required packages
library("shiny")
library("bslib")

# Load the biological functions
source(
  file = "app_functions.R",
  local = TRUE
)

# Load the User Interface
source(
  file = "ui.R",
  local = TRUE
)

# Load the Server
source(
  file = "server.R",
  local = TRUE
)

# Launch the Shiny app
shinyApp(
  ui = ui,
  server = server
)