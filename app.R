######################################
## Risk ratio app
######################################

setwd("./")

source("functions/packages.R")
source("ui/ui.R")
source("server/server.R")

shinyApp(ui, server)