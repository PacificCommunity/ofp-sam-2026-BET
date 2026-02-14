# =============================================================================
# MFCL OUTPUT VISUALIZATION SHINY APP
# Author: Kyuhan Kim
# Description: Interactive dashboard for visualizing MFCL stock assessment outputs
# =============================================================================


if (!exists("ui") || !exists("server")) {
  source("R/00_globals.R")
  source("R/ui.R")
  source("R/server.R")
}

# RUN APPLICATION
# =============================================================================

shinyApp(ui, server)

