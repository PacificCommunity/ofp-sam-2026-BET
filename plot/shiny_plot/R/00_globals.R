# Load required packages
library(shiny)              # Web application framework
library(shinydashboard)     # Dashboard layout
library(shinyWidgets)       # Enhanced UI widgets
library(shinyFiles)         # Directory browser
library(ggplot2)            # Data visualization
library(cowplot)            # Plot arrangement
library(dplyr)              # Data manipulation
library(FLR4MFCL)           # MFCL file reading
library(tidyr)              # Data tidying
library(viridis)            # Color palettes
library(stringr)            # String manipulation
library(purrr)              # Functional programming
library(DT)                 # Interactive tables
library(parallel)           # Parallel processing

# Fix namespace conflicts
renderDataTable <- DT::renderDataTable  # Use DT's renderDataTable
dataTableOutput <- DT::dataTableOutput  # Use DT's dataTableOutput

# Load helper functions
source("helpers.R")
source("../../tools/model_payload.R")

# Jitter diagnostics in mod_likelihood use center-adjustment helpers from tools/jitter.R
# (e.g., jitter_adjusted_center_one). Source it with a robust relative-path fallback.
jitter_tool_candidates <- c(
	"../../tools/jitter.R",
	"../tools/jitter.R",
	"tools/jitter.R"
)
jitter_tool_path <- jitter_tool_candidates[file.exists(jitter_tool_candidates)][1]
if (!is.na(jitter_tool_path) && nzchar(jitter_tool_path)) {
	source(jitter_tool_path)
} else {
	warning("Could not locate tools/jitter.R; jitter center diagnostics may be unavailable.")
}
