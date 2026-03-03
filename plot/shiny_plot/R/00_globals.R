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
