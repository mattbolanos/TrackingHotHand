# ------------ #
# --- ui.R --- #
# ------------ #

# Packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(janitor)
library(shinythemes)

# nbastatR connect thing
Sys.setenv("VROOM_CONNECTION_SIZE" = 1350000)

# # Source DB connection
source("connection.R")
# Source utiliies
source("utilities.R")
# Source modules
source("modules/module_home.R")
source("modules/module_shot_charts.R")
source("modules/module_def_dist.R")
# Source functions
source("functions/plot_functions.R")
source("functions/ui_functions.R")

## UI ## 
fluidPage(
  theme = shinytheme(theme = "yeti"),
  # CSS Components to include
  tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
  navbarPage(
    title = "NBA Hot Hand Analysis",
    position = "fixed-top",
    tabPanel(
      "Home",
      htmltools::br(),
      home_ui("home_page")
    ),
    tabPanel(
      "2015-2021 Shot Charts by Streak",
      htmltools::br(),
      shot_chart_ui("shot_chart")
    ),
    tabPanel(
      "2014-2016 Streaks & Defender Distances",
      htmltools::br(),
      distance_ui("distance")
    )
  )
  

  
)
