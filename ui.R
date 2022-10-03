### ------------ ###
### --- ui.R --- ###
### ------------ ###

# Packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(janitor)
library(shinythemes)

# nbastatR connect thing
Sys.setenv("VROOM_CONNECTION_SIZE" = 1350000)

# Source DB connetion
source("connection.R")
# Source functions
source("utilities.R")
# Source modules
source("modules/module_shot_charts.R")
source("modules/module_home.R")

## UI ## 
fluidPage(
  theme = shinytheme(theme = "yeti"),
  title = "NBA Hot Hand Analysis",
  # CSS Components to include
  tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
  navbarPage(
    "NBA Hot Hand Analysis",
    position = "fixed-top",
    tabPanel(
      "Home",
      htmltools::br(),
      home_ui("home_page")
    ),
    tabPanel(
      "2015-2021 Shot Charts by Streak",
      shot_chart_ui("shot_chart")
    )
  )
  
  # navbarPage(
  #   position = "fixed-top",
  #   tabPanel(
  #     id = "home_page_id",
  #     # Tab: Home
  #     "Home",
  #     home_ui(id = "home_page")
  #   ),
  #   tabPanel(
  #     "Home2"
  #   )
  # )
  # tabPanel(
  #   id = "shot_chart_id",
  #   # Tab: Player Rankings
  #   "2015-21 Streak Shooting by Zone",
  #   shot_chart_ui("shot_chart")
  # )
  # tabPanel(
  #   id = "player_prof",
  #   # Tab: Player Profile
  #   "Player Profile",
  #   player_profile_UI("player_profile")
  # )
  
  
  # Table Defender Distance Tab UI ---------------------------------------------------------------------------------------------------
  
  # tabPanel("Streak Shooting and Defender Distance: 2014-16",
  #          strong("2014-16 Streak Shooting With Defender Distance",style = "font-size:30px;font-family:Georgia; color:black"),
  #          br(),
  #          strong("Version: 2021-08-03",style = "font-size:16px;font-family:Georgia; color:red"),
  #          strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com | ",a("Spinner", href = "https://i.pinimg.com/originals/27/3d/59/273d598e4856924672bf1a0ce62f0d54.gif", target =
  #                                                                                "_blank"),style = "font-size:14px;font-family:Georgia; color:black"),
  #          br(),
  #          strong("Glossary",style = "font-size:23px;font-family:Georgia; color:black"),
  #          p("% Total FGA Tight - Percent of Total FGA that Closeset Defender Distance is Tight or Very Tight",style = "font-size:14px;font-family:Arial; color:black"),
  #          p("Streak eFG% - The effective FG% of the player when on the selected streak (e.g. a player's eFG% after 3+ consecutive makes)",style = "font-size:14px;font-family:Arial; color:black"),
  #          p("Streak Open eFG% - The effective FG% of the player when on the selected streak AND closest defender distance is either Open or Wide Open",style = "font-size:14px;font-family:Arial; color:black"),
  #          p("Streak Tight eFG% - The effective FG% of the player when on the selected streak AND closest defender distance is either Tight or Very Tight",style = "font-size:14px;font-family:Arial; color:black"),
  #          p("Coverage Diff - Streak Tight eFG% minus Streak Open eFG%", style = "font-size:14px;font-family:Arial; color:black"),
  #          strong("**Select Consecutive Makes/Misses**",style = "font-size:26px;font-family:Georgia; color:black"),
  #          mainPanel(pickerInput('streak2',
  #                                choices = c("1 Make", "2 Makes", "3+ Makes", 
  #                                            "1 Miss", "2 Misses", "3+ Misses"),
  #                                selected = "2 Makes", width = '50%'),
  #                    br(),
  #                    withSpinner(DT::dataTableOutput("table2"),
  #                                image = "loading.gif",
  #                                image.width = "100px",
  #                                image.height = "100px"),
  #                    width = 15))
  
)
