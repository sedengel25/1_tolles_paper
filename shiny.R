# app.R
library(shiny)
#library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
#library(readr)
library(sf)
library(tidyverse)

# data preparation
source("data_preparation.R")

# filter point by space and time, create od-flow lines
source("helper_functions.R")

# basic panel ui
source("ui_functions.R")

# basic panel logic
source("server_function.R")

# ui
ui <- fluidPage(
  titlePanel("OD-Heatmaps & Flows"),
  fluidRow(
    column(
      12,
      sliderInput("hours_window", "Zeitfenster (Startzeitpunkt + Stunden):",
                  min = 1, max = 100, value = 2, step = 1, width = "100%")
    )
  ),
  fluidRow(
    column(6, panel_ui_basic("a", "Panel A")),
    column(6, panel_ui_basic("b", "Panel B"))
  )
)
# server
server <- function(input, output, session) {
  hours_r <- reactive({ input$hours_window })
  panel_server_basic("a", df, hours_r)
  panel_server_basic("b", df, hours_r)
}

shinyApp(ui, server)
