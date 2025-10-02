# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(readr)
library(sf)
library(tidyverse)

df <- read_delim("data/od_flows.csv", delim = ",", show_col_types = FALSE)
if (!inherits(df$start_time, "POSIXct")) df$start_time <- as_datetime(df$start_time, tz = "UTC")
time_min <- min(df$start_time, na.rm = TRUE)
time_max <- max(df$start_time, na.rm = TRUE)

sf.lines <- df %>%
  mutate(
    geometry = pmap(list(start_lng, start_lat, end_lng, end_lat), ~ {
      st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
    })
  ) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(25833)

sf.lines$distance <- st_length(sf.lines) %>% as.numeric()
sf.lines$duration <- as.numeric(sf.lines$end_time - sf.lines$start_time, units = "secs")
sf.lines$m_s <- sf.lines$distance / sf.lines$duration

df$distance <- sf.lines$distance
df$duration <- as.numeric(df$end_time - df$start_time, units = "secs")
df$m_s <- sf.lines$m_s

select_points <- function(data, mode = c("start","end")) {
  mode <- match.arg(mode)
  if (mode == "start") {
    data %>% transmute(when = start_time, lat = start_lat, lng = start_lng)
  } else {
    data %>% transmute(when = start_time, lat = end_lat, lng = end_lng)
  } %>%
    filter(is.finite(lat), is.finite(lng), !is.na(when))
}

make_lines <- function(data) {
  g <- mapply(function(x1,y1,x2,y2) {
    st_linestring(matrix(c(x1,y1,x2,y2), ncol = 2, byrow = TRUE))
  }, data$start_lng, data$start_lat, data$end_lng, data$end_lat, SIMPLIFY = FALSE)
  st_sf(m_s = data$m_s, geometry = st_sfc(g, crs = 4326))
}

panel_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    h4(title),
    fluidRow(
      column(6, dateInput(ns("anchor_date"), "Datum", value = as_date(time_max),
                          min = as_date(time_min), max = as_date(time_max))),
      column(3, numericInput(ns("anchor_hour"), "Stunde",
                             value = hour(time_max), min = 0, max = 23, step = 1)),
      column(3, actionButton(ns("plot"), "Plot"))
    ),
    radioButtons(ns("mode"), NULL, choices = c("Startpunkte"="start","Endpunkte"="end"),
                 selected="start", inline=TRUE),
    radioButtons(ns("view"), NULL, choices = c("Heatmap"="heat","Flows (Linien, m/s-Farbe)"="flow"),
                 selected="heat", inline=TRUE),
    leafletOutput(ns("map"), height = 480),
    div(textOutput(ns("count")), style = "margin-top:6px; font-weight:600;")
  )
}

panel_server <- function(id, data, hours_r) {
  moduleServer(id, function(input, output, session) {
    anchor_posix <- reactive({
      req(input$anchor_date, input$anchor_hour)
      make_datetime(
        year  = year(input$anchor_date),
        month = month(input$anchor_date),
        day   = day(input$anchor_date),
        hour  = input$anchor_hour,
        min   = 0, sec = 0, tz = "UTC"
      )
    })
    
    df_f_cached <- eventReactive(input$plot, {

      start_t <- anchor_posix() 
      end_t <- start_t + hours(hours_r())
      data %>% filter(start_time >= start_t, start_time <= end_t)
    }, ignoreInit = TRUE)
    
    output$map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(13.7373, 51.0504, 12)
    })
    
    observe({
      req(df_f_cached())
      df_f <- df_f_cached()
      proxy <- leafletProxy("map", session = session) %>% clearHeatmap() %>% clearShapes() %>% clearControls()
      
      if (input$view == "heat") {
        p <- select_points(df_f, input$mode)
        if (nrow(p) > 0) {
          proxy %>% addHeatmap(lng = ~lng, lat = ~lat, data = p, radius = 2, blur = 0.9, max = 1)
        }
      } else {
        f <- make_lines(df_f %>% filter(is.finite(m_s)))
        if (nrow(f) > 0) {
          pal <- colorNumeric("viridis", domain = c(0, 6), na.color = "#808080")
          proxy %>% addPolylines(data = f, weight = 1, opacity = 0.05, color = ~pal(pmin(m_s, 6))) %>%
            addLegend("bottomright", pal = pal, values = c(0, 6), title = "Geschwindigkeit (m/s)")
        }
      }
    })
    
    output$count <- renderText({
      req(df_f_cached())
      paste0("Flows im Zeitfenster: ", nrow(df_f_cached()))
    })
  })
}

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
    column(6, panel_ui("a", "Panel A")),
    column(6, panel_ui("b", "Panel B"))
  )
)

server <- function(input, output, session) {
  hours_r <- reactive({ input$hours_window })
  panel_server("a", df, hours_r)
  panel_server("b", df, hours_r)
}

shinyApp(ui, server)
