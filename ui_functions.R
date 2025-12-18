panel_ui_basic <- function(id, title) {
  ns <- NS(id)
  tagList(
    h4(title),
    # first input row
    fluidRow(
      column(6, dateInput(ns("anchor_date"), "Datum", value = as_date(time_max),
                          min = as_date(time_min), max = as_date(time_max))),
      column(3, numericInput(ns("anchor_hour"), "Stunde",
                             value = hour(time_max), min = 0, max = 23, step = 1)),
      column(3, actionButton(ns("plot"), "Plot"))
    ),
    # second input row (mode selection)
    radioButtons(ns("mode"), NULL, choices = c("Startpunkte"="start","Endpunkte"="end"),
                 selected="start", inline=TRUE),
    radioButtons(ns("view"), NULL, choices = c("Heatmap"="heat","Flows (Linien, m/s-Farbe)"="flow"),
                 selected="heat", inline=TRUE),
    # actual plot
    leafletOutput(ns("map"), height = 480),
    div(textOutput(ns("count")), style = "margin-top:6px; font-weight:600;")
  )
}