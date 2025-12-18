panel_server_basic <- function(id, data, hours_r) {
  moduleServer(id, function(input, output, session) {
    # get time from panel date and panel hour
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
      # calculate range based on top slider
      start_t <- anchor_posix() 
      end_t <- start_t + hours(hours_r())
      data %>% filter(start_time >= start_t, start_time <= end_t)
    }, ignoreInit = TRUE)
    
    # set map section
    output$map <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(13.7373, 51.0504, 12)
    })
    
    
    observe({
      req(df_f_cached())
      df_f <- df_f_cached()
      proxy <- leafletProxy("map", session = session) %>%
        clearHeatmap() %>%
        clearShapes() %>%
        clearControls()
      # heatmap
      if (input$view == "heat") {
        p <- select_points(df_f, input$mode)
        if (nrow(p) > 0) {
          proxy %>% addHeatmap(lng = ~lng, lat = ~lat, data = p, radius = 2, blur = 0.9, max = 1)
        }
      # od-flows
      } else {
        f <- make_lines(df_f %>% filter(is.finite(m_s)))
        if (nrow(f) > 0) {
          pal <- colorNumeric("viridis", domain = c(0, 6), na.color = "#808080")
          proxy %>%
            addPolylines(data = f, weight = 1, opacity = 0.05, color = ~pal(pmin(m_s, 6))) %>%
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