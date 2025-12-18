library(sf)
library(tidyverse)

# filter point by space and time
select_points <- function(data, mode = c("start","end")) {
  mode <- match.arg(mode)
  if (mode == "start") {
    data %>% transmute(when = start_time, lat = start_lat, lng = start_lng)
  } else {
    data %>% transmute(when = start_time, lat = end_lat, lng = end_lng)
  } %>%
    filter(is.finite(lat), is.finite(lng), !is.na(when))
}

# create od-flow lines
make_lines <- function(data) {
  g <- mapply(function(x1,y1,x2,y2) {
    st_linestring(matrix(c(x1,y1,x2,y2), ncol = 2, byrow = TRUE))
  }, data$start_lng, data$start_lat, data$end_lng, data$end_lat, SIMPLIFY = FALSE)
  st_sf(m_s = data$m_s, geometry = st_sfc(g, crs = 4326))
}