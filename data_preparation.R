# data preparation
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