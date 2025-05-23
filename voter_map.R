library(ggplot2)
library(sf)
library(ggmap)
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(htmlwidgets)

#Note: this is inprogress, only used 2014 data

voters <- read_excel("~/Benzarti_voterparty_city.xlsx")
View(voters)

california_border <- states(cb = TRUE) %>%
  filter(NAME == "California")

voters <- voters %>%
  mutate(net_lean = share_dem + share_demind - share_rep + share_repind,
         leaning = case_when(net_lean > 0 ~ "Democrat", 
                             net_lean < 0 ~ "Republican", 
                             net_lean == 0 ~ "Tied"))

voters <- voters %>%
  filter(!is.na(city))

register_google(key = "AIzaSyCElSkthY2YDXK0jqBtyoHoO_QyWwa_ZO0")

voters_geo <- voters %>%
  mutate(city_full = paste0(city, ", CA")) %>%  # If all are in California
  mutate_geocode(city_full, source = "google")

View(voters_geo)


ggplot(voters_geo, aes(x = lon, y = lat)) +
  borders("state", regions = "california", fill = "gray95", color = "gray") +
  geom_point(aes(color = leaning, size = abs(net_lean)), alpha = 0.8) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red", "Tied" = "purple")) +
  theme_minimal() +
  labs(title = "City-Level Voter Lean in California",
       color = "Voter Lean",
       size = "Strength of Lean")

cities <- st_read("~/Downloads/tl_2023_06_place/tl_2023_06_place.shp")
names(cities)

cities <- cities %>%
  mutate(NAME = str_to_lower(NAME))

voters <- voters %>%
  mutate(city = str_to_lower(city))

southern_ca <- cities %>%
  filter(NAME %in% voters$city)

map_data <- southern_ca %>%
  left_join(voters, by = c("NAME" = "city"))%>%
  mutate(net_lean = net_lean*100)

View(map_data)

ggplot(map_data) +
  geom_sf(aes(fill = leaning)) +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red", "Tied" = "purple")) +
  theme_minimal() +
  labs(title = "Southern California Voter Lean by City",
       fill = "Voter Lean")

ggplot(map_data) +
  geom_sf(aes(fill = net_lean)) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Net Democratic Lean (Dem - Rep)", fill = "Net Lean")


ggplot() +
  geom_sf(data = california_border, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = map_data, aes(fill = leaning), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red", "Tied" = "purple")) +
  theme_minimal() +
  labs(title = "Southern California Voter Lean by City",
       fill = "Voter Lean") +
  coord_sf(xlim = c(-120.6, -119.6), ylim = c(34.2, 35))  # adjust these as needed


california_counties <- counties(state = "CA", cb = TRUE)

ggplot() +
  geom_sf(data = california_border, fill = NA, color = "black") +
  geom_sf(data = california_counties, fill = NA, color = "black") +
  geom_sf(data = map_data, aes(fill = net_lean)) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
                       name = "Net Democratic Lean") +
  coord_sf(xlim = c(-120.7, -119.6), ylim = c(34.2, 35.2))+
  theme_minimal()+
  labs(title = "Santa Barbara County Voter Lean by City")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank())
  )
  

#interactive map

interactive_map <- leaflet(map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%  # clean basemap
  addPolygons(
    fillColor = ~colorNumeric(palette = c("white", "blue"), domain = map_data$net_lean)(net_lean),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    label = ~paste0(NAME, ": ", round(net_lean, 2)),  # 👈 hover text
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )

saveWidget(interactive_map, file = "southern_ca_voter_map.html", selfcontained = TRUE)


