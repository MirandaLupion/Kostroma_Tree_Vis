library(rsconnect)
library(leaflet)
library(rgdal)
library(tidyverse)
library(readr)

# Import the CSV in
# Add an OBJECTID to merge with shapefile by 

per_cap <- read_csv("trees_per_cap_1848.csv") %>%
  mutate(OBJECTID = "")

per_cap$OBJECTID[1] <- 5
per_cap$OBJECTID[2] <- 8
per_cap$OBJECTID[3] <- 14
per_cap$OBJECTID[4] <- 13
per_cap$OBJECTID[5] <- 1
per_cap$OBJECTID[6] <- 2
per_cap$OBJECTID[7] <- 6
per_cap$OBJECTID[8] <- 4
per_cap$OBJECTID[9] <- 10
per_cap$OBJECTID[10] <- 7 
per_cap$OBJECTID[11] <- 12
per_cap$OBJECTID[12] <- 11
  
  
# Read in the shape file for the map from the files stored in the app folder
# Project the shape file in the standard WGS 84 projection. 
# I chose this projection because it is used by most GPS navigation tools and the tech, on which Leaflet is based

kos_map <- readOGR(dsn = "kos_shp", layer = "kos_shape_georef")
kos_map <- spTransform(kos_map, CRS("+init=epsg:4326"))
kos_map_per_cap <- merge(kos_map, per_cap, by = "OBJECTID", duplicateGeoms = FALSE)

nc_pal <- colorBin(palette = "Blues", bins = 5,
                       domain = kos_map_per_cap@data$per_cap_tree_desyatin)

kos_map_per_cap %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 2,
              fillOpacity = .75,
              color = ~nc_pal(per_cap_tree_desyatin),
              label = ~paste0(district, " - Forested land per capita (in desyatins): ", per_cap_tree_desyatin),
              highlight = highlightOptions(weight = 3,
                                           color = "red",
                                           bringToFront = TRUE)) %>%
  addLegend("bottomright",
            pal = nc_pal,
            values = ~per_cap_tree_desyatin,
            na.label = "NA",
            title = "Forested land per capita (in desyatins)",
            opacity = 1)
