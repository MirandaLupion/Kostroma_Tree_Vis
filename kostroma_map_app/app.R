library(shiny)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(tidyverse)
library(readr)

# Map data prep 

kos_map <- readOGR(dsn = "kos_shp", layer = "kos_shape_georef")
kos_map <- spTransform(kos_map, CRS("+init=epsg:4326"))

# data import

map_data <- read_csv("kostroma_map_app/kostroma_map_data.csv")

data_options <- c("Total area" = "total_area_desyatin_1908", 
                  "Trees per capita (1848)" = "per_cap_tree_desyatin_1848", 
                  "Forested area (1818)" = "forested_area_1818",
                  "Forest cover as a percent of total area (1818)" = "percent_forested_1818",
                  "Forested area (1908)" = "forested_area_1908",
                  "Forest cover as a percent of total area (1908)" = "percent_forested_1908",
                  "Percent change in forested area from 1818 to 1908" = "percent_change_forested_area") 

# Define UI for application that allows the users to map a given data set

ui <- fluidPage(
   
   # Application title
  
   titlePanel("Mapping Kostroma Province"),
   
   # Ability to delect the data set to map
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "map_var", 
                    label = "Indicator* to map", 
                    choices = crime_options_map,
                    selected = crime_options_map[1]),
        
        htmlOutput("define_variables_map")),
      
      # Show a plot of the generated distribution
      
      mainPanel(
        leafletOutput("map", width = "100%", height = "500px"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  map_subset <- reactive({
    req(input$year, input$map_var)
    crime_map %>%
      filter(YEAR == input$year) %>%
      select(ID_1, NAME, input$map_var) %>%
      rename(selected_var = input$map_var)}) 
    

    
  output$map <- renderLeaflet({
    kos_map_per_cap <- merge(kos_map, per_cap, by = "OBJECTID", duplicateGeoms = FALSE)
    
    nc_pal <- colorBin(palette = "Blues", bins = 5,
                       domain = kos_map_per_cap@data$selected_var)
    
    kos_map_per_cap %>%
      leaflet() %>%
      addTiles() %>%
      setView(lat = 57, lng = 62, zoom = 6) %>%
      setMaxBounds(lng1 = 55, lat1 = 48, lng2 = 68, lat2 = 60) %>%
      addPolygons(weight = 2,
                  fillOpacity = .75,
                  color = ~nc_pal(selected_var),
                  label = ~paste0(district, " - Forested land per capita (in desyatins): ", per_cap_tree_desyatin),
                  highlight = highlightOptions(weight = 3,
                                               color = "red",
                                               bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = nc_pal,
                values = ~selected_var,
                na.label = "NA",
                title = "Forested land per capita (in desyatins)",
                opacity = 1)
    
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

