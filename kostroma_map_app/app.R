library(shiny)
library(rsconnect)
library(leaflet)
library(rgdal)
library(shinythemes)
library(tidyverse)
library(readr)

# Shapefile data prep 

kos_map <- readOGR(dsn = "kos_shp", layer = "kos_shape_georef")
kos_map <- spTransform(kos_map, CRS("+init=epsg:4326"))

rivers <- readOGR(dsn ="kos_rivers_shp", layer ="kos_rivers_clip")
rivers <- spTransform(rivers, CRS("+proj=longlat +init=epsg:4326"))

# Raster prep - need to decrease the size to add the layer

ras <- raster::raster("raster_file/kos_his_georef.tif") %>%
  raster::aggregate(fact = 2, fun = mean)

# Data import

map_data <- read_csv("kostroma_map_data.csv")

# Create vector of neat names for labeling and the UI

data_options <- c("Total area" = "total_area_desyatin_1908", 
                  "Forested area per capita" = "per_cap_tree_desyatin_1848", 
                  "Forested area" = "forested_area_1818",
                  "Forest cover as a percent of total area" = "percent_forested_1818",
                  "Forested area" = "forested_area_1908",
                  "Forest cover as a percent of total area" = "percent_forested_1908",
                  "Percent change in forested area from 1818 to 1908" = "percent_change_forested_area") 


# Create a vector of data type labels to add to the 

label_options <- c(" desyatins" = "total_area_desyatin_1908", 
                  " desyatins" = "per_cap_tree_desyatin_1848", 
                  " desyatins" = "forested_area_1818",
                  "%" = "percent_forested_1818",
                  " desyatins" = "forested_area_1908",
                  "%" = "percent_forested_1908",
                  "%" = "percent_change_forested_area") 


# Define UI for application that allows the users to map a given data set

ui <- fluidPage(theme = shinytheme("paper"),
   
   # Application title
  
   titlePanel("Mapping Tree Cover in Kostroma Province"),
   
   # Ability to delect the data set to map
   
   sidebarLayout(
      sidebarPanel(
        
         selectInput(inputId = "year",
                    label = "Select a year to view possible data sets",
                    choices = c(1818, 1848, 1908),
                    selected = NULL),


         uiOutput("ui"),
         
         checkboxInput(inputId = "box",
                       label = "Add historical base map (1822)",
                       value = FALSE)),
      
      mainPanel(
        htmlOutput("title"),
        leafletOutput("map", width = "100%", height = "500px"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$ui <- renderUI({
    if (is.null(input$year))
      return()
    switch(input$year,
           "1818" = selectInput(inputId = "map_var",
                                label = "Choose a data set to map",
                                choices = data_options[3:4],
                                selected = data_options[3]),
            "1848" =  selectInput(inputId = "map_var",
                                  label = "Choose a data set to map",
                                  choices = data_options[2],
                                  selected = data_options[2]),
            "1908" =  selectInput(inputId = "map_var",
                                  label = "Choose a data set to map",
                                   choices = data_options[5:7],
                                    selected = data_options[5]))
  })  
  
  map_subset <- reactive({
    req(input$year, input$map_var)
    map_data %>%
      select(OBJECTID, district, input$map_var) %>%
      rename(selected_var = input$map_var)}) 
  
  output$map <- renderLeaflet({

    
    kos_map <- merge(kos_map, map_subset(), by = "OBJECTID", duplicateGeoms = FALSE)
    
    # Color pal for the shapefile 
    
    nc_pal <- colorBin(palette = "Greens", bins = 5,
                       domain = kos_map@data$selected_var)
    
    # Create basic map 
    
    kos_map <- kos_map %>% 
      leaflet() %>%
      addProviderTiles(providers$Stamen.Watercolor, group = "Stamen") %>%
      setView(lat = 57, lng = 62, zoom = 6) %>%
      addPolylines(data = rivers, 
                   weight = 2,
                   opacity = 1,
                   color = "blue",
                   group = "Rivers") %>%
      # setMaxBounds(lng1 = 55, lat1 = 48, lng2 = 68, lat2 = 60) %>%
      addLegend("bottomright",
                pal = nc_pal,
                values = ~selected_var,
                na.label = "NA",
                title = paste0(names(data_options[which(data_options == input$map_var)]), 
                               " (", 
                               names(label_options[which(label_options == input$map_var)]), ")"),
                opacity = 1)  %>%
      addLayersControl(
        baseGroups = c("Stamen"),
        overlayGroups = c("Rivers"),
        options = layersControlOptions(collapsed = FALSE))
   
   
    
    # If historical basemap option is checked, add the following elements to the map 
     
    if (input$box == TRUE) {
    
    
    # Color pal for the raster
    
    pal_ras <- colorNumeric(palette = "Greys", domain = NULL,
                            na.color = "transparent")
      #(AFTER RAS) colors = pal_ras, 
    
    kos_map %>%
      addRasterImage(ras, color = pal_ras, opacity = 0.8) %>% 
      addPolygons(weight = 4,
                  fillOpacity = .7,
                  color = ~nc_pal(selected_var),
                  label = ~paste0(district, 
                                  ", ", 
                                  str_to_lower(names(data_options[which(data_options == input$map_var)])), 
                                  ": ", 
                                  selected_var, 
                                  names(label_options[which(label_options == input$map_var)])),
                  highlight = highlightOptions(weight = 3,
                                               color = "red",
                                               bringToFront = TRUE))
    
    # If historical basemap option is NOT checked, just add the polygons to the basic map 
    
    } else {
      
      kos_map %>%
        addPolygons(stroke = TRUE,
                    weight = 4,
                    fillOpacity = .7,
                    color = ~nc_pal(selected_var),
                    label = ~paste0(district, 
                                    ", ", 
                                    str_to_lower(names(data_options[which(data_options == input$map_var)])), 
                                    ": ", 
                                    selected_var, 
                                    names(label_options[which(label_options == input$map_var)])),
                    highlight = highlightOptions(weight = 3,
                                                 color = "red",
                                                 bringToFront = TRUE)) 

      
    }
    
  })

output$title <- renderUI({
  h4(paste0(names(data_options[which(data_options == input$map_var)]), ", Kostroma Province (", input$year, ")"))
})    
  
}

# Run the application 

shinyApp(ui = ui, server = server)

