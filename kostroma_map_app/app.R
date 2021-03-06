library(shiny)
library(shinythemes)
library(rsconnect)
library(rgdal)
library(plotly)
library(readr)
library(scales)
library(stringr)
library(DT)
library(leaflet)
library(tidyverse)

# Shapefile data prep 

kos_map <- readOGR(dsn = "kos_shp", layer = "kos_shape_georef")
kos_map <- spTransform(kos_map, CRS("+proj=longlat +init=epsg:4326"))

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

# Application title

ui <- navbarPage("Tree Cover in Kostroma Province", theme = shinytheme("paper"),
    
   # Ability to select the data set to map
   
   tabPanel("The Map",
   sidebarLayout(position = "right",
      sidebarPanel(
         selectInput(inputId = "year",
                    label = "Select a year to view possible data sets",
                    choices = c(1818, 1848, 1908),
                    selected = NULL),

         uiOutput("ui"),
         br(),
         br(),
         img(src ="logo.png", height = 200, width = 200)),
      
      mainPanel(
        htmlOutput("title"),
        leafletOutput("map", width = "100%", height = "500px")))),
      
    tabPanel("Data Sources",
               sidebarLayout(
                 sidebarPanel(
                   img(src ="logo.png", height = 200, width = 200),
                   p("Credits...")
                 ),
                 mainPanel(
                   htmlOutput("sources"))
        
        )))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # The Map
  
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
    
    # Color pal for the historical basemap
    
    pal_ras <- colorNumeric(palette = "Greys", domain = NULL,
                            na.color = "transparent")
    
    # Create basic map 
    
    kos_map <- kos_map %>% 
      leaflet() %>%
      
      # Give users a choice of basemaps
      
      addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical Map") %>%
      
      # Set the initial view
      
      setView(lat = 57, lng = 62, zoom = 6) %>%
      
      # Polygons
      
      addPolygons(stroke = TRUE,
                  weight = 2,
                  fillOpacity = .7,
                  opacity = .5,
                  fillColor = ~nc_pal(selected_var),
                  color = "white",
                  label = ~paste0(district, 
                                  ", ", 
                                  str_to_lower(names(data_options[which(data_options == input$map_var)])), 
                                  ": ", 
                                  selected_var, 
                                  names(label_options[which(label_options == input$map_var)])),
                  highlight = highlightOptions(weight = 3,
                                               color = "red",
                                               bringToFront = TRUE)) %>%
      
      # River polylines
      
      addPolylines(data = rivers, 
                   weight = 2,
                   opacity = 1,
                   color = "blue",
                   group = "Rivers") %>%
    
      # 1822 map
      
      addRasterImage(ras, color = pal_ras, opacity = 0.8, group = "1822 Historical Map") %>%
      
       setMaxBounds(lng1 = 55, lat1 = 40, lng2 = 68, lat2 = 70) %>%
      
      addLegend("bottomright",
                pal = nc_pal,
                values = ~selected_var,
                na.label = "NA",
                title = paste0(names(data_options[which(data_options == input$map_var)]), 
                               " (", 
                               names(label_options[which(label_options == input$map_var)]), ")"),
                opacity = 1)  %>%
     
      # Give the user layers control
      
      addLayersControl(
        baseGroups = c("Stamen Watercolor", "Physical Map"),
        overlayGroups = c("Rivers", "1822 Historical Map"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
     
      # Rivers and historical map not present on the map by default 
      
      hideGroup("Rivers") %>%
      hideGroup("1822 Historical Map")
   
  })

output$title <- renderUI({
  h4(paste0(names(data_options[which(data_options == input$map_var)]), ", Kostroma Province (", input$year, ")"))
})    


# The Sources

output$sources <- renderUI({
  HTML(paste(br(),
  
  h4("1818"),
  tags$a(href = "https://www.prlib.ru/item/683125", "Forested area - "),
  p("Федорович, Дюбюк Евгений [Fedorovich, Dyubyuk Evgeniy]. “Материалы Для Оценки Земель Костромской Губернии. Т. 13. Леса и Лесное Хозяйство Костромской Губернии, Таблица 4 [Material for Evaluating Kostroma Province - Forests and Forestry of Kostroma Province, Table 4].” типография Х. А. Гелина [H.A. Gelin's Press], 1912, p. 27."),
  tags$a(href = "https://www.prlib.ru/item/683125", "Forest cover as a percent of total area - "),
  p("Федорович, Дюбюк Евгений [Fedorovich, Dyubyuk Evgeniy]. “Материалы Для Оценки Земель Костромской Губернии. Т. 13. Леса и Лесное Хозяйство Костромской Губернии, Таблица 4 [Material for Evaluating Kostroma Province - Forests and Forestry of Kostroma Province, Table 4].” типография Х. А. Гелина [H.A. Gelin's Press], 1912, p. 27."),
  br(),
  
  h4("1848"),
  tags$a(href = "http://elib.shpl.ru/ru/nodes/48-ch-3-kostromskaya-guberniya-1848-6-104-20-c-5-l-19-s-tabl#mode/inspect/page/151/zoom/4", 
         "Forested area per capita - "), 
  p("“Военно-Статистическое Обозрение Российской Империи - Костромская Губерния, Таблица 12 [Military Statistical Survey of the Russian Empire - Kostroma Guberniia, Table 12].” типография Департамента Генерального Штаба [Department of General Staff Press], 1848." ),
  br(),
  
  h4("1908"),
  tags$a(href = "https://www.prlib.ru/item/683125", "Forested area - "),
  p("Федорович, Дюбюк Евгений [Fedorovich, Dyubyuk Evgeniy]. “Материалы Для Оценки Земель Костромской Губернии. Т. 13. Леса и Лесное Хозяйство Костромской Губернии, Таблица 1 [Material for Evaluating Kostroma Province - Forests and Forestry of Kostroma Province, Table 1].” типография Х. А. Гелина [H.A. Gelin's Press], 1912, p. 15."),
  tags$a(href = "https://www.prlib.ru/item/683125", "Forest cover as a percent of total area - "),
  p("Федорович, Дюбюк Евгений [Fedorovich, Dyubyuk Evgeniy]. “Материалы Для Оценки Земель Костромской Губернии. Т. 13. Леса и Лесное Хозяйство Костромской Губернии, Таблица 1 [Material for Evaluating Kostroma Province - Forests and Forestry of Kostroma Province, Table 1].” типография Х. А. Гелина [H.A. Gelin's Press], 1912, p. 15."),
  tags$a(href = "https://www.prlib.ru/item/683125", "Percent change in forested area from 1818 to 1908 - "),
  p("Федорович, Дюбюк Евгений [Fedorovich, Dyubyuk Evgeniy]. “Материалы Для Оценки Земель Костромской Губернии. Т. 13. Леса и Лесное Хозяйство Костромской Губернии, Таблица 4 [Material for Evaluating Kostroma Province - Forests and Forestry of Kostroma Province, Table 4].” типография Х. А. Гелина [H.A. Gelin's Press], 1912, p. 27."),
  br(),
  
  h4("Geo-spatial data"),
  tags$a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZIZKQB", 
         "Rivers shapefile - "),
  p("O'Neill, Kelly, 2016, Rivers (named) of the Russian Empire (1820s), Harvard Dataverse, V1."),
  tags$a(href = "http://dighist.fas.harvard.edu/projects/imperiia/document/716", "Historical map (1822) - "),
  p("Kostromskaia guberniia,", em("Geographic Atlas of the Russian Empire, the Kingdom of Poland, and the Grand Duchy of Finland."))
  ))
  
})
  
}

# Run the application 

shinyApp(ui = ui, server = server)