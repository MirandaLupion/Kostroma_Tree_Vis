# Load libraries

library(shiny)
library(readr)
library(stringr)
library(shinythemes)
library(tidyverse)

# Load data from CSV files

area_by_district <- read_csv("area_by_district_1908.csv") %>%
  filter(district != "All") %>%
  gather(key = "condition", value, -district)

# area_options = c("Total area" = "total_area_desyatin",
#                  "Suitable area" = "suitable_area_desyatin",
#                  "Forested area" = "forested_area_desyatin",
#                  "Percent forested area (of total land)" = "percent_forested_total",
#                  "Percent forested area (of suitable land)" = "percent_forested_suitable")


ui <- fluidPage(fluidPage(theme = shinytheme("united")),
                
 # Application title
                
 titlePanel("Visualizing Forests in Kostroma Province"),
                
 sidebarLayout(
   sidebarPanel(
     
     radioButtons("radio", 
                  label = "View data as",
                  choices = list("percent" = 1, "raw" = 2), 
                  selected = 1),

     
     selectizeInput(inputId = "d", #internal label
                    label = "Select districts to include", #label that user sees
                    choices = c(area_by_district$district), #choose from this list 
                    selected = c(area_by_district$district),
                    multiple = TRUE, # can choose multiple 
                    options = list(maxItems = 13))), #can choose up to five
   
   # Show a plot of the generated distribution
   mainPanel(
     plotOutput("plot")
   )
 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pop_subset <- reactive({
    req(input$d, input$radio)
    if (input$radio == 1) {
      area_by_district %>%
      filter(district %in% input$d & condition %in% c("percent_forested_suitable", "percent_forested_total")) %>%
      mutate(condition = case_when(condition == "percent_forested_suitable" ~ "Percent forested area (of suitable land)",
                                   condition == "percent_forested_total" ~ "Percent forested area (of total land)"))
    } else {
      filter(area_by_district, district %in% input$d & condition %in% c("total_area_desyatin", "suitable_area_desyatin", "forested_area_desyatin"))
    }
    })
  
  
  y_value <- reactive({
    if (input$radio == 1) {
      y_value = "Percent"
    } else {
      y_value = "Area in desyatins (approximately 2.7 acres)"
    }
  })
  
  output$plot <- renderPlot({
    ggplot(data = pop_subset(), aes_string(x = "district", y = "value", fill = "condition")) + 
      geom_bar(position="dodge", stat="identity") + #color by region
      labs(x = "District", 
           title = "Forested area in Kostroma province, 1908",
           y = y_value()) +
      coord_flip() +
      theme(legend.title = element_blank())
  })}

# Run the application 
shinyApp(ui = ui, server = server)

