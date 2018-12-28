# Load libraries

library(shiny)
library(readr)
library(stringr)
library(shinythemes)
library(DT)
library(plotly)
library(tidyverse)
library(scales)

# Load data from CSV files

area_by_district <- read_csv("area_by_district_1908.csv") %>%
  filter(district != "All") %>%
  gather(key = "condition", value, -district)

# The UI

ui <- fluidPage(fluidPage(theme = shinytheme("flatly")),
                
 # Application title
                
 titlePanel("Forested Area in Kostroma Province"),

 # Allow user to select raw or percent data form
                 
 sidebarLayout(
   sidebarPanel(
     radioButtons("radio", 
                  label = "View data as",
                  choices = list("percent" = 1, "raw" = 2), 
                  selected = 1),
  
  # Allow user to select districts to include
     
     selectizeInput(inputId = "d", #internal label
                    label = "Select districts to include", #label that user sees
                    choices = c(area_by_district$district), #choose from this list 
                    selected = c(area_by_district$district),
                    multiple = TRUE, # can choose multiple 
                    options = list(maxItems = 13))), #can choose up to five
   
   # Create a tabset structure with two tabs
  
   mainPanel(
     tabsetPanel(
     
    # Tab for the graph with plotly vis  
        
     tabPanel("Graph", 
              h5("Mouse over the bars of the graph for more information."),
              br(),
              br(),
              plotlyOutput("plot")),
    
     # Tab for the table
     
     tabPanel("Table", 
              DT::dataTableOutput("table"))
))))

# Define server

server <- function(input, output) {
  
  # Filter the data based on the user inputs 
  
  pop_subset <- reactive({
    req(input$d, input$radio)
    if (input$radio == 1) {
      area_by_district %>%
      filter(district %in% input$d & condition %in% c("percent_forested_suitable", "percent_forested_total")) %>%
      mutate(condition = case_when(condition == "percent_forested_suitable" ~ "Percent forested area of suitable land",
                                   condition == "percent_forested_total" ~ "Percent forested area of total land"))
    } else {
      area_by_district %>%
      filter(district %in% input$d & condition %in% c("total_area_desyatin", "suitable_area_desyatin", "forested_area_desyatin")) %>%
      mutate(condition = case_when(condition == "total_area_desyatin" ~ "Total area", 
                                   condition == "suitable_area_desyatin" ~ "Suitable area",
                                   condition == "forested_area_desyatin" ~ "Forested area"))
    }
    })
  
  # Reactive y axis title 
  
  y_value <- reactive({
    if (input$radio == 1) {
      y_value = "Percent"
    } else {
      y_value = "Area in desyatins (approximately 2.7 acres)"
    }
  })
   
  # Reactive plot
  
  output$plot <- renderPlotly({
    ggplotly(tooltip = "y", ggplot(data = pop_subset(), aes_string(x = "district", y = "value", fill = "condition")) + 
      geom_bar(position="dodge", stat="identity") + #color by region
      labs(x = NULL, 
           title = "Forested area in Kostroma province, 1908",
           y = y_value()) +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.title = element_blank())) %>% 
      config(displayModeBar = FALSE)  %>%
      layout(legend = list(x = 100, y = 0.5))
   
                
  })
  
  # Reactive table
  
  output$table <- DT::renderDataTable({
    if (input$radio == 1) {
      table_data <- area_by_district %>%
        filter(district %in% input$d & condition %in% c("percent_forested_suitable", "percent_forested_total")) %>%
        mutate(condition = case_when(condition == "percent_forested_suitable" ~ "Percent forested (suitable land)",
                                     condition == "percent_forested_total" ~ "Percent forested (total land)")) %>%
        spread(condition, value) %>%
        rename(District = district)

    } else {
      table_data <- area_by_district %>%
        filter(district %in% input$d & condition %in% c("total_area_desyatin", "suitable_area_desyatin", "forested_area_desyatin")) %>%
        mutate(condition = case_when(condition == "total_area_desyatin" ~ "Total area",
                                     condition == "suitable_area_desyatin" ~ "Suitable area",
                                     condition == "forested_area_desyatin" ~ "Forested area")) %>%
        spread(condition, value) %>%
        rename(District = district)

    }
DT::datatable(table_data,
                   rownames = FALSE)
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

