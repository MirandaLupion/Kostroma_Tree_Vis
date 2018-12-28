# Load libraries

library(shiny)
library(readr)
library(stringr)
library(shinythemes)
# library(DT)
# library(ggrepel)
library(plotly)
library(tidyverse)

# Load data from CSV files

area_by_district <- read_csv("area_by_district_1908.csv") %>%
  filter(district != "All") %>%
  gather(key = "condition", value, -district)


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
     plotlyOutput("plot")
     # br(),
     # br(),
     # DT::dataTableOutput("table")
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
  
  y_value <- reactive({
    if (input$radio == 1) {
      y_value = "Percent"
    } else {
      y_value = "Area in desyatins (approximately 2.7 acres)"
    }
  })
   
  output$plot <- renderPlotly({
    ggplotly(tooltip = "y", ggplot(data = pop_subset(), aes_string(x = "district", y = "value", fill = "condition")) + 
      geom_bar(position="dodge", stat="identity") + #color by region
      labs(x = NULL, 
           title = "Forested area in Kostroma province, 1908",
           y = y_value()) +
      coord_flip() +
      theme(legend.title = element_blank())) %>% 
      config(displayModeBar = FALSE)  %>%
      layout(legend = list(x = 100, y = 0.5))
    # +
    #   geom_text_repel(mapping = aes(label = value, fontface = "bold"),
    #                   hjust = 1,
                      # segment.size = 0.2)
    
   
                
  })
  
  # output$table <- DT::renderDataTable({
  #   if (input$radio == 1) {
  #     table_data <- area_by_district %>%
  #       filter(district %in% input$d & condition %in% c("percent_forested_suitable", "percent_forested_total")) %>%
  #       mutate(condition = case_when(condition == "percent_forested_suitable" ~ "Percent forested (suitable land)",
  #                                    condition == "percent_forested_total" ~ "Percent forested (total land)")) %>%
  #       spread(condition, value)
  #       
  #   } else {
  #     table_data <- area_by_district %>%
  #       filter(district %in% input$d & condition %in% c("total_area_desyatin", "suitable_area_desyatin", "forested_area_desyatin")) %>%
  #       mutate(condition = case_when(condition == "total_area_desyatin" ~ "Total area", 
  #                                    condition == "suitable_area_desyatin" ~ "Suitable area",
  #                                    condition == "forested_area_desyatin" ~ "Forested area")) %>%
  #       spread(condition, value)
  #     
  #   }
  #   
  #   
  #   DT::datatable(table_data, 
  #                  rownames = FALSE
  #                  #colnames = c()
  #                 
  #                 )
  #   
  #       
  #   
  # })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

