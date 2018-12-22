library(shiny)
library(plotly)
library(readr)
library(tidyverse)
library(shinythemes)


ui <- fluidPage(fluidPage(theme = shinytheme("united")),
                
 # Application title
                
 titlePanel("Visualizing Forests in Kostroma Province"),
                
 # Sidebar with a select input function, letting users chose the sample demographic to analyze 
                
 sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "variable",
                                label = "Select the sample demographic to analyze",
                                choices = v_options,
                                multiple = FALSE, 
                                selected = v_options[6]),
                    
                    # And a button allowing users to download my data and further poke around if they wish 
                    
                    downloadButton(outputId = "download_data", 
                                   label = "Download data")),
                  
                  # Define the main panel
                  
                  mainPanel(
                    
                    # Use a tab layout to separate the various elements
                    
                    tabsetPanel(type = "tabs",
                                tabPanel("About this app", htmlOutput("about")),
                                tabPanel("Scatterplot", plotlyOutput("scatterplot1")),
                                tabPanel("Linear regression plot", plotOutput("scatterplot2")),
                                tabPanel("Regression summary", htmlOutput("stats")))
                    
                  )))
