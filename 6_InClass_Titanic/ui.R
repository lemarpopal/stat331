# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
library(tidyverse)
library(readr)

titanic <- read_csv("https://www.dropbox.com/s/volbfu8onyvjcri/titanic.csv?dl=1")

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Titanic Fare by Gender and Passenger Class"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Pclass", "Class:", 
                  choices=c(1,2,3), 
                  selected = 1),
      hr(),
      helpText("Data from Titanic passenger lists")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("titanicPlot")  
    )
    
  )
)