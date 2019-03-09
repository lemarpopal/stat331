# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
library(tidyverse)
library(readr)

titanic <- read_csv("https://www.dropbox.com/s/volbfu8onyvjcri/titanic.csv?dl=1")

# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$titanicPlot <- renderPlot({
    
    filtered_titanic <- titanic %>% filter(Pclass == input$Pclass)
    
    # Render a density plot
    ggplot(filtered_titanic, aes(x=Fare, fill=Sex)) + geom_density(alpha=0.5)
    
  })
}

