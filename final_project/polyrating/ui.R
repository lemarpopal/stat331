library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)

polyrating <- read_csv(
  "https://raw.githubusercontent.com/ayakkala1/stat_final/master/vignettes/polyrating.csv"
) %>% 
  mutate(date = parse_date_time(date,"%m%y")) %>%
  drop_na()

subjects <- polyrating %>%
            distinct(subject) %>%
            pull() %>%
            unlist()

token_words <- read_csv("https://raw.githubusercontent.com/ayakkala1/stat_final/master/vignettes/unique_poly.csv") #%>%
 
                

shinyUI(fluidPage(
  #theme = "mytheme.css",
  navbarPage("PolyRating",
             tabPanel("Term Frequencies",
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      'subject', label = "Subject: ", choices = c("ALL",subjects),
                      options = list(maxItems  = 100), selected = "ALL"
                    ),
                    hr(),
                    wellPanel(
                    checkboxGroupInput(inputId = "selected_type",
                                       label = tags$a(href = "https://en.wikipedia.org/wiki/Stop_words",
                                                                       "Take out Stopwords?"),
                                       choices = c("Yes"),
                                       selected = "Yes"),
                    textInput(inputId = "add_stop", 
                              label = "Add to the Stop Word lexicon: ", 
                              value = "", 
                              width = NULL, 
                              placeholder = NULL),
                    actionButton("add","Add"),
                    br(),
                    br(),
                    textInput(inputId = "delete", 
                              label = "Remove a word from Stop Word lexicon: ", 
                              value = "", 
                              width = NULL, 
                              placeholder = NULL),
                    actionButton("delete_butt","Delete"),
                    br(), br(),
                    actionButton("default","Default")),
                    wellPanel(checkboxGroupInput(inputId = "use_tf",
                                                 label = tags$a(href = "https://en.wikipedia.org/wiki/Tf%E2%80%93idf",
                                                                     "Use TF-IDF metric?"),
                                                 choices = c("Yes"),
                                                 selected = NULL))
                  ,
                  br(), br(),
                  h5("Built with",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                     "using", img(src = "https://raw.githubusercontent.com/juliasilge/tidytext/master/tools/tidytext.png",
                                  height = "30px"),
                     "on",
                     img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                         height = "30px"),
                     "."),
                  width = 4),
                   
          
                  mainPanel(
                     plotOutput("distPlot")
                           )
                          )
                        ),
             tabPanel("Word Over Time",
                      h4("Examine how word use changes over time in PolyRating reviews!"),
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            selectizeInput(
                              'timesubject', label = "Subject: ", choices = c("ALL",subjects),
                              options = list(maxItems  = 100), selected = "ALL"
                            ),
                            selectizeInput(inputId = "timeword", 
                                      label = "Choose words: ", 
                                      choices = c(token_words), 
                                      options = list(maxItems  = 100), 
                                      selected = "work"),
                            actionButton("examine","Examine")),
                          br(), br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "using", img(src = "https://raw.githubusercontent.com/juliasilge/tidytext/master/tools/tidytext.png",
                                          height = "30px"),
                             "on",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                                 height = "30px"),
                             "."),
                          width = 4),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("timePlot")
                        )
                      )
                     ),
             tabPanel("College Sentiment",
              h4("Look at a subject's sentiment over time!"),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   selectizeInput(
                     'sentimentsubj', label = "Subject: ", choices = subjects,
                     options = list(maxItems  = 4, selected = "STAT"
                                 )
                          ),
                 br(), br(),
                 h5("Built with",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                    "using", img(src = "https://raw.githubusercontent.com/juliasilge/tidytext/master/tools/tidytext.png",
                                 height = "30px"),
                    "on",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", 
                        height = "30px"),
                    "."),
                 width = 4)),
               mainPanel(
                 plotOutput("sentPlot")
               )
              )
             ),
             tabPanel("Word Cloud",
                      titlePanel("Word Cloud of Most Frequent Words by Subject"),
                      
                      sidebarLayout(
                        # Sidebar with a slider and selection inputs
                        sidebarPanel(
                          selectInput("selection", "Choose a Department:",
                                      choices = subjects),
                          actionButton("update", "Update Subject"),
                          hr(),
                          sliderInput("freq",
                                      "Minimum Frequency:",
                                      min = 1,  max = 50, value = 15),
                          sliderInput("max",
                                      "Maximum Number of Words:",
                                      min = 1,  max = 300,  value = 100)
                        ),
                        
                        # Show Word Cloud
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             )
          )
        )
      
  )

