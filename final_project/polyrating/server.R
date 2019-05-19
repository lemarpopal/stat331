#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)
library(wordcloud)

polyrating <- read_csv(
  "https://raw.githubusercontent.com/ayakkala1/stat_final/master/vignettes/polyrating.csv"
) %>% 
  mutate(date = parse_date_time(date,"%m%y")) %>%
  drop_na()

token_words <- read_csv("https://raw.githubusercontent.com/ayakkala1/stat_final/master/vignettes/unique_poly.csv")

data(stop_words)

subjects <- polyrating %>%
  distinct(subject) %>%
  pull() %>%
  unlist()

review_words <- polyrating %>%
  unnest_tokens(word,review) %>%
  count(subject, word, sort = TRUE) %>%
  ungroup()

total_words <- review_words %>%
  group_by(subject) %>%
  summarize(total = sum(n))

review_words <- left_join(review_words, total_words)

shinyServer(function(input, output, session) {
  
  #--------------------------------FREQUENCY TAB--------------------------
  add_to_stop_r <- reactiveValues()
  
  delete_to_stop_r <- reactiveValues()
  
  observe({
    if(input$add > 0){
      add_to_stop_r$dList <- c(isolate(add_to_stop_r$dList), isolate(input$add_stop))
    }
  })
  
  observe({
    if(input$delete_butt > 0){
      delete_to_stop_r$dList <- c(isolate(delete_to_stop_r$dList), isolate(input$delete))
    }
  })
  
  observe({
    if(input$default > 0){
      add_to_stop_r$dList <- reactiveValues()
      delete_to_stop_r$dList <- reactiveValues()
    }
  })

  stop_words_new <- reactive({stop_words %>%
                          filter(!word %in% delete_to_stop_r$dList)})
  
  
  tokens <- reactive({
    polyrating %>%
      unnest_tokens(word, review)
  })
  
  subject_selected <- reactive({
    req(input$subject)
    if ("ALL" %in% input$subject){tokens()
      }else{
    filter(tokens(), subject == input$subject)}
  })
  
  
  output$distPlot <- renderPlot({
    validate(
      need(input$subject != "", "Please select a subject.")
    )
    subject_words <- paste(input$subject,collapse = ", ")

    new_filter <- replace(input$subject, input$subject == "ALL", "[\\s\\S]+")

    if (length(input$use_tf) != 0){
      if (length(input$selected_type) == 0){
        stop_filter = FALSE
      } else{
        stop_filter = TRUE
      }
    if (stop_filter){
      review_words %>%
        bind_tf_idf(word, subject, n) %>%
        filter(str_detect(subject,new_filter)) %>%
        anti_join(stop_words_new()) %>%
        filter(!word %in% add_to_stop_r$dList) %>%
        top_n(10, tf_idf) %>%
        mutate(word = fct_reorder(word,tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("TF-IDF") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }else{
      review_words %>%
        bind_tf_idf(word, subject, n) %>%
        filter(str_detect(subject,new_filter)) %>%
        top_n(10, tf_idf) %>%
        mutate(word = fct_reorder(word,tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("TF-IDF") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }
    }else{
    if (length(input$selected_type) == 0){
      stop_filter = FALSE
    } else{
      stop_filter = TRUE
    }
    if (stop_filter){
      subject_selected() %>%
        anti_join(stop_words_new()) %>%
        filter(!word %in% add_to_stop_r$dList) %>%
        count(word) %>%
        top_n(10, n) %>%
        mutate(word = fct_reorder(word,n)) %>%
        ggplot(aes(word, n, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("Term Frequency") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
                guides(fill = FALSE)
    }else{
      subject_selected() %>%
        count(word) %>%
        top_n(10, n) %>%
        mutate(word = fct_reorder(word,n)) %>%
        ggplot(aes(word, n, fill = word )) + geom_col() +
        xlab(NULL) + coord_flip() + guides(fill = FALSE) + ylab("Term Frequency") +
        ggtitle(paste(c("Most important words in PolyRating Reviews for",subject_words),collapse = " ")) + 
        guides(fill = FALSE)
    }
    }
    }
  )
  
  #------------------------------ WORD OVER TIME TAB -------------------------------
  
  updateSelectizeInput(session = session, inputId = 'timeword', choices = c(token_words), server = TRUE)

  
  year_counts <- reactive({
    req(input$timesubject)
    if ("ALL" %in% input$timesubject){tokens() %>%
        count(date,word) %>%
        complete(date, word, fill = list(n = 0))
    }else{
      tokens() %>%
        filter(subject %in% input$timesubject) %>%
        count(date,word) %>%
        complete(date, word, fill = list(n = 0))}
  })
  
  year_totals <- reactive({year_counts() %>%
                            group_by(date) %>%
                            summarize(year_total = sum(n))})
  
  year_vals <- reactive({year_counts() %>%
      left_join(year_totals(), by = "date")})
  
  timeplot <- eventReactive(input$examine,
    {
      withProgress({
        setProgress(message = "Processing corpus...")
    year_vals() %>%
        filter(word %in% input$timeword) %>%
        ggplot(aes(date, n / year_total,fill = word, color = word)) +
        geom_point() +
        geom_smooth() +
        scale_y_continuous(labels = scales::percent_format()) +
        ylab("% frequency of word in review") +
        xlab(element_blank())
      })
  })
  
  output$timePlot <- renderPlot({timeplot()})
  
  #-------------------------------- SENTIMENT TAB ------------------------------------
  
  subject_selected_sent <- reactive({
    req(input$sentimentsubj)
    filter(tokens(), subject %in% input$sentimentsubj)
  })
  
  output$sentPlot <- renderPlot({
    validate(
      need(input$sentimentsubj != "", "Please select a subject.")
    )
    subject_words <- paste(input$sentimentsubj,collapse = ", ")
    
    subject_selected_sent() %>%
          anti_join(stop_words) %>%
          inner_join(get_sentiments("bing")) %>%
          count(date, subject, sentiment) %>%
          spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive - negative) %>%
          ggplot(aes(date, sentiment,fill = subject)) + geom_col(show.legend = FALSE) +
          facet_wrap(~subject) + ggtitle("College Sentiment Over Time") + 
          theme(panel.spacing.x = unit(1, "lines"), 
                plot.margin = margin(.3, .8, .3, .8, "cm")) +
          xlab(element_blank()) + ylab("Sentiment")
    })
  
  #--------------------------WORDCLOUD TAB -------------------------------
  getTermMatrix <- function(subj) {
    
    if (!(subj %in% subjects))
      stop("Unknown Subject")
    
    vals <- cloud_use() %>%
      filter(subject == subj) %>%
      mutate(freq = n/total) %>%
      unite(freq_n,c(freq,n),sep="::") %>%
      select(subject,freq_n,word) %>%
      spread(subject,freq_n) %>%
      separate(subj,c("FREQ","N"),sep="::") %>%
      drop_na() %>%
      mutate(FREQ = as.numeric(FREQ))
    
    m = as.matrix(vals)
    m
  }
  
  cloud_words <- reactive({tokens() %>%
    anti_join(stop_words) %>%
    count(subject, word, sort = TRUE) %>%
    ungroup()})
  
  cloud_total <- reactive({cloud_words() %>%
    group_by(subject) %>%
    summarize(total = sum(n))})
  
  cloud_use <- reactive({left_join(cloud_words(), cloud_total())})
  
  # Define a reactive expression for the document term matrix
  terms <- eventReactive(
    # Change when the "update" button is pressed...
    input$update,
    # ...but not for anything else
    {
      withProgress({
        setProgress(message = "Processing corpus...")
        as.tibble(getTermMatrix(input$selection)) %>%
          mutate(FREQ = as.double(FREQ))
      }
      )
    })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    wordcloud_rep(terms()$word, terms()$FREQ, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  }
)
