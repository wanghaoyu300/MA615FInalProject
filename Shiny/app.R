library(abind)
library(arules)
library(arulesViz)
library(clustMixType)
library(dashboardthemes)
library(dplyr)
library(GGally)
library(plotly)
library(rcompanion)
library(rvest)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(timeDate)
library(visNetwork)
library(wordcloud2)
library(xml2)
library(extrafont)
library(tm)
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(ggpubr)

ui = dashboardPage(
        dashboardHeader(title = "Indeed Explore"),
        dashboardSidebar(tags$h1("Wlecome!"), br(),
                         tags$small("The data were cllect from indeed.com"), br(), br()
                             ),
        dashboardBody(tabsetPanel(
            tabPanel("Years of Experience",plotOutput("yearplot"),
                     splitLayout(tableOutput("wordcount"),
                                 tableOutput("companycount"),
                                 tableOutput("loactioncount"))),
            tabPanel("Counting plot",
                     splitLayout(plotOutput("wordpic")),
                                 plotOutput("companypic"),
                                 plotOutput("locationpic")),
            tabPanel("Word Cloud", wordcloud2Output("wordcloud"))
            
            )))

server = function(input, output,session) {
    df <- read.csv("df.csv")
    df$description <- removeWords(df$description, stopwords("english"))
    df$description <- removePunctuation(df$description)
    df$description <- removeNumbers(df$description)
            
    Working_Experience <- function(text){
                
        extractnumber <- function(string){
            str_extract(string, "\\-*\\d+\\.*\\d*")}
                
            years <-  extractnumber(text)
                
            years <- as.numeric(years[!is.na(years)]) 
                 
            years <- years[(years < 15) & (years %% 1 == 0) & (years >= 0)] 
                
            p <- gghistogram(data.frame(years), 
                             x = "years",
                             fill = "pink", 
                             color = "lightblue",
                             alpha=0.5, 
                             binwidth = 1
                )
            ggpar(p, xlab = "Years", ylab = "Frequency",
                  title = "Working Experience Required",
                  xticks.by = 1,
                  ggtheme = theme_pubr())+ font("title", hjust=0.5)
            }
    Working_Experience(unlist(df))

    output$yearplot <- renderPlot(Working_Experience(unlist(df)))
            
    word_count <- df %>% 
        unnest_tokens(output = "word", token = "words", input = description) %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)

    output$wordcount <- renderTable(head(word_count,10))
            
    company_count <- df %>%
        unnest_tokens(output = "word", token = "words", input = description) %>%
        anti_join(stop_words) %>%
        count(company, sort = TRUE)
            
    output$companycount <- renderTable(head(company_count,10))
    
    location_count <- df %>%
        unnest_tokens(output = "word", token = "words", input = description) %>%
        anti_join(stop_words) %>%
        count(location, sort = TRUE)
            
    output$loactioncount <- renderTable(head(location_count,10))
    
    WordPlot <- function(text){
                ct <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
                operations <- Corpus(VectorSource(text))
                operations <- tm_map(operations, ct, "/")
                operations <- tm_map(operations, ct, "-")
                operations <- tm_map(operations, removePunctuation)
                operations <- tm_map(operations, stripWhitespace) 
                operations <- tm_map(operations, removeWords, stopwords("english")) 
                operations <- tm_map(operations, removeNumbers)
                operations <- tm_map(operations, stemDocument, "english") 
                temp <- DocumentTermMatrix(operations) 
                temp <- removeSparseTerms(temp, 0.99)
            }
            
    des <- df$description
    des1 <- WordPlot(des)
    temp <- c(des1)
    set.seed(300)
    PlotMaking <- function(temp){
    a1 <- as.matrix(t(temp))
    a2 <- sort(rowSums(a1),decreasing = TRUE)
    a3 <- data.frame(word = names(a2),freq = a2)
    set.seed(731)
    wordcloud2(a3)}
    
    output$wordcloud <- renderWordcloud2(PlotMaking(des1))

    p1 <-  ggplot(data = head(word_count, n = 10), aes(x = word, y = n)) + 
                geom_bar(stat = "identity", fill = "pink", col = "lightblue") +
                theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))
    output$wordpic <- renderPlot(p1)
    
    p2 <- ggplot(data = head(company_count, n = 10), aes(x = company, y = n)) + 
                geom_bar(stat = "identity", fill = "pink", col = "lightblue") +
                theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))
    
    output$companypic <- renderPlot(p2)
    
    p3 <- ggplot(data = head(location_count, n = 10), aes(x = location, y = n)) + 
                geom_bar(stat = "identity", fill = "pink", col = "lightblue") +
                theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))
    
    output$locationpic <- renderPlot(p3)
        }

shinyApp(ui = ui, server = server)