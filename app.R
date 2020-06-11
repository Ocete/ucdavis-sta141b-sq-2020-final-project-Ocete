readRenviron(".Renviron")
library(shiny)
library(dplyr)
library(httr)
library(tidyverse)
library(tidytext)
library(jsonlite)
library(rvest) # read_html
library(wordcloud)
library(gapminder)
library(textdata)
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(sentimentr)
myKey <- Sys.getenv("THE_GUARDIAN_KEY")

a <-"
lexicon to cite:
article{mohammad13,
author = {Mohammad, Saif M. and Turney, Peter D.},
title = {Crowdsourcing a Word-Emotion Association Lexicon},
journal = {Computational Intelligence},
volume = {29},
number = {3},
pages = {436-465},
doi = {10.1111/j.1467-8640.2012.00460.x},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
year = {2013}
}

# How to deploy: https://www.shinyapps.io/admin/#/dashboard
# To deploy the app run:
# library(rsconnect)
# rsconnect::deployApp('path/to/your/app')

"

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Set the title of the web browser tab
  title = "The Guardian Stats",
  
  titlePanel(h1('The Guardian Stats', align="center")),
  
  navbarPage("Menu",
    tabPanel(
      "Analysis", 
      
      # Sidebar layout with input and output definitions
      sidebarLayout(
        position = "left",
         sidebarPanel(
           selectInput('section', 'Section', c("-"), selected="-"),
           sliderInput("n_news", "Number of news:",
                       min = 5, max = 100, value = 10, step = 1),
           h4(textOutput('tfIdfTitle')),
           tableOutput('tfIdfTable'),
           textOutput('tfIdfText')
         ),
        
        # Main panel for displaying outputs
        mainPanel(
          tabsetPanel(
            
            tabPanel(
              "Wordcloud",
              h4(textOutput('wordcloudTitle')),
              plotOutput('myWordcloud', width="100%", height="550px")
            ),
            
            tabPanel(
              "Sentiment Analysis",
              column(8, plotlyOutput("pieChart", height = 700, width = "130%"))
            )
          )
        )
        
      )
    ),
    tabPanel(
      "About",
      h4(paste("This Shiny app scraps the last news from the selected section of The Guardian and",
         " displays some data about it. You can also select how many news are scrapped."))
    ),
    tabPanel(
      "Github link",
      h4(uiOutput("githubLink"))
    )
  )
)

server <- function(input, output, session) {
  
  ### PRECOMPUTED STUFF ###
  
  # We will use the stop words dataset
  data(stop_words)
  
  # Precompute the availeable sections
  endpoint <- URLencode(paste("http://content.guardianapis.com/sections?page-size=100&api-key=",
                              myKey, sep=""))
  r <- GET(endpoint, query = list())
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  query_response <- fromJSON(json, flatten = TRUE)
  availeableSections <- query_response$response$results[['id']]
  availeableSections <- c("-", availeableSections)
  updateSelectInput(session, "section", choices=availeableSections, selected = "-")
  
  ### VARIABLES AND DATA ###
  
  # This variable holds a dataframe with the data query from The Guardian API
  query_df <- reactive({
    if (input$section == "-")
      return (NULL)
    
    endpoint <- URLencode(paste("http://content.guardianapis.com/", input$section, 
                                "?page-size=", input$n_news, "&api-key=", myKey, sep=""))
    r <- GET(endpoint, query = list())
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    query_response <- fromJSON(json, flatten = TRUE)
    return(query_response$response$results)
  })
  
  # This variable holds a df with the info obtained from the 
  # different news with the selected section
  news_content_df <- reactive({
    if (input$section == "-")
      return (NULL)
    
    news_content_df <- NULL
    query_df <- query_df()
    for (i in seq_len(nrow(query_df))) {
      temp_url <- query_df[i,'webUrl']
      html <- read_html(temp_url)
      
      text <- html %>%
        html_nodes("p") %>%
        html_text()
      text <- paste(text, collapse = '')
      d <- as.Date(substr(query_df[i,'webPublicationDate'], 1, 10))
      
      news_content_df <- bind_rows(
        news_content_df,
        tibble(id=i, text=text, date=d)
      )
    }
    return(news_content_df)
  })
  
  # This variable holds a dataset equivalent to calling `get_sentiments("nrc")`
  # That call doesn't work on deployment because it needs interaction (it has a menu)
  # I had to swim through github to find where the data is kept
  # (https://github.com/EmilHvitfeldt/textdata/blob/ef8d9af6d99f15b16373575dd96502e9f674bd65/R/lexicon_nrc.R)
  # Then download it and use it locally.
  # I submitted an issue to fix this behaviour:
  # https://github.com/EmilHvitfeldt/textdata/issues/33
  sentiments_df <- reactive({
    df <- read.table("NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", header = FALSE,
                     sep = "", dec = ".", stringsAsFactors = FALSE,)
    colnames(df) <- c("word", "sentiment", "val")
    df <- df %>% 
      filter(val == 1) %>% 
      select("word", "sentiment")
    
    return(df)
  })
  
  ### DISPLAYED STUFF ###
  
  # Title before the wordcloud
  output$wordcloudTitle <- renderText({
    req(input$section != "-" && !is.null(nrow(query_df())))
    paste("Words that appeared the most in the last ", input$n_news, 
          "news from this section:", sep="")
  })
  
  # Plot the Wordcloud
  output$myWordcloud <- renderPlot({
    # Don't display any output and display error messages instead.
    shiny::validate(
      need(input$section != "-", "Please, choose a section")
    )
    shiny::validate(
      need( !is.null(nrow(query_df())) , 
            'There were not news on this sections recently.\n Please, select a different one.')
    )
    
    word_count_df <- news_content_df() %>%
      unnest_tokens(word, text) %>% 
      anti_join(stop_words, by="word") %>% 
      group_by(word) %>%
      count() %>% 
      arrange(desc(n))
    
    word_count_df %>% 
      with(wordcloud(
        word, n, min.freq = 2, max.words = 200, random.order = FALSE, scale=c(3,.5),
        colors = brewer.pal(8, "Dark2")))
  })
  
  # Title before the wordcloud
  output$tfIdfTitle <- renderText({
    req(input$section != "-" && !is.null(nrow(query_df())))
    "tf-idf study:"
  })
  
  # Table showing the tf-idf information
  output$tfIdfTable <- renderTable({
    req(input$section != "-" && !is.null(nrow(query_df())))
    
    df <- news_content_df() %>%
      unnest_tokens(word, text) %>% 
      anti_join(stop_words, by="word") %>% 
      group_by(id, word) %>%
      count()
    
    df_word_count <- df %>% 
      group_by(word) %>% 
      summarize(total_frequency=sum(n))
    
    df_news_count <- df %>% 
      select(id, word) %>% 
      distinct(id, word) %>% 
      group_by(word) %>% 
      summarize(n_news=n())
    
    df_final <- df %>% 
      bind_tf_idf(word, id, n) %>%
      inner_join(df_word_count, by="word") %>%
      inner_join(df_news_count, by="word") %>%
      arrange(desc(tf_idf)) %>% 
      ungroup() %>% 
      slice(1:10) %>% 
      select(word, total_frequency, n_news, tf, idf, tf_idf)
    
    return (df_final)
  })
  
  # Text explaining the tf-idf study
  output$tfIdfText <- renderText({
    req(input$section != "-" && !is.null(nrow(query_df())))
    paste("total_frequency is the total number of appereances of the word",
          "and n_news, the number of news where it appears.")
  })
  
  
  # Pie chart for sentinet analysis
  output$pieChart <- renderPlotly({
    # Don't display any output and display error messages if no option has been selected.
    shiny::validate(
      need(input$section != "-", "Please, choose a section")
    )
    shiny::validate(
      need( !is.null(nrow(query_df())) , 
            'There were not news on this sections recently.\n Please, select a different one.')
    )
    
    df <- news_content_df() %>%
      unnest_tokens(word, text) %>% 
      anti_join(stop_words, by="word") %>% 
      group_by(word) %>% # Change this to group by news instead
      count() %>% 
      inner_join(sentiments_df(), by="word") %>% 
      group_by(sentiment) %>% 
      count() %>% 
      arrange(desc(n))
    
    labels <- df[['sentiment']]
    values <- df[['n']]
    plot_ly(
      type = "pie", labels=labels, values=values,
      textinfo = "label",
      insidetextorientation = "radial"
    ) %>%
      layout(title = paste("Word by word sentiment analysis of the last ", 
                           input$n_news, " news.", sep=""))
    
  })
  
  # Configure the Github Link displayer
  output$githubLink <- renderUI({
    githubUrl <- a("https://github.com/Ocete/ucdavis-sta141b-sq-2020-final-project-Ocete", 
                   href="https://github.com/Ocete/ucdavis-sta141b-sq-2020-final-project-Ocete")
    tagList("URL link:", githubUrl)
  })
}

shinyApp(ui = ui, server = server)

# reference to plot with dat in x axis https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781849513067/4/ch04lvl1sec12/plotting-date-and-time-on-the-x-axis
