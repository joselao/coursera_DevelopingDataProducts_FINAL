# Twitter downloader and analyzer
#    This app allows the user to:
#    - download Twitter data related for specific word/words
#    - generate some plots related to downloaded tweets

library(twitteR)
library(plyr)
library(leaflet)
library(shiny)
library(leaflet)
library(wordcloud2)
library(ggplot2)
library(tm)
library(plotly)
library(NLP)

shinyUI(fluidPage(
      # Application title
      titlePanel("Twitter downloader and analyzer."),
      h4("COURSERA - Developing Data Products | week 4 project assignment"),
      h5("Jose Lao | 22-october-2016"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
            sidebarPanel(
                  #h4("Main input values:"),
                  textInput("WORD","Word/words to search:",value=""),
                  numericInput("numtweets","number of tweets to download?",1000,min=100,max=100000,step=1000),
                  checkboxInput("remove.retweets", "remove retweets?", value=FALSE),
                  
                  h4("Geolocation coordinates:"),
                  numericInput("LAT","Latitude",41.390205),
                  numericInput("LON","Longitude",2.154007),
                  numericInput("RAD","Radius (km)",30,0.1,10000,5),
                  checkboxInput("useGEO", "use geolocation option?", value=FALSE),
                  
                  actionButton("button", "Download!")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                  tabsetPanel(
                        tabPanel("INSTRUCTIONS",
                              verbatimTextOutput("tabinfo"),
                              br(),
                              h4("Description."),
                              p("This app allows the user to:"),
                              p("- download Twitter data related for specific word/words"),
                              p("- generate some plots related to downloaded tweets"),
                              br(),
                              h4("Instructions."),
                              p("1) Introduce the word or words to search in Twitter (Use '+' to separate query terms, or \"word1 word2\" for phrases)"),
                              p("2) Set the number of tweets you want to download (more tweets = more waiting time!!)"),
                              p("3) Set if you want to remove retweets"),
                              p("4) Click the Download button"),
                              p("5) Wait and enjoy it!!"),
                              br(),
                              em("OPTIONAL: you can also set latitude, longitude and radius and check 'use geolocation option' if you want to search on an specific location")),
                        tabPanel("PLOTS",plotlyOutput("plot1"),
                                         HTML("<br><br>"),
                                         plotlyOutput("plot2")),
                        tabPanel("WORDCLOUD",wordcloud2Output("wcloud")),
                        tabPanel("TWEETS",dataTableOutput("table")),
                        tabPanel("MAP",verbatimTextOutput("info"),
                                       leafletOutput("map", width="100%", height="500px"))
                  )
            )
      )
))
