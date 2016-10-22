# Twitter downloader and analyzer
#    This app allows the user to:
#    - download Twitter data related for specific word/words
#    - generate some plots related to downloaded tweets

# loading some functions before 
source("Twitter_functions.R")

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

shinyServer(function(input, output) {
      
      # Take a reactive dependency on input$button (downloading twitter data)
      df <- eventReactive(input$button, {
            LAT <- input$LAT
            LON <- input$LON
            RAD <- input$RAD
            
            if(input$useGEO==FALSE){
                   LAT <- NULL
                   LON <- NULL
                   RAD <- NULL
            }

            # function for downloading tweets
            withProgress(message="downloading data from twitter...",value=10, {
                  TWITTERdownload(input$WORD,numtweets=input$numtweets,LAT=LAT,LON=LON,RAD=RAD,remove.retweets=input$remove.retweets)
            })
      })
      
      # TAB message
      TABmessage <- reactive({
            DATALENGTH <- NROW(df())
            DATALENGTH_GEO <- length(na.omit(df()$longitude))
            if(DATALENGTH_GEO == 0) paste0("Downloaded ", DATALENGTH," tweets, (none of them has geodata)")
                  else
                  paste0("Downloaded ", DATALENGTH," tweets, (only ",DATALENGTH_GEO," with geodata)")
      })
      output$tabinfo <- renderText({
            TABmessage()
      })
      
      # plot 1
      output$plot1 <- renderPlotly({
            PLOTLYgraphs(tweets.df=df(),type=1)
      })
      
      # plot 2
      output$plot2 <- renderPlotly({
            PLOTLYgraphs(tweets.df=df(),type=2)
      })

      # word cloud
      output$wcloud <- renderWordcloud2({
            withProgress(message="calculating word frequency...",value=10, { wordfreq <- WORDCLOUDfreq(df()) })
            withProgress(message="processing wordcloud...",value=10, { wordcloud2(wordfreq, size=3) })
      })
      
      # raw table
      output$table <- renderDataTable({
            df()[,c("text","screenName","replyToSN","created","retweetCount","longitude","latitude")]
      })

      # MAP message
      MAPmessage <- reactive({
            DATALENGTH_GEO <- length(na.omit(df()$longitude))
            if(DATALENGTH_GEO == 0) "There are no geo data for plotting!" else paste0("Plotting ",DATALENGTH_GEO," points (only tweets with geodata position.)")
      })
      output$info <- renderText({
            MAPmessage()
      })
      
      # render the MAP
      output$map <- renderLeaflet({
            TWITTERmap(df())
      })

})


