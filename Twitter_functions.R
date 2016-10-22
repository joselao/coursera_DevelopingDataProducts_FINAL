TWITTERdownload <- function(WORD,numtweets=100,LAT=NULL,LON=NULL,RAD=NULL,remove.retweets=FALSE,LANGUAGE=NULL,SINCE=NULL,UNTIL=NULL){
      #' This function downloads the tweets from twitter, and returns a dataframe from twitter.
      #' you need API keys for it, see:
      #' read: http://geoffjentry.hexdump.org/twitteR.pdf
      #' go to : https://apps.twitter.com/apps/new
      #' the functions returns the table from twitter that contains the words in the word argument
      #' arguments:
      #' @param WORD word or words for search in twitter "Clinton+Trump" # Use "+" to separate query terms
      #' @param numtweets number of tweets to download
      #' @param LAT latitude area for searching
      #' @param LON longitude area for searching
      #' @param RAD radius area (in km) for search inside
      #' @param remove.retweets TRUE or FALSE, if TRUE retweets are deleted
      #' @param LANGUAGE if NULL any language, or following the ISO 639-1 codes: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes: # english:"en", spanish:"es", catalan:"ca"
      #' @param SINCE date from to start to download from twitter
      #' @param UNTIL date until finish to download from twitter
      #' @return returns a dataframe
      library(twitteR)
      library(plyr)
      
      #############################
      # APY KEYS
      # you need API keys here: https://apps.twitter.com/apps/new, also read: http://geoffjentry.hexdump.org/twitteR.pdf
      consumer_key <- "<<insert here your consumer_key>>"
      consumer_secret <- "<<insert here your consumer_secret>>"
      access_token <- "<<insert here your access_token>>"
      access_secret <- "<<insert here your access_secret>>"
      setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
      
      #############################
      # generating GEO argument
      if(is.null(LAT)|is.null(LON)|is.null(RAD)) GEO <- NULL else GEO <- paste0(LAT,",",LON,",",RAD,"km")
      
      #############################
      # check for API limits (not to execute)
      # API limits: https://dev.twitter.com/rest/public/rate-limiting
      # -->> 15 calls every 15 minutes, and 180 calls every 15 minutes.
      # getCurRateLimitInfo(c("search","application"))
      
      #############################
      # donwloading tweets
      tweets <- searchTwitter(WORD,n=numtweets,geocode=GEO,lang=LANGUAGE,since=SINCE,until=UNTIL)
      tweetsNOretweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
      if(remove.retweets==FALSE) tweets.df <- twListToDF(tweets)
      if(remove.retweets==TRUE) tweets.df <- twListToDF(tweetsNOretweets)
      
      tweets.df$latitude <- as.numeric(tweets.df$latitude)
      tweets.df$longitude <- as.numeric(tweets.df$longitude)
      
      return(tweets.df)
}

TWITTERmap <- function(tweets.df){
      # this function creates a map from the dataframe returned with function: TWITTERdownload()
      #' @param tweets.df a data.frame returned form function: TWITTERdownload()
      #' @return returns a leaflet map
      #############################
      # comments about why there are twits without GEO coordinates:
      # https://dev.twitter.com/overview/terms/geo-developer-guidelines
      
      tweets.df.MAP <- subset(tweets.df, is.na(tweets.df$latitude)==FALSE & is.na(tweets.df$longitude)==FALSE)
      if(NROW(tweets.df.MAP)==0) return()
      
      # LEAFLET MAP
      library(leaflet)
      tweets.df.MAP$POPUP <- paste0("<b>TEXT:</b> ",tweets.df.MAP$text,"<br><br><b>NAME:</b> ",tweets.df.MAP$screenName)
      colorLEVELS <- colorBin(c("#FFAE63","#ff0000","#c80000","#840084"), domain=tweets.df.MAP$retweetCount, bins=10, na.color="#808080")
      map <- leaflet() %>%
      addTiles() %>% addProviderTiles("CartoDB.Positron") %>% 
      addCircles(tweets.df.MAP$longitude, tweets.df.MAP$latitude, radius=1000, fillColor=colorLEVELS(tweets.df.MAP$retweetCount), color="black", opacity=0.7, fillOpacity=0.5, stroke=TRUE, popup=tweets.df.MAP$POPUP)
      return(map)
}

WORDCLOUDfreq <- function(tweets.df){
      # this function creates a wordcloud from the dataframe returned with function: TWITTERdownload()
      #' @param tweets.df a data.frame returned form function: TWITTERdownload()
      #' @return returns a dataframe with the frequency of words in order to execute: wordcloud2() function
      #############################
      # code extracted from:
      # https://www.r-bloggers.com/create-twitter-wordcloud-with-sentiments/
      # and:
      # http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
      # and:
      # https://www.r-bloggers.com/word-cloud-in-r/
      
      library(tm)
      library(NLP)
      library(wordcloud2)
      
      clean.text <- function(some_txt){
            # some_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
            # some_txt <- gsub("@\\w+", "", some_txt)
            # some_txt <- gsub("[[:punct:]]", "", some_txt)
            # some_txt <- gsub("[[:digit:]]", "", some_txt)
            # some_txt <- gsub("http\\w+", "", some_txt)
            # some_txt <- gsub("[ \t]{2,}", "", some_txt)
            # some_txt <- gsub("^\\s+|\\s+$", "", some_txt)
            # some_txt <- gsub("amp", "", some_txt)
            
            # define "tolower error handling" function
            try.tolower <- function(x){
                  y <- NA
                  try_error <- tryCatch(tolower(x), error=function(e) e)
                  if (!inherits(try_error, "error")) y <- tolower(x)
                  return(y)
            }

            some_txt <- sapply(some_txt, try.tolower)
            some_txt <- some_txt[some_txt != ""]
            names(some_txt) <- NULL
            return(some_txt)
      }

      ##############################################
      # Cleaning the text and make freq table
      x <- clean.text(tweets.df$text)
      x <- unlist(strsplit(x, "\n"))
      x <- gsub("\t","",x)
      x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
      x <- x[!(x %in% c("", "|"))]
      docs <- Corpus(DataframeSource(data.frame(as.character(x))))
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, function(x) removeWords(x, stopwords("english")))
      docs <- tm_map(docs, function(x) removeWords(x, stopwords("spanish")))
      docs <- tm_map(docs, function(x) removeWords(x, stopwords("catalan")))
      ap.tdm <- TermDocumentMatrix(docs)
      ap.m <- as.matrix(ap.tdm)
      ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
      ap.d <- data.frame(word = names(ap.v),freq=ap.v)
      ##############################################

      return(ap.d)
}

PLOTLYgraphs <- function(tweets.df, type=1){
      # this function creates two types of plotly graphs (type=1 or type=2)
      # from the dataframe returned with function: TWITTERdownload()
      #' @param tweets.df a data.frame returned form function: TWITTERdownload()
      #' @param type, can be 1 or 2 for switch between return plot 1 or plot 2
      #' @return returns a plotly graph object
      
      library(plotly)

      tweets.df$tag <- paste(tweets.df$created,"<br>Name: ",tweets.df$screenName,"<br>isRetweet?: ",tweets.df$isRetweet)
      
      if(type==1) {
            plot <- plot_ly(tweets.df, x=~created, y=~retweetCount, type="scatter", color=~isRetweet, colors=c("red","blue"), mode="markers", text=~tag) %>%
                                     config(displayModeBar=FALSE) %>% layout(showlegend=FALSE)
      }
      
      if(type==2){
            RETWEET <- as.data.frame(table(tweets.df$isRetweet))
            plot <- plot_ly(RETWEET, labels=~Var1, values=~Freq, type="pie", marker=list(colors=c("red","blue"))) %>%
            config(displayModeBar=FALSE) %>%
            layout(title = 'is Retweet?', autosize=FALSE, height=200, width=400,
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
      
      return(plot)
}


# examples of execution:
# TWEETS <- TWITTERdownload("coursera",numtweets=1000)
# TWITTERmap(TWEETS)
# wordfreq <- WORDCLOUDfreq(TWEETS); wordcloud2(wordfreq, size=3);
# PLOTLYgraphs(TWEETS,1)
# PLOTLYgraphs(TWEETS,2)
