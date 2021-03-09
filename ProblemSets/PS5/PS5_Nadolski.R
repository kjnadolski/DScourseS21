## Karley Nadolski
## Econ 5253
## Problem Set 5 - Webscraping

library(tidyverse)
library(rvest)
library(janitor)
library(leaflet)

# 3. Find a webpage that has data that interests you, or that might be useful
#    to you, but which doesn't have an API. 
      
      # Loading in Data from the Wikipedia Page: List of Unusual deaths
          deaths <- read_html("https://en.wikipedia.org/wiki/List_of_unusual_deaths")
    
      # Scraping in data from unusual deaths during the 1990s
        # SELECTOR: #mw-content-text > div.mw-parser-output > table:nth-child(29)
          deaths_1990s <- 
            deaths %>%
            html_node("#mw-content-text > div.mw-parser-output > table:nth-child(29)") %>% ## select table element
            html_table() %>% ## convert to a data table
            clean_names()
          
          View(deaths_1990s)
          
          
        # Scraping HTML data not formatted in a table - Craigslist Apartment listings in OKC area
          # Based off of the Tutorial on: https://nkaza.github.io/post/2020-02-04-scraping-craigslist-posts/
          
          location <- 'oklahomacity'
          bedrooms <- 2
          bathrooms <- 2
          min_sqft <- 900
          
          baseurl <- paste0("https://", location, ".craigslist.org/search/apa")
              # craigslist URLs are specific to the city of inquiry
          
          # Build out the query
          queries <- c("?")
          queries <- c(queries, paste0("bedrooms=", bedrooms))
          queries <- c(queries, paste0("bathrooms=", bathrooms))
          queries <- c(queries, paste0("minSqft=", min_sqft))
          
          query_url <- paste0(baseurl,queries[1], paste(queries[2:length(queries)], collapse = "&"))
          
          raw_query <- xml2::read_html(query_url)
          
          raw_query
              # READ OUT
              # {html_document}
              # <html class="no-js">
              # [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF ...
              # [2] <body class="search has-map">\n    <script type="text/javascript"><!--\  ...
          
          ## Select out the listing ads
          raw_ads <- html_nodes(raw_query, "li.result-row")
          raw_ads %>% head()
              # {xml_nodeset (6)}
              # [1] <li class="result-row" data-pid="7282603359">\n\n        <a href="https:/ ...
              # [2] <li class="result-row" data-pid="7287981545">\n\n        <a href="https:/ ...
              # [3] <li class="result-row" data-pid="7287978930" data-repost-of="701627389 ...
              # [4] <li class="result-row" data-pid="7281886333">\n\n        <a href="https:/ ...
              # [5] <li class="result-row" data-pid="7284873176">\n\n        <a href="https:/ ...
              # [6] <li class="result-row" data-pid="7285867557">\n\n        <a href="https:/ ...
          
        # Extracting attributes from each result-row: ID, Title, Price, Date and Locale
          
          ids <-
            raw_ads %>%
            html_attr('data-pid')
          
          titles <-
            raw_ads %>%
            html_node("a.result-title") %>%
            html_text()
          
          prices <-
            raw_ads %>% 
            html_node("span.result-price") %>%
            html_text() %>%
            str_extract("[0-9]+") %>%
            as.numeric()

          dates <-
            raw_ads%>%
            html_node('time') %>%
            html_attr('datetime')   
          
          locales <-
            raw_ads %>%
            html_node(".result-hood") %>%
            html_text()
            
          craigslist <- data.frame(ids, titles, prices, dates, locales)
          View(craigslist)
          
# 4. Find a website or other data source that hosts an API.
      # Use the API to generate a table of data that is interesting/meaningful to you in some way. 
          library(twitteR)
          
          requestURL = "https://api.twitter.com/oauth/request_token"
          accessURL = "https://api.twitter.com/oauth/access_token"
          authURL = "https://api.twitter.com/oauth/authorize"
          consumerKey = Sys.getenv("TWIT_API_KEY")
          consumerSecret = Sys.getenv("TWIT_API_KEYS")
          
          accessToken = Sys.getenv("TWIT_ACCESS")
          accessSecret = Sys.getenv("TWIT_ACCESSS")
          
          
          setup_twitter_oauth(consumerKey,
                              consumerSecret,
                              accessToken,
                              accessSecret)
          
          # Search Twitter for tweets in Norman with the word 'vaccine'
            # Use this website to find location code: http://www.geocodesource.com/browse/location/us/ok/12185
          
          tweets <- searchTwitter('vaccine', 
                                  geocode='35.2225685120,-97.4394760132,20mi',  
                                  n=500, retryOnRateLimit=1)
          # Converting a list of tweets to a dataframe
          tweets.df <- twListToDF(tweets) 
          View(tweets.df)
          
          # Cleaning the data for any text analysis - isolating the text of the tweets and cleaning them
          tweets.df.clean <- gsub("http.*","",tweets.df$text)
          tweets.df.clean <- gsub("https.*","",tweets.df.clean)
          tweets.df.clean <- gsub("#.*","",tweets.df.clean)
          tweets.df.clean <- gsub("@.*","",tweets.df.clean)
          tweets.df.clean <- gsub("RT","",tweets.df.clean)
          View(tweets.df.clean)
          