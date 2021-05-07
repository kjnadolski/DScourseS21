## Project Rough Draft
## ECON 5253: Data Science for Economists 

library(tidyverse)
library(rvest)
library(janitor)
library(leaflet)
library(purrr)

      # Scraping HTML data not formatted in a table - Craigslist Apartment listings in OKC area
      # Based off of the Tutorial on: https://nkaza.github.io/post/2020-02-04-scraping-craigslist-posts/
      
      location <- 'oklahomacity'
      bedrooms <- 2
      bathrooms <- 1
      min_sqft <- 900
      
      baseurl <- paste0("https://", location, ".craigslist.org/search/apa")
      # craigslist URLs are specific to the city of inquiry
      
      # Build out the query
      queries <- c("?")
      queries <- c(queries, paste0("bedrooms=", bedrooms))
      queries <- c(queries, paste0("bathrooms=", bathrooms))
      queries <- c(queries, paste0("minSqft=", min_sqft))
      
      query_url <- paste0(baseurl, queries[1], paste(queries[2:length(queries)], collapse = "&"))
      
          # query craigslist!
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
          
          # Extracting attributes from each result-row: ID, Title, Price, Bedrooms, Date, Square Footage, and Locale
      
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
        html_text()
      
      prices <- gsub(",", "", prices)
      
      prices <- 
        prices %>%
        str_extract("[0-9]+") %>%
        as.numeric()
      
      bdrms <- 
        raw_ads %>%
        html_node(".housing") %>%
        html_text() %>%
        str_extract(".*br") %>%
        str_extract("[0-9]") %>%
        as.numeric()
      
      dates <-
        raw_ads%>%
        html_node('time') %>%
        html_attr('datetime')   
      
      locales <-
        raw_ads %>%
        html_node(".result-hood") %>%
        html_text()
      
      sqft <- 
        raw_ads %>%
        html_node('.housing') %>%
        html_text() %>%
        str_extract(".*?ft2") %>%
        str_extract("[0-9]+") %>%
        as.numeric()
      
      
      craigslist <- data.frame(ids, titles, prices, bdrms, dates, locales, sqft)
      View(craigslist)
      
      # Scraping individual ad postings for extra information not included on the initial search results
      # the map_dfr() function operates like a for loop, iteratively assessing the function for each url in the urls vector
      # Add Locations of the apartments/houses by scraping each individual ad for the location attributes
      urls <-
        raw_ads %>%
        html_node(".result-title") %>%
        html_attr("href")
      
      
      latlongs <- 
        map_dfr(urls, function(x){
          xml2::read_html(x) %>% 
            html_node("#map") %>%
            html_attrs() %>%
            t() %>%
            as_tibble() %>%
            select_at(vars(starts_with("data-"))) %>%
            mutate_all(as.numeric)
        }
        )
      
      # scraping the description from each individual ad's URL
      descriptions <- 
        map_dfr(urls, function(x){
          read_html(x) %>%
            # SELECTOR: #postingbody
            html_nodes("#postingbody") %>%
            html_text() %>%
            as_tibble()
        }
        )
      
          craigslist <- data.frame(ids, titles, prices, bdrms, dates, locales, sqft, descriptions, latlongs)
          View(craigslist)

      
        # Creating a loop to scrape all of the entries off of different pages 
            # Based off of the tutorial from: https://medium.com/swlh/exploring-san-francisco-apartments-on-craigslist-with-r-43e5fa38a77b
        loopn <- seq(from = 121, to = 3000, by = 120)
        
        for(i in loopn) {
          Sys.sleep(5) # delays each query by 5 seconds
          queriesloop <- queries
          
          # Add offset to URL in intervals of 120
          queriesloop <- c(queries, paste0("s=", i))
          query_url <- paste0(baseurl,queriesloop[1], paste(queriesloop[2:length(queriesloop)], collapse = "&"))
        
          # query craigslist!
          raw_query <- xml2::read_html(query_url)
        
          ## Select out the listing ads
          raw_ads <- html_nodes(raw_query, "li.result-row")
        
        # Extracting attributes from each result-row: ID, Title, Price, Date, Square Footage, and Locale
        
            ids <- raw_ads %>% html_attr('data-pid')
            titles <- raw_ads %>% html_node("a.result-title") %>% html_text()
            prices <- raw_ads %>% html_node("span.result-price") %>% html_text()
              prices <- gsub(",", "", prices)
              prices <- prices %>% str_extract("[0-9]+") %>% as.numeric()
            bdrms <- raw_ads %>% html_node(".housing") %>% html_text() %>% str_extract(".*br") %>% str_extract("[0-9]") %>% as.numeric()
            dates <- raw_ads%>% html_node('time') %>% html_attr('datetime')   
            locales <- raw_ads %>% html_node(".result-hood") %>% html_text()
            sqft <- raw_ads %>% html_node('.housing') %>% html_text() %>% str_extract(".*?ft2") %>% str_extract("[0-9]+") %>% as.numeric()
        
              # Scraping individual ad postings for extra information not included on the initial search results
                  # the map_dfr() function operates like a for loop, iteratively assessing the function for each url in the urls vector
              # Add Locations of the apartments/houses by scraping each individual ad for the location attributes
              urls <- raw_ads %>% html_node(".result-title") %>% html_attr("href")
              latlongs <-  map_dfr(urls, function(x){
                  xml2::read_html(x) %>% 
                    html_node("#map") %>%
                    html_attrs() %>%
                    t() %>%
                    as_tibble() %>%
                    select_at(vars(starts_with("data-"))) %>%
                    mutate_all(as.numeric)
                }
                )
               
               # scraping the description from each individual ad's URL
                  descriptions <- 
                    map_dfr(urls, function(x){
                      read_html(x) %>%
                        # SELECTOR: #postingbody
                        html_nodes("#postingbody") %>%
                        html_text() %>%
                        as_tibble()
                    }
                  )
            
                  
            craigslistloop <- data.frame(ids, titles, prices, bdrms, dates, locales, sqft, descriptions, latlongs)
            
            craigslist <- rbind(craigslist, craigslistloop)
            # RBIND posts in each loop separately to the master craigslist data frame
        }
          View(craigslist)
          
          names(craigslist)[names(craigslist) == "value"] <- "descriptions"
          View(craigslist)
          
          
          # Create an observation that is rent per square footage
          craigslist$rentbysqft <- craigslist$prices/craigslist$sqft
        
            # cleaning the descriptions (getting rid of any extraneous HTML language, numbers, punctuation)
            craigslist$descriptions <- gsub("QR Code Link to This Post", "", craigslist$descriptions)
            craigslist$descriptions <- gsub("\n","", craigslist$descriptions)
            craigslist$descriptions <- gsub("/","", craigslist$descriptions)
            craigslist$descriptions <- gsub("[0-9]","", craigslist$descriptions)
            craigslist$descriptions <- gsub("$","", craigslist$descriptions)
            craigslist$descriptions <- gsub("%","", craigslist$descriptions)
            craigslist$descriptions <- gsub(",","", craigslist$descriptions)
            craigslist$descriptions <- gsub("|","", craigslist$descriptions)
            craigslist$descriptions <- gsub("@.*","", craigslist$descriptions)
            craigslist$descriptions <- gsub("!","", craigslist$descriptions)
            craigslist$descriptions <- gsub("\t", "", craigslist$descriptions)
            
            
        # Quantifying description sentiment (organized by reference dictionary used)
            
            descriptions <- data.frame(craigslist$ids, craigslist$descriptions)
            View(descriptions)
            
            library(SentimentAnalysis)
            sentiment <- analyzeSentiment(craigslist$descriptions)
            # adding sentiment analysis observations to craigslist dataframe
                craigslist$wordcount <- sentiment$WordCount
                craigslist$sentimentGI <- sentiment$SentimentGI
                craigslist$p.GI <- sentiment$PositivityGI
                craigslist$sentimentHE <- sentiment$SentimentHE
                craigslist$p.HE <- sentiment$PositivityHE
                craigslist$sentimentLM <- sentiment$SentimentLM
                craigslist$p.LM <- sentiment$PositivityLM
                craigslist$sentimentQDAP <- sentiment$SentimentQDAP
                craigslist$p.QDAP <- sentiment$PositivityQDAP
                craigslist$binary <- convertToBinaryResponse(sentiment)$SentimentQDAP
                craigslist$direction <- convertToDirection(sentiment)$SentimentQDAP
                
                
            # performing sentiment analysis on TITLES
                title.sentiment <- analyzeSentiment(craigslist.clean$titles)
                craigslist.clean$t.wordcount <- title.sentiment$WordCount
                craigslist.clean$t.sentimentGI <- title.sentiment$SentimentGI
                craigslist.clean$t.p.GI <- title.sentiment$PositivityGI
                craigslist.clean$t.sentimentHE <- title.sentiment$SentimentHE
                craigslist.clean$t.p.HE <- title.sentiment$PositivityHE
                craigslist.clean$t.sentimentLM <- title.sentiment$SentimentLM
                craigslist.clean$t.p.LM <- title.sentiment$PositivityLM
                craigslist.clean$t.sentimentQDAP <- title.sentiment$SentimentQDAP
                craigslist.clean$t.p.QDAP <- title.sentiment$PositivityQDAP
                craigslist.clean$t.binary <- convertToBinaryResponse(title.sentiment)$SentimentQDAP
                craigslist.clean$t.direction <- convertToDirection(title.sentiment)$SentimentQDAP
            
            # Understanding the Read out: 
              # DictionaryGI - Dictionary with opinionated words from the Harvard-IV dictionary as used in the General Inquirer software
              # DictionaryHE - Dictionary with opinionated words from Henry's Financial dictionary
              # DictionaryLM - Dictionary with opinionated words from Loughran-McDonald Financial dictionary
              # DictionaryQDAP - qdap package
            
            # cleaning the dataset to exclude duplicate entries with different id numbers
                unique(craigslist$descriptions) # want to exclude duplicate titles (some rental companies will create identical listings with different id numbers and time stamps)
                sum(duplicated(craigslist$descriptions)) # 705
                craigslist.clean <- craigslist[!(duplicated(craigslist$descriptions)),]
                View(craigslist.clean)
                
                # cleaning the dataset to exclude a single outlier with monthly rent = 50,000
                  sum(craigslist.clean$prices > 25000) # 1
                    # this outlier has most likely come from a typo in the data, it would not be reasonable for an apartment in OKC to have a rent greater than $25,000 per month
                  sum(craigslist.clean$prices == 0) # 61
                    # it would also be unreasonable for an apartment in OKC to have a rent of $0, these observations most likely came formatting that had the rent price stored in a different area of the listing (in the picture or the description, for example)
                  craigslist.clean  <- craigslist.clean[!craigslist.clean$prices == 0,]
                  craigslist.clean <- craigslist.clean[!craigslist.clean$prices > 25000,]
                  
            # Visualizing certain observations
                hist(craigslist.clean$wordcount, breaks = 10)
                hist(craigslist.clean$sentimentHE)
                hist(craigslist.clean$sentimentGI)
                hist(craigslist.clean$sentimentLM)
                hist(craigslist.clean$sentimentQDAP)
                
                hist(craigslist.clean$sqft)
                hist(log(craigslist.clean$prices), breaks = 3, xlim= c())
                hist(craigslist.clean$bdrms, breaks = 4)
                
                max(craigslist.clean$prices) # $5300
                min(craigslist.clean$prices) # $175
                
            # Visualizing relationships
                par(mfrow=c(3,2))
                
                plot(craigslist.clean$wordcount, craigslist.clean$prices, xlab = "Word Count", ylab= "Prices")
                abline(lm(craigslist.clean$prices ~ craigslist.clean$wordcount), col = "blue")
                
                plot(craigslist.clean$direction, craigslist.clean$prices, xlab = "Sentiment", ylab= "Prices")
      
                plot(craigslist.clean$t.binary, craigslist.clean$prices, xlab = "Title Sentiment", ylab= "Prices")
                
                plot(craigslist.clean$sqft, craigslist.clean$prices, xlab = "Sq. Footage", ylab= "Prices")
                abline(lm(craigslist.clean$prices ~ craigslist.clean$sqft), col = "blue")
                
                plot(craigslist.clean$bdrms, craigslist.clean$prices, xlab = "Bedrooms", ylab= "Prices")
                
                plot(craigslist.clean$sentimentQDAP, craigslist.clean$prices, xlab = "QDAP Sentiment", ylab= "Prices")
                abline(lm(craigslist.clean$prices~craigslist.clean$sentimentQDAP), col="blue")
                
            # Descriptive Stats
                median(craigslist.clean$prices)
                median(craigslist.clean$sqft)
                
                mean(craigslist.clean$rentbysqft)
                median(craigslist.clean$rentbysqft)
                
                max(craigslist.clean$wordcount)
                min(craigslist.clean$wordcount)
                
                
            # Visualizing Latitude and Longitude 
                names(craigslist.clean)
                
                leaflet(craigslist.clean) %>%
                  addProviderTiles(providers$Stamen.TonerLines, group = "Basemap") %>%
                  addProviderTiles(providers$Stamen.TonerLite, group = "Basemap") %>%
                  addCircles(lat = ~`data.latitude`, lng = ~`data.longitude`, label = paste(craigslist.clean$ids, craigslist.clean$locales, craigslist.clean$prices, sep=",")
                  )
                
            
    # Building regression models
          # Research Question: What is the premium of a positive description on craigslist?
            
          # Sentiment Analysis Variables: 
              # wordcount
              # sentimentQDAP
              # t.wordcount
              # t.direction
            
          # Craigslist Variables: 
              # log(rent)
              # sqft
              # bdrms
            
            
    # OLS estimation: 
        library(MASS)
                
        # setting up a variable for log(prices)
                craigslist.clean$logrent <- log(craigslist.clean$prices)
                craigslist.clean$sqft100 <- craigslist.clean$sqft/100
                
                craigslist.clean$p.zscore <- ((craigslist.clean$p.QDAP - mean(craigslist.clean$p.QDAP))/sd(craigslist.clean$p.QDAP))
        
        ols.craigslist <- lm(logrent ~ sqft100 + bdrms + p.zscore + wordcount + t.wordcount + t.direction, data = craigslist.clean)
        irls.craigslist <- rlm(logrent ~ sqft100 + bdrms + p.zscore + wordcount + t.wordcount + t.direction, data = craigslist.clean)
            summary(ols.craigslist)
            summary(irls.craigslist)
            
            # To interpret linear regression with a log transformed outcome variable: 
                # exponentiate the coefficient (exp(beta_hat)), subtract one from this number, multiply by 100
                # this will give the % increase or decrease in the response for every one-unit increase in the independent variable
            
            # using lm() for more analysis of Linear Models 
            est.LM <- lm(prices ~ sqft + p.LM, data = craigslist.clean)
            est.QDAP <- lm(prices ~ sqft + p.QDAP, data = craigslist.clean)
            est.GI <- lm(prices ~ sqft + p.GI, data = craigslist.clean)
            est.HE <- lm(prices ~ sqft + p.HE, data = craigslist.clean)
            est.wc <- lm(prices ~ sqft + wordcount, data = craigslist.clean)
            est.sentiment <- lm(prices ~ sqft + wordcount + sentimentQDAP + t.direction, data = craigslist.clean)
            est.bdrms <- lm(prices ~ bdrms, data = craigslist.clean)
            
            summary(est.LM)
            summary(est.QDAP)
            summary(est.GI)
            summary(est.HE)
            summary(est.wc)
            summary(est.sentiment)
            summary(est.bdrms)
        