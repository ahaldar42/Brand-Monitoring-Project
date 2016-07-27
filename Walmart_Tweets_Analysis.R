library("twitteR")

# Declare Twitter API Credentials
api_key <- "L0sjvk5OTXhvLsVo9HLc1ScPy"
api_secret<-"0nynNRTIToFVyXvOfRsahxpEd0Skgab2TCZjCPrmEsyb048av3"
token<-"2400155744-2RHsPjkeRNSM7GHxZCMl3VtNCivwZUA2vT3PWbP"
token_secret<-"6jXnWQ4aCexfgkHbpcCEDUo9EvBkxEXEAZ4N1XHCNx7uS"

#Create Twitter connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)
#Should get message "Using direct authentication
#----------------------------------------------------------------------------------------------
#New york 40.71° N, 74.01° W
Walmart.tw.NY<-searchTwitter("#Walmart",n=5000, lang="en",geocode='40.71,-74.01,100mi')
saveRDS(Walmart.tw.NY, 'WalTws0717_NY_100m.RDS')
#----------------------------------------------------------------------------------------------
##LA 34.05, -118.24,
Walmart.tw.LA<-searchTwitter("#Walmart",n=5000, lang="en",geocode = '34.05,-118.24,100 mi')
saveRDS(Walmart.tw.LA, 'WalTws0717_LA_100m.RDS')
#----------------------------------------------------------------------------------------------
#SF 37.78° N, -122.42° SF
Walmart.tw.SF<-searchTwitter("#Walmart",n=5000, lang="en",geocode='37.78,-122.42,100mi')
saveRDS(Walmart.tw.SF, 'WalTws0717_SF_100m.RDS')
#----------------------------------------------------------------------------------------------
##Houston 29.76, -95.37,
Walmart.tw.Hou<-searchTwitter("#Walmart",n=5000, lang="en",geocode = '29.76,-95.37,100 mi')
saveRDS(Walmart.tw.Hou, 'WalTws0717_Hou_100m.RDS')
#----------------------------------------------------------------------------------------------
##Chicago 41.88, -87.63
Walmart.tw.Chic<-searchTwitter("#Walmart",n=5000, lang="en",geocode = '41.88,-87.63,100 mi')
saveRDS(Walmart.tw.Chic, 'WalTws0717_Chic_100m.RDS')
#----------------------------------------------------------------------------------------------
##Salt Lake City 40.76, -111.89
Walmart.tw.SLC<-searchTwitter("#Walmart",n=5000, lang="en",geocode = '40.76,-111.89,100 mi')
saveRDS(Walmart.tw.SLC, 'WalTws0717_SLC_100m.RDS')
#----------------------------------------------------------------------------------------------
##Philly 39.95 -75.17
Walmart.tw.Phil<-searchTwitter("#Walmart",n=5000, lang="en",geocode = '39.95,-75.17,100 mi')
saveRDS(Walmart.tw.Phil, 'WalTws0717_Phil_100m.RDS')
#----------------------------------------------------------------------------------------------
######################################################################################
#                                Starting working on data
########################################################################################

################                    New York

########################################################################################         
#library("twitteR")
library('lubridate')
library("ggplot2")

library(stringr)
library(tm)
library(wordcloud)

library(syuzhet)

######## NY ##############################################################          
tw.NY <- readRDS('WalTws0717_NY_100m.RDS') 
d.NY <- twListToDF(tw.NY) 

#no of tweets vs day
g <- ggplot(data = d.NY, aes(x = created)) 
plot.time <- g +
        geom_histogram(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        labs( x= "Time",y="Number of tweets",title = "Tweets over time (New_York)")+
        scale_fill_gradient(low = "lightblue", high = "midnightblue")
plot(plot.time)
dev.copy(png, file="1.TwvsTime_NY.png", width=720, height=480)
dev.off()

#tweets vs retweets
plot.rtw <- ggplot(d.NY, aes(created,fill=isRetweet))+
        geom_density(alpha = .5) +
        xlab('All tweets')+labs(title='Tweets vs Retweets (New_York)')
plot(plot.rtw) 
dev.copy(png, file="2.TwvsRwt_NY.png", width=720, height=480)
dev.off()

#############  Little more analysis #########################################
###Trying to figure out the chatter

####### For NY ##############################################################
nohandles_NY <- str_replace_all(d.NY$text, "@\\w+", "")
wordCorpus_NY <- Corpus(VectorSource(nohandles_NY))
wordCorpus_NY <- tm_map(wordCorpus_NY, removePunctuation)
wordCorpus_NY <- tm_map(wordCorpus_NY, content_transformer(tolower))
wordCorpus_NY <- tm_map(wordCorpus_NY, removeWords, stopwords("english"))
wordCorpus_NY <- tm_map(wordCorpus_NY, removeWords,c("amp","walmart"))
wordCorpus_NY <- tm_map(wordCorpus_NY, removeWords, "http\\w+")
wordCorpus_NY <- tm_map(wordCorpus_NY, stripWhitespace)

set.seed(123)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
wordcloud(words = wordCorpus_NY, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)
dev.copy(png, file="4.WordCloud_NY.png", width=720, height=480)
dev.off()

############ NY #################################################
d.NY.text<-d.NY$text
d.NY.text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", d.NY.text)
d.NY.text <- gsub("[[:punct:]]", "", d.NY.text)
d.NY.text = gsub("http\\w+", "", d.NY.text)

Walmart.Sentiment <- get_nrc_sentiment(d.NY.text)

d.NY.text <- cbind(d.NY.text, Walmart.Sentiment)
sentimentTotals <- data.frame(colSums(d.NY.text[,c(2:11)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "none") +
        xlab("Sentiment") + ylab("Total Count") + 
        ggtitle("Total Sentiment Score for All Tweets (New_York)")

dev.copy(png, file="5.Sentiment_NY.png", width=720, height=480)
dev.off()


##Most angry tweet
d.NY.text[which.max(d.NY.text$anger),1]
#walmart Lantern fix for Russellville power outage and 
#marriott Fairfield Inn with bad emergency lightning 

d.NY.text[which.max(d.NY.text$negative),1]
#BREAKING Bomb threat prompts evacuation of Walmart

d.NY.text[which.max(d.NY.text$anticipation),1]
# How Walmarts mobile strategy paid off in a national deployment of Walmart Pay

########################################################################################
 
################                    Los Angeles

########################################################################################         
tw.LA <- readRDS('WalTws0717_LA_100m.RDS') 
d.LA <- twListToDF(tw.LA) 

#no of tweets vs day
g <- ggplot(data = d.LA, aes(x = created)) 
plot.time <- g +
        geom_histogram(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        labs( x= "Time",y="Number of tweets",title = "Tweets over time (Los_Angeles)")+
        scale_fill_gradient(low = "lightblue", high = "midnightblue")
plot(plot.time)
dev.copy(png, file="1.TwvsTime_LA.png", width=720, height=480)
dev.off()

#tweets vs retweets
plot.rtw <- ggplot(d.LA, aes(created,fill=isRetweet))+
        geom_density(alpha = .5) +
        xlab('All tweets')+labs(title='Tweets vs Retweets (Los_Angeles)')
plot(plot.rtw) 
dev.copy(png, file="2.TwvsRwt_LA.png", width=720, height=480)
dev.off()

#############  Little more analysis #########################################

# library(stringr)
# library(tm)
# library(wordcloud)

####### For LA ##############################################################
nohandles_LA <- str_replace_all(d.LA$text, "@\\w+", "")
wordCorpus_LA <- Corpus(VectorSource(nohandles_LA))
wordCorpus_LA <- tm_map(wordCorpus_LA, removePunctuation)
wordCorpus_LA <- tm_map(wordCorpus_LA, content_transformer(tolower))
wordCorpus_LA <- tm_map(wordCorpus_LA, removeWords, stopwords("english"))
wordCorpus_LA <- tm_map(wordCorpus_LA, removeWords,c("amp","walmart"))
wordCorpus_LA <- tm_map(wordCorpus_LA, removeWords, "http\\w+")
wordCorpus_LA <- tm_map(wordCorpus_LA, stripWhitespace)

set.seed(123)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
wordcloud(words = wordCorpus_LA, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)
dev.copy(png, file="3.WordCloud_LA_1.png", width=720, height=480)
dev.off()

wordCorpus_LA <- tm_map(wordCorpus_LA, removeNumbers)
wordCorpus_LA <- tm_map(wordCorpus_LA, stripWhitespace)
wordcloud(words = wordCorpus_LA, min.freq=25, scale=c(5,0.1), max.words=75, random.order=FALSE, 
          rot.per=0.25, use.r.layout=FALSE, colors=pal)
dev.copy(png, file="4.WordCloud_LA_2.png", width=720, height=480)
dev.off()

############ LA #################################################
d.LA.text<-d.LA$text
d.LA.text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", d.LA.text)
d.LA.text <- gsub("[[:punct:]]", "", d.LA.text)
d.LA.text = gsub("http\\w+", "", d.LA.text)

Walmart.Sentiment <- get_nrc_sentiment(d.LA.text)

d.LA.text <- cbind(d.LA.text, Walmart.Sentiment)
sentimentTotals <- data.frame(colSums(d.LA.text[,c(2:11)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "none") +
        xlab("Sentiment") + ylab("Total Count") + 
        ggtitle("Total Sentiment Score for All Tweets (Los_Angeles)")

dev.copy(png, file="5.Sentiment_LA.png", width=720, height=480)
dev.off()
########################################################################################

##Most angry tweet
d.LA.text[which.max(d.LA.text$anger),1]
#Black ppl cannot exercise the right to walk around with AR15 assault weapons 
#black man murder n WalMart buying play gun for his son-LA


d.LA.text[which.max(d.LA.text$anticipation),1]
#Late repost but I cant wait to share my success story 
#Walmartshareholders WalMart summer16 Armystrong God1st 

########################################################################################



