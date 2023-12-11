library(devtools)
library(tau)
library(rtweet)
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library("textclean")
library("tokenizers")
library("stopwords")
library("Rstem")
library(twitteR)
library(tm)
library(NLP)
library(SentimentAnalysis)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(sentiment)
library(katadasaR)
library(caret)
library(recipes)
library(ArchR)
library(parallel)


#Deskripsi data
keyword="Jokowi 3 Periode"
jumlahtweet=100
type="recent"
bahasa="id"

some_tweets <- searchTwitter(keyword,n=jumlahtweet,type=type,lang=bahasa,retryonratelimit=FALSE)
tweets=some_tweets[,c("status_id","created_at","screen_name","text","retweet_text","retweet_count","favorite_count")]
write.csv(tweets,file="D:/AAS/BEM 2022/Riset/data4.csv",row.names=TRUE)