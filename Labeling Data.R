################     ANALISIS SENTIMEN (POSITIF/NETRAL/NEGATIF)   #########################################################################################################
library(readr)
setwd('D:\\Perkuliahan\\Project')
data <- read_csv("text-clean.csv")
View(data)
library(stringr)
library(plyr)
tweet=data$text


#pos = scan('positive.txt', what='character')
#neg = scan('negative.txt', what='character')

pos.words = c("setuju", "dukung","optimis", 'kawal', 'pembangunan', "lebihbaik")
neg.words = c("tolak", "cancel", "hentikan", "sengsara", "menolak", "sebagiankecil")

getSentimentScore = function(sentences, pos.words, neg.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    #remove digit, punctuation, dan special/control character:
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    
    #convert semua teks menjadi lowercase:
    sentence = tolower(sentence)
    
    #pisahkan setiap kalimat menggunakan spasi (space delimiter):
    words = unlist(str_split(sentence, "\\s+"))
    
    #lakukan boolean match dari setiap kata-kata menggunakan pos &amp;amp;amp; neg opinion-lexicon:
    pos.matches = !is.na(match(words, pos.words))
    neg.matches = !is.na(match(words, neg.words))
    
    #score sentimen = total positive sentiment - total negative:
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  #return data frame berisi kalimat beserta sentimennya:
  return(data.frame(text = sentences, score = scores))
}

Result = getSentimentScore(tweet,pos.words,neg.words)

library(dplyr)
New.Result <- Result %>%
  filter(score != 0)


New.Result$score
New.Result %>%
  count(score)


library(plyr)
New.Result$score <- as.factor(New.Result$score)
New.Result$sentiment = revalue(New.Result$score,c('1'="Positif",'2'="Positif",'3'="Positif",'4'="Positif",'5'="Positif",
                                                  '-1'="Negatif",'-2'="Negatif",'-3'="Negatif",'-4'="Negatif",'-5'="Negatif"
                                                  ,'-6'="Negatif",'-7'="Negatif",'-8'="Negatif",'-9'="Negatif",'-10'="Negatif"))

New.Result %>%
  count(sentiment, sort = T)


#function for plotting
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=sentiment)) + 
    geom_bar(aes(y=..count.., fill=sentiment)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}

New.Result %>%
  count(score)
count(New.Result$sentiment)

plotSentiments1(New.Result, "Sentiment Analysis of Tweets on Twitter about Jokowi 3 Periode")
plot(sentiment)

plotSentiments2 <- function(sentiment_dataframe, title)
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=sentiment)) +
    geom_bar(aes(y=..count.., fill=sentiment)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Words") +
    xlab("Polarity Categories")
}

#plotting tweets polarity
plotSentiments2(tanpa_pos_neg, "Polarity Analysis of Tweets on Twitter about Jokowi 3 Periode")

##Rekap Emosi
rekapemosi=tanpa_pos_neg%>%
  group_by(sentiment)%>%
  count(n)
rekapemosi=data.frame(tanpa_pos_neg$sentiment)
attach(rekapemosi)
hitungemosi1=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='anger', sort = T);hitungemosi1
hitungemosi2=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='anticipation', sort = T);hitungemosi2
hitungemosi3=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='trust', sort = T);hitungemosi3
hitungemosi4=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='disgust', sort = T);hitungemosi4
hitungemosi5=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='sadness', sort = T);hitungemosi5
hitungemosi6=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='fear', sort = T);hitungemosi6
hitungemosi7=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='joy', sort = T);hitungemosi7
hitungemosi8=rekapemosi%>%
  count(tanpa_pos_neg.sentiment=='surprise', sort = T);hitungemosi8

rekapemotion=data.frame(hitungemosi1,hitungemosi2,hitungemosi3,hitungemosi4,hitungemosi5,
                        hitungemosi6,hitungemosi7,hitungemosi8)
rekapemotion