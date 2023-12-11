#Loading Package
library(textclean)
library(katadasaR)
library(tokenizers)
library(wordcloud)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(tidytext)
library(stopwords)
library(tm)
library(readr)
library(timeSeries)
library(graphics)
library(ggplot2)
library(ggfortify)
library(TSstudio)
library(tibble)
# import data
data <- read.csv("./datafixsentimen.csv",stringsAsFactors=TRUE)
str(data)
library(datetime)
library(xts)
data$created_at = as.date(data$created_at)
library(anytime)
datetime1 = strptime(x = as.character(data$created_at),
                     format = "%m/%d/%Y %H:%M")
data$created_at=datetime1
  #anytime(data$created_at)

##visualisasi time series 
ts_plot(data, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Distribution of Tweets regarding the Jokowi Issue 3 Periods for the last 1 Week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

help(ts_plot)
## Preprocessing Data ##
#Hilangkan redundant data
dim(data)
data_NoRed <- data %>%
  distinct(id,.keep_all = T)
glimpse(data_NoRed)
dim(data_NoRed)

#Pre-Cleaning
tweets=data_NoRed$text
clean.text = function(x)
{
  # remove rt
  x = gsub("RT", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  #remove hashtag
  x = gsub('#\\S+', "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # convert to lower case
  x = tolower(x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

cleantweet=clean.text(tweets)
#Coba liat berubah ngga
tweets[2]
cleantweet[2]

#Kembalikan tweet yang pre-cleaning ke Nored
data_NoRed$text=cleantweet

# Pilih atribut yang dianalsis
data_selected <- data_NoRed %>%
  select(id, screen_name, text,retweet_count, favorite_count)


# Mengubah data dalam bentuk corpus
data_corpus <- VCorpus(VectorSource(data_selected$text))

# atur meta untuk data corpus
meta(data_corpus, "retweetCount") <- data_selected$retweetCount
meta(data_corpus, "favoriteCount") <- data_selected$favoriteCount

#import stopwords
file <- "https://raw.githubusercontent.com/masdevid/ID-Stopwords/master/id.stopwords.02.01.2016.txt"
stop_words <- read.delim(url(file))
names(stop_words) <- "word"
stop_words <- as_tibble(stop_words)
stop_words <-stop_words$word

# Jangan jadi anak alay udah gede !
urlfile = "https://raw.githubusercontent.com/nasalsabila/kamus-alay/master/colloquial-indonesian-lexicon.csv"
KamusAlaySiAmron <- read_csv(url(urlfile))
KamusAlaySiAmron <- KamusAlaySiAmron %>%
  select(slang)
KamusAlaySiAmron <- KamusAlaySiAmron$slang
KamusAlaySiAmron <- KamusAlaySiAmron[sample(length(KamusAlaySiAmron), 700)]

kamusManual <- c("amp","bocil","uf","ri","lha","wowiek","wii","watimptes","wahufufuf","waeeuymau","vu","usulanperiodeufufufufuf","upsufd","rb","abg","aang","abcdefg","ads","aerufuf","agaaain","ahahaha","ahdodol","ahhga","ahihi","ajg","aksijd","am","ancoooorrrrancoooorrrr","anjay","ayangrondeya","ayakayak","b","bacotanya","bag","bagir","bahn","bapakuf","barucc",
                 "dur","uf","mrk","uaufef","gcrg","udddudfcudfudfcududf","uddeudfcudf","uddudfududee","udeudeeudfududeeud","yaufuf","yauufd","yahc","yaaufaufaufa","yaaahhentuuuttt","xixixi","wt","anjerr","anjai","anjiir","anjirrrrrrrrrrrrrr","apaak","arabufufuf","atasuffuffuff","bahlulhartanticimin","banggk","bangunstadionsirkuitlayak",
                 "udeudfudfbudfudfudfudfudeeudfbudeeud","sbykm","ufa","ufuf","aa","lha","konstitusiufaa","banyaaakufufufe","zkzkzkz","zhzowhe","ytuff","ygjt","basiketum","batuufuf","bcotin","bcra","beeuhhh","begooo","benarufdufdufd","berhatihatilahudufefudufef","berpotensiperiodeuf","bersihuf","bet","bf","bgst","bib","binjayuffufd","blablabla",
                 "blansak","blas","blenek","blgbyk","bngst","bongufufufuf","boni","bray","buk","bucucu","c","cie","cia","ckpsj","cnbc","cobaufuf","coyyufufufd","cz","dajjallue","dajuta","dbca","dehhhh","uffufe","ufufufuf","n","p","oh","ufufufufuf","uaaufee","i","ufaufa","ahyufufd","z","u")

# jadi cleaning service

cleaning <- function(data) {
  data <- tm_map(data, content_transformer(function(x) gsub("\\.|\\,", " ", x, perl=T)))
  data <- tm_map(data, content_transformer(function(x) gsub("pic.twitter.com/\\S+", "", x, perl=T)))
  data <- tm_map(data, content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T)))
  data <- tm_map(data, removePunctuation)
  data <- tm_map(data, content_transformer(tolower))
  data <- tm_map(data, content_transformer(function(x) gsub(x, pattern = "@([A-Za-z0-9_]+)",replacement="")))
  data <- tm_map(data, content_transformer(function(x) gsub(x, pattern = "#([A-Za-z0-9_]+)",replacement="")))
  data <- tm_map(data, removeNumbers)
  data <- tm_map(data, removeWords, stop_words)
  data <- tm_map(data, removeWords, KamusAlaySiAmron)
  data <- tm_map(data, removeWords, kamusManual)
  data <- tm_map(data, stripWhitespace)
  return(data)
}

clean_corpus <- cleaning(data_corpus)


# Kembalikan menjadi bentuk timbel eh tibble
tidy_data <- tidy(clean_corpus)
head(tidy_data)
data_NoRed$text[14044]
data$text[14058]
# rapihkan dong, kan udah gede
tidy_data <- tidy_data %>%
  select(id, text)
data_rapih <- data_selected
data_rapih$text <- tidy_data$text
tweet=data_rapih$text


# Lakukan tokenizing (beli token listrik) pada atribut tweet
data_rapih <- data_rapih %>%
  unnest_tokens(word,text)

#Lakukan Stemming [OPTONAL] ## Kalo Obs > 10000 mending ga usah
kata <- data_rapih$word
kata_rapih <- lapply(kata, katadasaR)    

# Cek Token Kata yang dihasilkan
count_word<- data_rapih %>%
  count(word, sort = T)
view(count_word)
write.csv(count_word,
          "D:/AAS/BEM 2022/Riset/Bahan dari Mila/frekuensiwordcloud.csv",
          row.names=F)

# Visualisasi # Note : sesuaikan freq dengan data kalian
wordcloud(words = count_word$word, freq = count_word$n,
          min.freq = 50,
          max.words=1000,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Kalo udah aman datanya, udah bersih berkilau tanpa noda, bisa di eksport ke CSV.
write.csv(data_rapih,
          "D:/AAS/BEM 2022/Riset/Bahan dari Mila/datarapihjokowi.csv",
          row.names=F)
write.csv(data_selected,
          "D:/AAS/BEM 2022/Riset/Bahan dari Mila/databersihjokowi.csv",
          row.names=F)

###LABELLING

library(translateR)
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tm)
library(tibble)
library(dplyr, warn.conflicts = F)
library(SnowballC)
library(lubridate)
#install.packages("RTextTools")

#import document
data <- read.csv("databersihjokowi.csv")
#data$id= as.character(id)
attach(data)
str(data)


# Data hasil translate
library(readxl)
data_tweet <- read_excel("data_tweet10_en (1).xlsx")
attach(data_tweet)
#data_tweet$id= as.character(id)

data_tweet <- data_tweet %>%
  rename(tweet_en = `tweet-id`)

## filter tweet bahasa inggris ##
# fungsi penampah stop words
stop_words<- tidytext::stop_words
custom_stop_words <- bind_rows(tibble(word = c("NA","t.co","csun","blm","rt","https",
                                               "BLM","blacklivesmatter","black",
                                               "georgefloyd","2",'#blm','#blacklivesmatter',
                                               '#georgefloyd','august','n7opjxesad','3','ehfb1yavka',
                                               'zvkphi1g8g','0085','2063','0093','0094'), 
                                      lexicon = c("custom")), stop_words)
#tambah stop_word jika diperlukan

#custom <- tambah_stopword(stopword_baru,
# stopword = stop_words)
# Hapus Stop Words 

tweet_en1 <- data_tweet %>%
  select(-tweet) %>%
  rename(tweet_en = `tweet-id`) %>%
  unnest_tokens(word, tweet_en) %>%
  anti_join(custom_stop_words)

# cek apakah ada stopword baru yang harus dihapus
tweet_en1 %>%
  count(word, sort = T) %>%
  view()

## Steming ##
stemmed_tweet <- tweet_en1 %>%
  mutate(word = wordStem(word))

# lupa belum pake screen_name

tweet_ready <- stemmed_tweet %>%
  inner_join(data, by = "id") %>%
  select (id, screen_name, word)

str(data)
str(stemmed_tweet)
# memberikan label polarity
polarity <- tweet_ready %>%  
  inner_join(get_sentiments("bing")) %>%  
  group_by(screen_name) %>%  
  count(sentiment, sort = T)  

# memberikan label emotion
tanpa_pos_neg <- tweet_ready %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(screen_name) %>%
  filter(sentiment != "negative") %>%
  filter(sentiment != "positive") %>%
  count(sentiment, sort = T) %>%
  view ()


emotion <- tweet_ready %>%
  inner_join(tanpa_pos_neg) %>%
  group_by(screen_name)
  
# memberikan label score emotion 
  score_emotion <- tweet_ready %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(screen_name) %>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment)

## Modelling ##

emosi <- emotion %>%
  mutate(trust = ifelse(sentiment == "trust", TRUE, FALSE)) %>%
  mutate(fear = ifelse(sentiment == "fear", TRUE, FALSE)) %>%
  mutate(anger = ifelse(sentiment == "anger", TRUE, FALSE)) %>%
  mutate(disgust = ifelse(sentiment == "disgust", TRUE, FALSE)) %>%
  mutate(anticipation = ifelse(sentiment == "anticipation", TRUE, FALSE)) %>%
  mutate(joy = ifelse(sentiment == "joy", TRUE, FALSE)) %>%
  mutate(surprise = ifelse(sentiment == "surprise", TRUE, FALSE)) %>%
  arrange(id)

################     ANALISIS SENTIMEN (POSITIF/NETRAL/NEGATIF)   #########################################################################################################
library(readr)

data <- read_csv("databersihjokowi.csv")
View(data)
library(stringr)
library(plyr)
tweet=data$text


pos = scan('positive.txt', what='character')
neg = scan('negative.txt', what='character')

pos.words = c(pos, "setuju", "Indonesiabutuhkerja","dukung","optimis", "satuju")
neg.words = c(neg, "tolakomnibuslaw", "tolakruuciptakerja", "tolak", "cancel", "hentikan", "dianteup")

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



#################################################################################################################################################
####----------------------------NAIVE BAYES----------------------------------####
# Load required libraries
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
list_of_values <- c("Positif", "Negatif")
New.Result <- filter(New.Result, sentiment %in% list_of_values)
New.Result %>%
  count(sentiment, sort = T)
New.Result$sentiment <- as.factor(New.Result$sentiment)
data_corpus <- VCorpus(VectorSource(New.Result$text))
# Inspect the corpus
data_corpus

dtm <- DocumentTermMatrix(data_corpus)
dim(dtm)

#SAMPLE
Sample <- sample(1:12741, 1000)
df.test <- New.Result[Sample,]
df.train <- New.Result[-Sample,]


dtm.train <- dtm[-Sample,]
dtm.test <- dtm[Sample,]

corpus.train <- data_corpus[-Sample]
corpus.test <- data_corpus[Sample]

#FEATURE SELECTION
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
## [1] 2089

# Use only 5 most frequent words (fivefreq) to build the DTM
dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  6000 2089

dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(dictionary = fivefreq))

dim(dtm.test.nb)
## [1]  6000 2089
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Train the classifier
classifier <- naiveBayes(trainNB,df.train$sentiment , laplace = 2)
# Use the NB classifier we built to make predictions on the test set.
pred <- predict(classifier, newdata= testNB)
# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$sentiment )
# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$sentiment)

conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']

