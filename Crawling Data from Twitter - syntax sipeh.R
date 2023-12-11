###Crawling Data Menggunakan R

##Tahap 1 Proses Autentifikasi di Twitter
#Install package:
install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("expss")
install.packages("writexl")

#Jika sudah pernah install sebelumnya, cukup running syntax berikut:
library(twitteR)
library(ROAuth)
library(RCurl)
library(expss)
library("writexl")

#Download sertifikat dari curl
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

#Meminta izin kepada twitter dengan meruning berikut:
reqURL <-"https://api.twitter.com/oauth/request_token"
accessURL <-"https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "w8kdUkmkcjRl9vcpdvIjqWoYx"
CUSTOMER_SECRET <-"7pdUR1Lo75ma1CZhhxhqOBSVI40OUDDDUE8RsiJhkZJjn3WSe2"
ACCESS_TOKEN <- "1252153761113485312-viIqgiQx6M3Y5rICXwRUegLZHJQOlI"
ACCESS_Secret <-"ZqCrzLUkSvA99G75gdLFu3aUUW0MVU9Hp32gZqeXC79RX"


#Men setup autorization
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_Secret)


##Tahap 2 Pengambilan Data dari Twitter
#Mengambil tweet tentang corona dengan filter language versi bahasa indonesia
search.string1 <-"tiga periode"
search.string2 <-"3 periode"
search.string3 <-"perpanjangan masa jabatan"
search.string4 <-"tigaperiode"
search.string5 <-"3periode"
search.string6 <-"absolute power"
search.string7 <-"perpanjangan kekuasaan"


no.of.tweets <- 30000


#Versi bahasa indonesia

## Tiga Periode
search1.Tweets <- searchTwitter(search.string1, n=no.of.tweets,lang="id",)

df_tigaperiode <- do.call("rbind", lapply(search1.Tweets, as.data.frame))
#View(df_tigaperiode)

data_tigaperiode = cbind(df_tigaperiode[5], df_tigaperiode[11], df_tigaperiode[1])
View(data_tigaperiode)

## 3 Periode
search2.Tweets <- searchTwitter(search.string2, n=no.of.tweets,lang="id",)

df_3periode <- do.call("rbind", lapply(search2.Tweets, as.data.frame))
#View(df_3periode)

data_3periode = cbind(df_3periode[5], df_3periode[11], df_3periode[1])
View(data_3periode)

## Perpanjangan Masa Jabatan (PMJ)
search3.Tweets <- searchTwitter(search.string3, n=no.of.tweets,lang="id",)

df_pmj <- do.call("rbind", lapply(search3.Tweets, as.data.frame))
#View(df_pmj)

data_pmj = cbind(df_pmj[5], df_pmj[11], df_pmj[1])
View(data_pmj)





## tigaPeriode
search4.Tweets <- searchTwitter(search.string4, n=no.of.tweets,lang="id",)

df_tigaperiode_ <- do.call("rbind", lapply(search4.Tweets, as.data.frame))
#View(df_tigaperiode)

data_tigaperiode_ = cbind(df_tigaperiode_[5], df_tigaperiode_[11], df_tigaperiode_[1])
View(data_tigaperiode_)

## 3Periode
search5.Tweets <- searchTwitter(search.string5, n=no.of.tweets,lang="id",)

df_3periode_ <- do.call("rbind", lapply(search5.Tweets, as.data.frame))
#View(df_3periode)

data_3periode_ = cbind(df_3periode_[5], df_3periode_[11], df_3periode_[1])
View(data_3periode_)

## Absolute Power
search6.Tweets <- searchTwitter(search.string6, n=no.of.tweets,lang="id",)

df_absP <- do.call("rbind", lapply(search6.Tweets, as.data.frame))
#View(df_pmj)

data_absP = cbind(df_absP[5], df_absP[11], df_absP[1])
View(data_absP)


## Perpanjangan Kekuasaan (PK)
search7.Tweets <- searchTwitter(search.string7, n=no.of.tweets,lang="id",)

df_pk <- do.call("rbind", lapply(search7.Tweets, as.data.frame))
#View(df_pmj)

data_pk = cbind(df_pk[5], df_pk[11], df_pk[1])
View(data_pk)





created = rbind(df_tigaperiode[5], df_3periode[5], df_pmj[5], df_tigaperiode_[5], df_3periode_[5], df_absP[5])
names = rbind(df_tigaperiode[11], df_3periode[11], df_pmj[11], df_tigaperiode_[11], df_3periode_[11], df_absP[11])
tweets = rbind(df_tigaperiode[1], df_3periode[1], df_pmj[1], df_tigaperiode_[1], df_3periode_[1], df_absP[1])


rawData = cbind(created, names, tweets)
View(rawData)

write_xlsx(rawData,"D:\\Raw Data-3.0.xlsx")

write.csv(rawData,"D:\\Raw Data.csv", row.names = FALSE)
