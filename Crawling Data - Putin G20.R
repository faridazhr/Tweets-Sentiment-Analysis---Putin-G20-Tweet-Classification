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
search.string1 <-"Rusia G20"
search.string2 <-"Putin G20"


no.of.tweets <- 30000

## Rusia G20
search1.Tweets <- searchTwitter(search.string1, n=no.of.tweets,lang="en",)

df_rusiaG20 <- do.call("rbind", lapply(search1.Tweets, as.data.frame))
#View(df_tigaperiode)

data_rusiaG20 = cbind(df_rusiaG20[5], df_rusiaG20[11], df_rusiaG20[1])
View(data_rusiaG20)


## Putin G20
search2.Tweets <- searchTwitter(search.string2, n=no.of.tweets,lang="en",)

df_putinG20 <- do.call("rbind", lapply(search2.Tweets, as.data.frame))


data_putinG20 = cbind(df_putinG20[5], df_putinG20[11], df_putinG20[1])
View(data_putinG20)




created = rbind(df_rusiaG20[5], df_putinG20[5])
names = rbind(df_rusiaG20[11], df_putinG20[11])
tweets = rbind(df_rusiaG20[1], df_putinG20[1])


rawData = cbind(created, names, tweets)
View(rawData)

write_xlsx(rawData,"D:\\Raw Data-PutinG20.xlsx")

write.csv(rawData,"D:\\Raw Data.csv", row.names = FALSE)
