#Website link - https://apps.twitter.com/

library("twitteR") #Load library
#Setting Connection with Twitter API
consumer_key<- "oIzwT8sZWFzA6Oozko6NFr2f0"
consumer_secret<- "HAQkTT3A5fIzzOkQFKjmjUJDzPC6KsrcXXXWo5Q29zgVii6U4D"
access_token<- "1580567246258442241-FtaInOgKeaaAwrX1o2rpMBQGCyDiCd"
access_secret<- "bpgo7K8fjd7YDaWi8nFWFJqg7V5V2exQYfFfbdhbTFQ2H" 

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

#Pulling tweets
tweets<- searchTwitter('Samsung', n=1000, lang = 'en')
tweets
Samsung.df <- twListToDF(tweets) #Tweets to data frame format

library(xlsx)
write.xlsx(Samsung.df,"C:/Users/jadha/Desktop/R Files/tweets_smnsn.xlsx") #Setting csv file directory
str(tweets)

#Build corpus
library(tm) #text mining
corpus<- iconv(Samsung.df$text, to="UTF-8")
corpus<- Corpus(VectorSource(corpus))
inspect(corpus[1:10])

#Data Cleaning
corpus<- tm_map(corpus,tolower) #Convert to lower case
inspect(corpus[1:10])

corpus<- tm_map(corpus, removePunctuation) #Remove punctuation
inspect(corpus[1:10])

corpus<- tm_map(corpus, removeNumbers) #Remove numbers
inspect(corpus[1:10])

cleanset<- tm_map(corpus,removeWords, stopwords('english')) #Remove common English words
inspect(cleanset[1:10])

removeURL<- function(x) gsub('http[[:alnum:]]*','',x)
cleanset<- tm_map(cleanset, content_transformer(removeURL)) #Remove URLs
inspect(cleanset[1:5])

cleanset<- tm_map(cleanset, removeWords, c('samsung')) #Remove specific word
cleanset<- tm_map(cleanset, stripWhitespace) #Remove white spaces
inspect(cleanset[1:5])

# Term document matrix
tdm<- TermDocumentMatrix(cleanset)
tdm
tdm<-as.matrix(tdm) #Convert tdm to matrix
tdm[1:10, 1:20]

#Bar Plot
w<- rowSums(tdm) #Display words with their frequency
w<- subset(w,w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))
#Word Cloud
library(wordcloud)
w<- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq=w,
          max.words = 150,
          random.order = F,
          min.freq=5,
          colors=brewer.pal(8,'Dark2'),
          scale=c(5,0.3),
          rot.per =0.7 )
library(wordcloud2)
w<- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           size=0.5,
           shape = 'globe',
           rotateRatio = 0.5,
           minSize = 1)
#Sentiment Analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#Read File
apple<- read.csv(file.choose(),header = T)
tweets<- iconv(Samsung.df$text, to="UTF-8")

#Obtain Sentiment Scores
s<- get_nrc_sentiment(tweets)
head(s)
head('snapdragon')
#Bar Plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Samsung Tweets')
