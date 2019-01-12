library(shiny)
runApp(list(ui= fluidPage(
  titlePanel("opening web pages"),
  sidebarPanel(
    selectInput(inputId='test',label=1,choices=1:5)
  ),
  mainPanel(
    htmlOutput("inc")
  )
),
server = function(input, output) {
  getPage<-function() {
    return((HTML(readLines('http://twitter.com/search?q=%22Win+A+Copy%22'))))
  }
  output$inc<-renderUI({
    x <- input$test  
    getPage()
  })
})
)



r<-httr::GET("http://woeid.rosselliot.co.nz/lookup/latvia")
r<-httr::GET("http://woeid.rosselliot.co.nz/lookup/India")

httr::http_type(r)

woeid <- httr::content(r, "text")#,"parsed"))

class(woeid)

grep('Country', woeid, value = T)


woeid <-strsplit(woeid, "\t\n")

grep("Country", woeid)



pos <- grep('data-woeid', woeid[[1]])
pos2 <- grep('District-County', woeid[[1]])

country <- grep ('India', woeid[[1]][pos])

matches <- as.numeric(regmatches(woeid[[1]][pos], gregexpr("[[:digit:]]+", woeid[[1]][pos])))



tweets_cb <- searchTwitter("#coinbase", n = 3000,lang = "en")
tweets_bin <- searchTwitter("#binance", n = 3000,lang = "en")
tweets_bh <- searchTwitter("#blockchain", n = 3000,lang = "en")
#tweets_mch <- searchTwitter("#machine", n = 3000, lang = "en")

#convert list to data frame
coinbase_tweets <- twListToDF(tweets_cb)
binance_tweets <- twListToDF(tweets_bin)
blockchain_tweets <- twListToDF(tweets_bh)
#machine_tweets <- twListToDF(tweets_mch)

#extract text only
coinbase_text<- coinbase_tweets$text
binance_text<- binance_tweets$text
blockchain_text<- blockchain_tweets$text
#tech_text<- tech_tweets$text

#convert all text to lower case
coinbase_text<- tolower(coinbase_text)
binance_text<- tolower(binance_text)
blockchain_text<- tolower(blockchain_text)
#tech_text<- tolower(tech_text)

# Replace blank space ("rt")
coinbase_text <- gsub("rt", "", coinbase_text)
binance_text <- gsub("rt", "", binance_text)
blockchain_text <- gsub("rt", "", blockchain_text)
#tech_text <- gsub("rt", "", tech_text)

# Replace @UserName
coinbase_text <- gsub("@\\w+", "", coinbase_text)
binance_text <- gsub("@\\w+", "", binance_text)
blockchain_text <- gsub("@\\w+", "", blockchain_text)
#tech_text <- gsub("@\\w+", "", tech_text)


# Remove punctuation
coinbase_text <- gsub("[[:punct:]]", "", coinbase_text)
binance_text <- gsub("[[:punct:]]", "", binance_text)
blockchain_text <- gsub("[[:punct:]]", "", blockchain_text)
#tech_text <- gsub("[[:punct:]]", "", tech_text)


# Remove links
coinbase_text <- gsub("http\\w+", "", coinbase_text)
binance_text <- gsub("http\\w+", "", binance_text)
blockchain_text <- gsub("http\\w+", "", blockchain_text)
#tech_text <- gsub("http\\w+", "", tech_text)

# Remove tabs
coinbase_text <- gsub("[ |\t]{2,}", "", coinbase_text)
binance_text <- gsub("[ |\t]{2,}", "", binance_text)
blockchain_text <- gsub("[ |\t]{2,}", "", blockchain_text)
#tech_text <- gsub("[ |\t]{2,}", "", tech_text)


# Remove blank spaces at the beginning
coinbase_text <- gsub("^ ", "", coinbase_text)
binance_text <- gsub("^ ", "", binance_text)
blockchain_text <- gsub("^ ", "", blockchain_text)
#tech_text <- gsub("^ ", "", tech_text)

# Remove blank spaces at the end
coinbase_text <- gsub(" $", "", coinbase_text)
binance_text <- gsub(" $", "", binance_text)
blockchain_text <- gsub(" $", "", blockchain_text)
#tech_text <- gsub(" $", "", tech_text)

#convert into corpus type
coinbase_text.text.corpus <- Corpus(VectorSource(coinbase_text))
binance_text.text.corpus <- Corpus(VectorSource(binance_text))
blockchain_text.text.corpus <- Corpus(VectorSource(blockchain_text))

#clean up by removing stop words
coinbase_text.text.corpus <- tm_map(coinbase_text.text.corpus, function(x)removeWords(x,stopwords()))
binance_text.text.corpus <- tm_map(binance_text.text.corpus, function(x)removeWords(x,stopwords()))
blockchain_text.text.corpus <- tm_map(blockchain_text.text.corpus, function(x)removeWords(x,stopwords()))
#tech_tweets.text.corpus <- tm_map(tech_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#function(x)removeWords(x,stopwords())
# google_tweets <- removeWords(google_text,stopwords())
# amazon_tweets <- removeWords(amazon_text,stopwords())
# facebook_tweets <- removeWords(facebook_text,stopwords())
# tech_tweets <- removeWords(tech_text,stopwords())

library("wordcloud")
#generate twordcloud
wordcloud(coinbase_text.text.corpus,min.freq = 50,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(binance_text.text.corpus,min.freq = 50,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(blockchain_text.text.corpus,min.freq = 50,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
#wordcloud(tech_tweets,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)


#getting emotions using in-built function
mysentiment_coinbase<-get_nrc_sentiment((coinbase_text))
mysentiment_binance<-get_nrc_sentiment((binance_text))
mysentiment_blockchain<-get_nrc_sentiment((blockchain_text))

#calculationg total score for each sentiment
Sentimentscores_coinbase<-data.frame(colSums(mysentiment_coinbase[,]))
Sentimentscores_binance<-data.frame(colSums(mysentiment_binance[,]))
Sentimentscores_blockchain<-data.frame(colSums(mysentiment_blockchain[,]))

names(Sentimentscores_coinbase)<-"Score"
Sentimentscores_coinbase<-cbind("sentiment"=rownames(Sentimentscores_coinbase),Sentimentscores_coinbase)
rownames(Sentimentscores_coinbase)<-NULL

names(Sentimentscores_binance)<-"Score"
Sentimentscores_binance<-cbind("sentiment"=rownames(Sentimentscores_binance),Sentimentscores_binance)
rownames(Sentimentscores_binance)<-NULL

names(Sentimentscores_blockchain)<-"Score"
Sentimentscores_blockchain<-cbind("sentiment"=rownames(Sentimentscores_blockchain),Sentimentscores_blockchain)
rownames(Sentimentscores_blockchain)<-NULL

library (ggplot2)
#plotting the sentiments with scores
ggplot(data=Sentimentscores_coinbase,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on coinbase")


ggplot(data=Sentimentscores_binance,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on binance")


ggplot(data=Sentimentscores_blockchain,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on blockchain")


library (rtweet)
twitter_tokens<- create_token(app = "ShinyTwit", consumer_key = consumerKey,
                              consumer_secret = consumerSecret,
                              accessToken,
                              accessTokenSecret)

# find recent tweets with #rstats but ignore retweets
users <- search_users("#binance", n = 3000)

users$location[which(users$location=="")]<-NA

users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")



# how many locations are represented
length(unique(users$location))
## [1] 304

users %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - unique locations ")
library (dplyr)
library(tidytext)


woeid <-23424977
#-- Extracting Trends using getTrends Function
current_trends  <-  getTrends(woeid)
current_trends["trend_date"]  <-  Sys.Date()


