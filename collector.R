setwd("~/ShinyTwit")
source('pcg.R')

#Get working directory, check for TwitLib folder
mainDir <- getwd()
subDir <- "TwitLib"

#Check if folder exists
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

#Readin csv file and a key word
csv_hashtags <- 'hashtagsList.csv'
hashtagSearch <- 'blockchain'

if (file.exists(csv_hashtags)){
  lib <- read.csv (csv_hashtags)
  if (hashtagSearch %in% lib[,1]) {} else {
    write.table(hashtagSearch, file = csv_hashtags, sep = ",", append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
  }
} else {
  write.csv (hashtagSearch, file = csv_hashtags, sep = ",", col.names = F, row.names = F)
}

#Create empty library, if it does not exist
hashtagLib <- paste(hashtagSearch, '.csv', sep = "")

l <- list.files()
if (hashtagLib %in% l){
  dat <- read_twitter_csv (hashtagLib, unflatten = FALSE)
} else {
  dat <- NULL
  datas <- NULL
}

sleep_for_a_minute <- function() { Sys.sleep(420) }
start_time <- Sys.time()

print (start_time)
#counter
c <- 0

while(abs(as.numeric(difftime(start_time, Sys.time(), unit = 'secs')))<(2100))
{
  datas <- search_tweets(hashtagSearch, n = 4000,lang = "en")
  c = c+1
  print (paste0('Call: ', c))
  datas$text <- tolower(datas$text)
  datas$text <- gsub("rt", "", datas$text)
  datas$text <- gsub("@\\w+", "", datas$text)
  datas$text <- gsub("[[:punct:]]", "", datas$text)
  datas$text <- gsub("http\\w+", "", datas$text)
  datas$text <- gsub("[ |\t]{2,}", "", datas$text)
  datas$text <- gsub("^ ", "", datas$text)
  datas$text <- gsub(" $", "", datas$text)
  datas$text <- qdap::rm_stopwords(datas$text, separate = FALSE) #remove stop words
  datas$created_at <- as.character(datas$created_at)
  dat <- rbind (dat, datas)
  dat <- dat[!duplicated(dat[,'text']),]
  dat$text <- gsub("[^A-Za-z0-9 ]","",dat$text)
  sleep_for_a_minute ()
}

write_as_csv(dat, hashtagLib, prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")

# 
# strptimeD <- function (x) {
#   return (strptime(x, "%m/%d/%Y %H:%M"))
# }
# 
# xasDF <- data.frame(dat)
# d <- xasDF$created_at
# d1 <- d[1:36577]
# d1 <- strptimeD(d1)
# d1 <- as.character(d1)
# 
# d2 <- d[36578:38745]
# d2 <- as.POSIXct(as.numeric(d2), origin="1970-01-01")
# d2 <- as.character(d2)
# 
# d3 <- d[38746:63521]
# d3 <- strptime(d3, "%Y-%m-%d %H:%M:%S")
# d3 <- as.character(d3)
# dt <- c(d1,d2,d3)
# 
# xasDF$created_at <- dt
# dat$created_at<-dt
# 
# dat <- dat[complete.cases(dat[ ,3]),]
# 
# xasDF.na <- xasDF[complete.cases(xasDF[ ,3]),]
# print (dim(xasDF.na))

#d[1:36577,3]<-apply (d[1:36577,3],, strptimeD)

#d[1:36577]<-strptimeD(d[1:36577])
