setwd("~/ShinyTwit")
source('pcg.R')

#Get working directory, check for TwitLib folder
mainDir <- getwd()
subDir <- "TwitLib"

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

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

#Create empty library, if it does not exist alredy
l<-list.files ()
hashtagLib <- paste(hashtagSearch, '.csv', sep = "")

if (hashtagLib %in% l){
  datas <- read.csv (hashtagLib)
} else {
  datas <- NULL
}
  
sleep_for_a_minute <- function() { Sys.sleep(900) }
start_time <- Sys.time()

#sys.time()
c <- 0

while(abs(as.numeric(difftime(start_time, Sys.time(), units = 'secs')))<(2000))
{
  dat <- searchTwitter(hashtagSearch, n = 3000,lang = "en")
  dat <- twListToDF(dat)
  datas <- rbind (datas, dat)
  datas <- unique(datas)
  c = c+1
  print (paste0('Call: ', c))
  if (c%%3 == 0) {
    sleep_for_a_minute ()
  }
}

datas$text <- tolower(datas$text)
datas$text <- gsub("rt", "", datas$text)
datas$text <- gsub("@\\w+", "", datas$text)
datas$text <- gsub("[[:punct:]]", "", datas$text)
datas$text <- gsub("http\\w+", "", datas$text)
datas$text <- gsub("[ |\t]{2,}", "", datas$text)
datas$text <- gsub("^ ", "", datas$text)
datas$text <- gsub(" $", "", datas$text)
datas$text[which(datas$text == "")]<- NA
datas <- datas[complete.cases(datas[,1]),]
datas$created[which(datas$created == "")] <- NA
datas <- datas[complete.cases(datas[,5]),]
datas <- unique(datas)

#datas$text <- removeWords(datas$text, stopwords())
write.table(datas, file = hashtagLib, sep = ",", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

#0dat <- searchTwitter(hashtagSearch, n = 3000,lang = "en", since='2011-03-01', until='2011-03-02')

