library ('data.table')
library ('tm')
library ('syuzhet')
library ('SnowballC')
library ('dygraphs')
library ('quantmod')
library ('ggplot2')
library ('readr')
library ('dplyr')
library ('lubridate')
library ('parallel')
library ('corrplot')
#read in big table
x <- fread ('blockchain.csv')

cl <- detectCores() - 1 # or 
cl <- makeCluster(cl)
clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))

#subset only first 50000 characters
x_text <- x$text

#measure a time of processing for sentiment analysis of twits (6 min)
#can be parallelized in the function
Sys.time()
sentims <- get_nrc_sentiment((x_text), cl = cl)
stopCluster (cl)
Sys.time()

#create a data frame with (hours)
datas <- cbind(x$created_at, sentims)
colnames(datas)[1] <- 'Date'

#extract hours of the Date
datas <- datas %>% 
  mutate(hour_of_day = hour(as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S"))))

#extract day of the Date
datas <- datas %>% 
  mutate(day = day(as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S"))))

#mean by hours for entire data set
y_h <- datas %>%
  mutate(hour_of_day = hour(as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S")))) %>%
  group_by(hour_of_day) %>%
  summarise_all(funs(mean)) #summarize all columns

y_d <- datas %>%
  mutate(hour_of_day = hour(as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S")))) %>%
  group_by(day) %>%
  summarise_all(funs(mean)) #summarize all columns

#get the mean of the column by day & hour
y <- datas %>% 
  mutate(hour_of_day = hour(as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S")))) %>% 
  group_by(hour_of_day, day) %>% 
  summarise_all(funs(mean)) #summarize all columns
  #summarise(meanValue = mean(anticipation)) #summarize particular column


#sort data frame by hour of the day
y <- y[with(y, order( day,hour_of_day)),]


#get correlation
M <- cor (y[,4:ncol(y)])

#plot corrplot
corrplot (M, method = 'pie', type = 'upper')
