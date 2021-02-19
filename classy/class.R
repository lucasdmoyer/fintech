

######################
##Project 1 FIN 6392##
######################
#change interface
install.packages("rmarkdown")
library(rmarkdown)
#load packages
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("DescTools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("finreportr")
install.packages("pdfetch")
install.packages("readr")
install.packages("corrplot")
install.packages("sqldf")
install.packages("olsrr")
library(olsrr)
library(sqldf)
library(tidyverse)
library(caret)
library(corrplot)
library(readr)
library(pdfetch)
library(finreportr)
library(quantmod)
library(PerformanceAnalytics)
library(DescTools)
library(dplyr)
library(tidyr)


#Set time period
start_date <- "2016-01-01"
start_date_tech <- "2015-11-12"
end_date <- "2020-01-01"
period <- "2016-01-01/2020-01-01"
period_google_trend <- "2016-01-01 2020-01-01"
date_list <- c("2018-12-31","2019-01-31","2019-02-28","2019-03-29","2019-04-30","2019-05-31","2019-06-28"
               ,"2019-07-31","2019-08-30","2019-09-30","2019-10-31","2019-11-27","2019-12-30")#trading day


#Choose a portfolio of securities
#Appl, aal, WFC, nvda,wmt,dis, UVxy, XLE, BTC-USD, LTC-USD
sequrities = c('AAL', 'AAPL', 'WFC', 'NVDA', 'WMT', 'DIS','UVXY', 'XLE', 'BTC-USD', 
               'LTC-USD')#,"CL=F","GC=F"
Securites_KW <- data.frame(c("American Airlines","AAL")
                           ,c("Apple","AAPL")
                           ,c("Wells Fargo","WFC")
                           ,c("Nvidia","NVDA")
                           ,c("Walmart","WMT")
                           ,c("Disney","DIS")
                           ,c("UVXY","UVXY")
                           ,c("XLE","XLE")
                           ,c("Bitcoin","BTC-USD")
                           ,c("LTC","LTC-USD")
                           
)

##retrieval the daily trading data (Stock)

# Stock Performance Chart
stickers = c('AAL', 'AAPL', 'WFC', 'NVDA', 'WMT', 'DIS','UVXY', 'XLE', 'BTC-USD', 
             'LTC-USD')
getSymbols(stickers)

chartSeries(AAL,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(AAPL,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(NVDA,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(WMT,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(DIS,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(WFC,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(`BTC-USD`,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

chartSeries(`LTC-USD`,subset='2016-01::2019-01',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

##Retrieve the fundamental data you deem relevant

# Macroeconomics data

getSymbols(c("DGS10","CPIAUCSL","UNRATE","GDP","NETEXP"),src="FRED")
macros <- UNRATE[period]
macros$DGS10<-DGS10[period]
macros$CPIAUCSL<-CPIAUCSL[period]
macros$GDP<-GDP[period]
macros$NETEXP<-NETEXP[period]
macros.df <- data.frame(date=index(macros), coredata(macros))
#Add crude oil and gold to macros.df
oil <- data.frame(pdfetch_YAHOO("CL=F", fields="adjclose",from=start_date_tech, to=end_date, interval= "1d"))
oil <- add_rownames(oil, var = "date")
oil$date <- as.Date(oil$date)
gold <- data.frame(pdfetch_YAHOO("GC=F", fields="adjclose",from=start_date_tech, to=end_date, interval= "1d"))
gold <- add_rownames(gold, var = "date")
gold$date <- as.Date(gold$date)
macros.df <- left_join(macros.df, oil, by=c("date"))
macros.df <- left_join(macros.df, gold, by=c("date"))
head(macros.df)
macros.df$UNRATE[2] <- UNRATE["2016-01-01"]###################################ATTENTION for further revise
macros.df$CPIAUCSL[2] <- CPIAUCSL["2016-01-01"]#
macros.df$GDP[2] <- GDP["2016-01-01"]#
macros.df$NETEXP[2] <- NETEXP["2016-01-01"]#

head(macros.df) # We will do data imputing later


#Load big five factors
setwd("D:/courses/FIN 6392")
big_five <- read.table('F-F_Research_Data_5_Factors_2x3_daily.txt', header = TRUE, sep = "", dec = ".")
big_five <- add_rownames(big_five, var = "date")
big_five$date <- as.Date(big_five$date, "%Y%m%d")
tail(big_five)#last day is 2020-08-31, which is later than our ending day, so we can use this dataset


#Extract the technical analysis indicators
# Adjusted Closing Price (Without weekend data)
# Retrieve technical indicators
technical_indicators <- function(val){
  df <- data.frame(pdfetch_YAHOO(val, fields="adjclose",
                                 from=start_date_tech, to=end_date, interval= "1d"))
  return <- c(NA, diff(df[,1])/df[1:(length(df[,1])-1),1]*100)
  df <- add_rownames(df, var = "date")
  security <- rep(val,length(df$date))
  sma20 <- SMA(df[,2],n=20)
  ema14 <- EMA(df[,2],n=14)
  bb20 <- BBands(df[,2], sd=2.0)
  rsi14 <- RSI(df[,2], n=14)
  macd <- MACD(df[,2], nFast=12, nSlow=26, nSig=9, maType=SMA)
  df <- cbind(security,df,return,sma20,ema14,bb20,rsi14,macd)#
  names(df) <- c("security","date","price","return","sma20","ema14","bb20_dn","bb20_mavg","bb20_up","bb20_pctB","rsi14","macd","macd_signal")#
  df$date <- as.Date(df$date)
  return(df)
}

df_total = data.frame()
for (val in sequrities)
{
  df <- technical_indicators(val)
  df_total <- rbind(df_total,df)
}
head(df_total)
df_total <- df_total[-c(1: 34),]#Drop extra rows so that the data start from 2016-01-04
head(df_total)

##Retrieve data from social media (e.g. twitter or reddit or other social media sources)
# Reddit Analyze
library(RedditExtractoR)
library(sentimentr)
library(dplyr)


#############################TIME WARNING!!!(BELOW)#######################################

# Create lists of securities, tickers, target subreddit.
assets_list <- c('American Airlines', 'Apple', 'Wells Fargo', 
                 'Nvidia', 'Walmart', 'Disney', 'UVXY',
                 'XLE', 'Bitcoin', 'Litecoin')

ticker <- c('AAL', 'AAPL', 'WFC', 'NVDA', 'WMT', 'DIS',
            'UVXY', 'XLE', 'BTC-USD', 'LTC-USD')

target_subr <- c('wallstreetbets', 'thetagang', 'investing', 
                 'stocks', 'options', 
                 'CryptoCurrency', 'CryptoMarkets', 
                 'Bitcoin', 'btc', 'litecoin')

# Get Reddit of the first 8th securities
mid_df <- data.frame()
RedditText_df <- data.frame()
for (x in 1:8) {
  print(assets_list[x])
  for (y in 1:5) {
    tem_df <- data.frame()
    tem_df <- get_reddit(assets_list[x], subreddit = target_subr[y], 
                         page_threshold = 100, sort_by="new")
    mid_df <- rbind(mid_df, tem_df)
  }
  name_col <- rep(ticker[x], nrow(mid_df))
  mid_df <- cbind(name_col, mid_df)
  RedditText_df <- rbind(RedditText_df, mid_df)
  mid_df <- data.frame()
}

# Get Reddit of Bitcoin, Litecoin
mid_df <- data.frame()
for (x in 9:10) {
  for (y in 6:10) {
    tem_df <- data.frame()
    tem_df <- get_reddit(assets_list[x], subreddit = target_subr[y], page_threshold = 100, sort_by="new")
    mid_df <- rbind(mid_df, tem_df)
  }
  name_col <- rep(ticker[x], nrow(mid_df))
  mid_df <- cbind(name_col, mid_df)
  RedditText_df <- rbind(RedditText_df, mid_df)
  mid_df <- data.frame()
}

# Change the Date Format
RedditText_df$post_date <- RedditText_df$post_date %>% as.Date('%d-%m-%y')
RedditText_df$comm_date <- RedditText_df$comm_date %>% as.Date('%d-%m-%y')

# Delete the URL details
RedditText_rd <- subset(RedditText_df, select = -c(17:19))

# Filter Data by Date
RedditText_rd <- RedditText_rd %>% subset(post_date > as.Date("2015-12-31"))
RedditText_rd <- RedditText_rd %>% subset(post_date < as.Date("2019-01-02"))

# Save the Raw Reddit Data
write.csv(RedditText_rd, "RedditAnalytics.csv")

# Sentiment
post_sentiment <- sentiment(RedditText_rd$post_text)
comment_sentiment <- sentiment(RedditText_rd$comment)

# View the Head of Sentiment Data
head(post_sentiment)
head(comment_sentiment)

# Change the Column Name
colnames(post_sentiment)[1] <- 'id'
colnames(comment_sentiment)[1] <- 'id'

# Combine Comment Sentiment with Securities by ID
Reddit_sentiment_raw <- left_join(RedditText_df[1,2], post_sentiment, by = 'id')
Reddit_sentiment <- left_join(Reddit_sentiment_raw, comment_sentiment, by = 'id')

# Sum the sentiment factors by Securities and Date
Reddit_fin <- Reddit_sentiment %>% 
  group_by(name_col, comm_date) %>% 
  summarise(Total = sum(sentiment, na.rm = TRUE))

# Save the Reddit Sentiment Data
write.csv(Reddit_fin, "Reddit_sum.csv")
#############################TIME WARNING!!!(ABOVE)#######################################

#Here is the outcome of Reddit Sentiment Analysis
#############################PLEASE RUN THE FOLLOWING CODE INSTEAD#######################
RedditSentiment_sum <- read.csv("Reddit_sum_1003.csv")
head(RedditSentiment_sum,20)
names(RedditSentiment_sum)[names(RedditSentiment_sum) == "comm_date"] <- "date"
RedditSentiment_sum$date <- as.Date(RedditSentiment_sum$date)
names(RedditSentiment_sum)[names(RedditSentiment_sum) == "name_col"] <- "security"
names(RedditSentiment_sum)[names(RedditSentiment_sum) == "Total"] <- "reddit_sentiment"
RedditSentiment_sum <- select(RedditSentiment_sum,-c(1))
head(RedditSentiment_sum)

##Google Trends Data
install.packages("gtrendsR")  
install.packages("dplyr")
install.packages("tibbletime")
install.packages("lubridate")
library(gtrendsR)
library(dplyr)
library(tibbletime)
library(lubridate)
## write a function to get web query activity
google_trend <- function(keyword,security_name) {
  df <- data.frame(gtrends(
    keyword = keyword,
    geo = "US",
    time = period_google_trend,
    gprop = c("web", "news", "images", "froogle", "youtube"),
    hl = "en-US",
    cookie_url = "http://trends.google.com/Cookies/NID",
    onlyInterest = TRUE
  ))
  df <- df[,1:2]
  Security <- rep(security_name,length(df$interest_over_time.date))
  Hits_scale <- scale(df$interest_over_time.hits)
  df <- cbind(Security, df,Hits_scale[,1])
  names(df) <- c("security","date","hits","hits_scale")
  return(df)
}

##Retrieve google trend data
google_trend_total = data.frame()
for (val in Securites_KW)
{
  df <- google_trend(val[1],val[2])
  google_trend_total <- rbind(google_trend_total,df)
}
head(google_trend_total)

#Change weekend to weekdays, so that the Google Trend data can match with the technical analysis data
length_google_trend <- length(google_trend_total$security)+1
i=1
while(i<length_google_trend)
{
  if (IsWeekend(as.Date(google_trend_total$date[i])) == TRUE) {
    google_trend_total$date[i] <- google_trend_total$date[i]- lubridate::days(2)
  }
  i=i+1
}
head(google_trend_total)


###Merge final dataframe
total <- left_join(df_total, macros.df, by=c("date"))
total <- left_join(total, google_trend_total, by=c("date","security"))
head(total)

#impute missing value with previous value (macro data & google trend data)
total$UNRATE <- LOCF( total$UNRATE)
total$DGS10 <- LOCF( total$DGS10)
total$CPIAUCSL <- LOCF( total$CPIAUCSL)
total$GDP <- LOCF( total$GDP)
total$NETEXP <- LOCF( total$NETEXP)
total$hits <- LOCF( total$hits)
total$hits_scale <- LOCF( total$hits_scale)

#Add big five data and drop the rows where RF is NA
total <- left_join(total, big_five, by=c("date"))
length(total$date)#11310
total <- na.omit(total, cols="RF")
length(total$date)#9946

# Excess return calculation and factors scale
total$ExcRet<-total$return-total$RF
head(total)
for (i in 5:20){total[,i]<-scale(total[,i])}
head(total)
tail(total)


#Add a column of Excess return in t+1 day
total$ExcRet_t1 <- c(total$ExcRet[2:length(total$ExcRet)],NA)
head(total)

#delete data of last date of each securities in the dataset
tail(total)
typeof(total$date)#"double"
total$date <- as.Date(total$date)
total <- total[total$date < "2019-12-31", ]###################################ATTENTION for further revise
tail(total)

length(total$ExcRet)#9936


#Add Reddit Sentiment and impute missing value with 0
head(RedditSentiment_sum)
total <- left_join(total, RedditSentiment_sum, by=c("date","security"))
total$reddit_sentiment[is.na(total$reddit_sentiment)] <- 0
#total[210:220,]

##write to csv
write.csv(total,"C:/FIN 6392.001 - Financial Technology and Blockchain/pj1\\merge_2016-2019.csv", row.names = FALSE)
total <- read.csv("merge_2016-2019.csv")



# Regression Analysis
lm1 <- lm(total$ExcRet_t1 ~ total$sma20+total$ema14+total$bb20_dn+total$rsi14
          +total$macd+total$UNRATE+total$DGS10+total$CPIAUCSL+total$GDP
          +total$NETEXP+total$CL.F+total$GC.F+total$hits_scale
          +total$Mkt.RF+total$SMB+total$HML+total$RMW+total$CMA+total$reddit_sentiment)
summary(lm1)


# Make predictions
head(total[,c(5:27,31)])
predictions <- lm1 %>% predict(total[,c(5:27,31)])
# Model performance
# (a) Compute the prediction error, RMSE
RMSE(predictions, total$ExcRet_t1)
# (b) Compute R-square

R2(predictions, total$ExcRet_t1)

lm2 <- lm(total$ExcRet_t1 ~ total$sma20+total$ema14+total$bb20_dn+total$rsi14
          +total$macd+total$DGS10+total$CPIAUCSL+total$GDP
          +total$NETEXP+total$CL.F+total$GC.F+total$hits_scale
          +total$Mkt.RF+total$SMB+total$HML+total$RMW+total$CMA+total$reddit_sentiment)#+total$UNRATE
summary(lm2)

#Plot correlation matrix
x <- total[5:29]
y <- total[5:29]
correlation <- cor(x, y)
corrplot(correlation, method="circle")

install.packages("olsrr")
library(olsrr)
model <- lm(ExcRet_t1 ~ sma20+ema14+bb20_dn+rsi14
            +macd+UNRATE+DGS10+CPIAUCSL+GDP
            +NETEXP+CL.F+GC.F+hits_scale
            +Mkt.RF+SMB+HML+RMW+CMA+reddit_sentiment, data = total)
summary(model)

model$
  #Variable Selection
  #We tried three method of stepwise regression to come up with the best factors(i.e., variables)
  #Build regression model from a set of candidate predictor variables by entering predictors 
  #based on p values, in a stepwise manner until there is no variable left to enter any more. 
  #The model should include all the candidate predictor variables. 
  forward_selection <- ols_step_forward_p(lm1, details = TRUE)#final R-Squared=0.006,
plot(forward_selection)
forward_selection
factor <- c(forward_selection$predictors[1:4])
forward_selection$model#final model generated from stepwise regression
forward_selection$model$coefficients["macd"]
#backward_selection <- ols_step_backward_p(model, details = TRUE)#final R-Squared=0.006
#plot(backward_selection)
#both <- ols_step_both_p(model, details = TRUE)#final R-Squared=0.005
#plot(both)

# We also calculate Relative Importance for Each Predictor
install.packages("relaimpo")
library(relaimpo)
Relative_Importance <- calc.relimp(model,type=c("lmg"),rela=TRUE)
#The top three important factor is:
#Relative importance metrics(lmg): hits_scale:0.3705,macd:0.1768,reddit_sentiment:0.1180




#Choose the factor for security-selection every month based on latest database (from 2016-1-1 to the executing day)
typeof(total$date)
total$date <- as.Date(total$date)
#tail(total[total$date < date_list[3], ])

#retrieve all the factors according to the executing day

factor_selection = data.frame()
for(date_execute in date_list){
  total_execute <- total[total$date <= date_execute, ]
  model <- lm(ExcRet_t1 ~ sma20+ema14+bb20_dn+rsi14
              +macd+UNRATE+DGS10+CPIAUCSL+GDP
              +NETEXP+CL.F+GC.F+hits_scale
              +Mkt.RF+SMB+HML+RMW+CMA+reddit_sentiment, data = total_execute)
  forward_selection <- ols_step_forward_p(model, details = FALSE)
  factor <- c(forward_selection$predictors[1]
              ,forward_selection$model$coefficients[forward_selection$predictors[1]]
              ,forward_selection$predictors[2]
              ,forward_selection$model$coefficients[forward_selection$predictors[2]]
              ,forward_selection$predictors[3]
              ,forward_selection$model$coefficients[forward_selection$predictors[3]])
  factor <- as.data.frame(t(factor))
  rownames(factor) <- date_execute
  factor <- add_rownames(factor, var = "date_execute")
  colnames(factor) <- c("date_execute","factor1","factor1_value","factor2","factor2_value","factor3","factor3_value")
  factor$date_execute <- as.Date(factor$date_execute)
  factor$factor1_value <- as.numeric(factor$factor1_value)
  factor$factor2_value <- as.numeric(factor$factor2_value)
  factor$factor3_value <- as.numeric(factor$factor3_value)
  factor_selection <- rbind(factor_selection,factor)
}
head(factor_selection)
##write to csv
write.csv(factor_selection,"C:/FIN 6392.001 - Financial Technology and Blockchain/pj1\\factor_selection_2019.csv", row.names = FALSE)

#filter the securities and decide what to long/short (every month in the testing year)
#retrieve all the trading data according to the strategy
security_selection <- data.frame()
for(date_slice in date_list){
  factor1 <- factor_selection[factor_selection$date_execute == date_slice,][2]
  factor2 <- factor_selection[factor_selection$date_execute == date_slice,][4]
  factor3 <- factor_selection[factor_selection$date_execute == date_slice,][6]
  #select top 1 stock
  top_slice <- total[total$date == date_slice, ]
  if (factor_selection[factor_selection$date_execute == date_slice,][3]<0) {
    top_slice <- arrange(top_slice, top_slice[,factor1$factor1])#ascend
    top_slice <- top_slice [1:3,]
  } else {
    top_slice <- arrange(top_slice, desc(top_slice[,factor1$factor1]))#descend
    top_slice <- top_slice [1:3,]    
  }
  if (factor_selection[factor_selection$date_execute==date_slice,][5]<0) {
    top_slice <- arrange(top_slice, top_slice[,factor2$factor2])
    top_slice <- top_slice [1:2,]
  } else {
    top_slice <- arrange(top_slice, desc(top_slice[,factor2$factor2]))
    top_slice <- top_slice [1:2,]
  }
  if (factor_selection[factor_selection$date_execute==date_slice,][7]<0) {
    top_slice <- arrange(top_slice, top_slice[,factor3$factor3])
    top_slice <- top_slice [1,]
  } else {
    top_slice <- arrange(top_slice, desc(top_slice[,factor3$factor3]))
    top_slice <- top_slice [1,]
  }
  ##last bottom 1 stock
  bottom_slice <- total[total$date == date_slice, ]
  if (factor_selection[factor_selection$date_execute == date_slice,][3]<0) {
    bottom_slice <- arrange(bottom_slice, desc(bottom_slice[,factor1$factor1]))#descend
    bottom_slice <- bottom_slice [1:3,]
  } else {
    bottom_slice <- arrange(bottom_slice, bottom_slice[,factor1$factor1])#ascend
    bottom_slice <- bottom_slice [1:3,]    
  }
  if (factor_selection[factor_selection$date_execute==date_slice,][5]<0) {
    bottom_slice <- arrange(bottom_slice, desc(bottom_slice[,factor2$factor2]))
    bottom_slice <- bottom_slice [1:2,]
  } else {
    bottom_slice <- arrange(bottom_slice, bottom_slice[,factor2$factor2])
    bottom_slice <- bottom_slice [1:2,]
  }
  if (factor_selection[factor_selection$date_execute==date_slice,][7]<0) {
    bottom_slice <- arrange(bottom_slice, desc(bottom_slice[,factor3$factor3]))
    bottom_slice <- bottom_slice [1,]
  } else {
    bottom_slice <- arrange(bottom_slice, bottom_slice[,factor3$factor3])
    bottom_slice <- bottom_slice [1,]
  }
  total_slice <- rbind(top_slice,bottom_slice)
  Side <- data.frame(c("buy","sell"))
  colnames(Side) <- c("side")
  total_slice <- cbind(Side, total_slice)
  security_selection <- rbind(security_selection,total_slice)
}
head(security_selection)
##write to csv
write.csv(security_selection,"C:/FIN 6392.001 - Financial Technology and Blockchain/pj1\\security_selection_2019.csv", row.names = FALSE)


## Portfolio Performance Exhibition 

# Load manually modification data
StockPerformance <- read.csv("Performance_Exhibition.csv")

# Dollar amount profit/loss and percentage return at the end of each month of year 2019
StockPerformance <- StockPerformance[2:14,c('Date','Profit.Loss','Portf.Ret','Market.Ret')]
StockPerformance
# Incredible Totally made $9122.9046 at the end of year 2019

# Regression result compare with market(S&P500)
lm2 <- lm(StockPerformance$Portf.Ret~StockPerformance$Market.Ret)
summary(lm2)
# With a positive alpha 0.05958