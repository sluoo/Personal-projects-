library(pacman) 
pacman::p_load(data.table,fixest,BatchGetSymbols,finreportr,tidyverse,lubridate)

#BatchGetSymbols and finreportr (scrape data from yahoo finance / scrape data from 10K)
#finereportr error 

#Download stock price data 
first.date = Sys.Date() - 2500
last.date = Sys.Date()
freq.data = "monthly"
tickers <- c("TSLA","NIO","PRPL","AAPL","SNAP","MU",
             "AMD","NVDA","TWTR")

#Get stock prices 
stocks <- BatchGetSymbols (tickers=tickers,
                          first.date=first.date,
                          last.date = last.date,
                          freq.data=freq.data,
                          do.cache=FALSE,#do not save data to computer
                          thresh.bad.data=0)

stock_DT <- stocks$df.tickers %>% setDT()

#Graph Returns and Prices
return_plots_all <- print(ggplot(stock_DT, aes(x=ref.date, y=ret.adjusted.prices, color=ticker))
                     + geom_line()
                     + theme_bw()
                     + labs(title = "", x="Date", y="Monthly Returns", subtitle = ""))

close_price_all <- print(ggplot(stock_DT, aes(x=ref.date, y=price.close, color=ticker))
                                + geom_line() + theme_bw() 
                                + labs(title = "", x="Date", y="Closing Price"))

#Individual stock price analysis 
stock_ind <- (stock_DT %>% filter(ticker %in% c("AAPL","NIO","AMD","TWTR")))
item <- stock_ind$price.close 
return_indv <- print(ggplot(stock_ind, aes(x=ref.date, y=item))
                     + geom_line()
                     + facet_wrap(.~ticker, scale="free_y")
                     + theme_bw()) 

#ideas: analyze volitality of stocks (compare high vs low price), regressions, estimate cap M regression etc. 

#All stock tickers in SP500
df.500 <- GetSP500Stocks()





