#Libraries
library(zoo)
library(xts)
library(TTR)
library(quantmod)

# Load list of symbols (Updated May 2017)
SYM <- as.character( read.csv('http://trading.chrisconlan.com/SPstocks_current.csv', 
                              stringsAsFactors = FALSE, header = FALSE)[,1] )

DATA <- list()
n <- length(SYM)
l <- 1
for (i in l:n) {
  j <- SYM[i]
  DATA[[j]] <- getSymbols(SYM[i], auto.assign = FALSE)
}
l <- i + 1
#save(DATA, file = "data/symbols.rda")

# S&P 500 index
sp500_index <- read.table("data/data_csv.csv",sep = ",", header = T)
sp500_index_xts <- xts(sp500_index[,-1],order.by = as.Date(sp500_index[,1]))
#save(sp500_index_xts, file = "data/sp500_index_xts.rda")
