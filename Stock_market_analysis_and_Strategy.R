# Workspace cleanup
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

# Check and install necessary packages
if(!require("quantmod")) {install.packages("quantmod"); library(quantmod)}
if(!require("xts")) {install.packages("xts"); library(xts)}
if(!require("fTrading")) {install.packages("fTrading"); library(fTrading)}

#1. quantmod Package (Technical Analysis)

# Load data for WIG20 index
WIG20_data <- read.csv("https://stooq.pl/q/d/l/?s=wig20&i=d")
WIG20_data <- WIG20_data[,c(1,2,3,4,5)]
colnames(WIG20_data) <- c("Date","Open", "High", "Low", "Close")
WIG20_data$Date <- as.Date(WIG20_data$Date)
WIG20_data <- WIG20_data[order(WIG20_data$Date),]

plot(WIG20_data[,1:2])

# Load data for DAX index
DAX_data <- read.csv("https://stooq.pl/q/d/l/?s=^dax&i=d", stringsAsFactors = TRUE) 
DAX_data <- DAX_data[,c(1,2,3,4,5)]
colnames(DAX_data) <- c("Date","Open", "High", "Low", "Close")
DAX_data$Date <- as.Date(DAX_data$Date)
DAX_data <- DAX_data[order(DAX_data$Date),]

plot(DAX_data[,1:2])

# Load data for S&P 500 index
SPX_data <- read.csv("https://stooq.pl/q/d/l/?s=^spx&i=d")
SPX_data <- SPX_data[,c(1,2,3,4,5)]
colnames(SPX_data) <- c("Date","Open", "High", "Low", "Close")
SPX_data$Date <- as.Date(SPX_data$Date)
SPX_data <- SPX_data[order(SPX_data$Date),]

plot(SPX_data[,1:2])

# OHLC (Open, High, Low, Close) format
Op(WIG20_data)
Hi(WIG20_data)
Lo(WIG20_data)
Cl(WIG20_data)

Op(DAX_data)
Hi(DAX_data)
Lo(DAX_data)
Cl(DAX_data)

Op(SPX_data)
Hi(SPX_data)
Lo(SPX_data)
Cl(SPX_data)

# Plot closing prices
plot(Cl(WIG20_data), col = 'black')
plot(Cl(DAX_data), col = 'black')
plot(Cl(SPX_data), col = 'black')

# Convert to xts format
WIG20_data_xts <- xts(WIG20_data[,-1], order.by = WIG20_data$Date)
DAX_data_xts <- xts(DAX_data[,-1], order.by = DAX_data$Date)
SPX_data_xts <- xts(SPX_data[,-1], order.by = SPX_data$Date)

# Stock charts
barChart(WIG20_data_xts, subset = 'last 3 months')
candleChart(WIG20_data_xts, multi.col=TRUE, theme='white')
candleChart(WIG20_data_xts, multi.col=FALSE, theme='white', subset = 'last 12 months')
candleChart(WIG20_data_xts, multi.col=TRUE, theme='black', subset = 'last 4 months')

barChart(DAX_data_xts)
candleChart(DAX_data_xts, multi.col=TRUE, theme='white')
candleChart(DAX_data_xts, multi.col=FALSE, theme='white', subset = 'last 4 months')
candleChart(DAX_data_xts, multi.col=TRUE, theme='black', subset = 'last 4 months')

barChart(SPX_data_xts)
candleChart(SPX_data_xts, multi.col=TRUE, theme='white')
candleChart(SPX_data_xts, multi.col=FALSE, theme='white', subset = 'last 4 months')
candleChart(SPX_data_xts, multi.col=TRUE, theme='black', subset = 'last 4 months')

# The multi.col=TRUE option applies four candle colors:
#   grey => Op[t] < Cl[t] and Op[t] < Cl[t-1]
#   white => Op[t] < Cl[t] and Op[t] > Cl[t-1]
#   red => Op[t] > Cl[t] and Op[t] < Cl[t-1]
#   black => Op[t] > Cl[t] and Op[t] > Cl[t-1]

# Add Simple Moving Average indicator
chartSeries(WIG20_data_xts, theme = "white", TA = "addSMA()", subset = 'last 12 months')
chartSeries(DAX_data_xts, theme = "white", TA = "addSMA()", subset = 'last 2 months')
chartSeries(SPX_data_xts, theme = "white", TA = "addSMA()", subset = 'last 2 months')

# Add Exponential Moving Average indicator
chartSeries(WIG20_data_xts, theme = "white", TA = "addEMA()", subset = 'last 2 months')
chartSeries(DAX_data_xts, theme = "white", TA = "addEMA()", subset = 'last 2 months')
chartSeries(SPX_data_xts, theme = "white", TA = "addEMA()", subset = 'last 2 months')

# 5. Momentum Indicator

# Momentum is an indicator that measures the rate of price change. A lag of 25 periods means that for each point on the momentum chart, it will be calculated as the difference between the current price and the price 25 periods ago.

# Calculate momentum for WIG20 index
WIG20_data$mom_lag1 <- momTA(x = WIG20_data$Open, lag = 1)
WIG20_data$mom_lag5 <- momTA(x = WIG20_data$Open, lag = 5)
WIG20_data$mom_lag25 <- momTA(x = WIG20_data$Open, lag = 25)

# Calculate momentum for DAX index
DAX_data$mom_lag1 <- momTA(x = DAX_data$Open, lag = 1)
DAX_data$mom_lag5 <- momTA(x = DAX_data$Open, lag = 5)
DAX_data$mom_lag25 <- momTA(x = DAX_data$Open, lag = 25)

# Calculate momentum for S&P 500 index
SPX_data$mom_lag1 <- momTA(x = SPX_data$Open, lag = 1)
SPX_data$mom_lag5 <- momTA(x = SPX_data$Open, lag = 5)
SPX_data$mom_lag25 <- momTA(x = SPX_data$Open, lag = 25)

# Chart with momentum indicator (lag 1)
chartSeries(WIG20_data_xts, theme = "white", TA = "addMomentum()", subset = 'last 6 months')
chartSeries(DAX_data_xts, theme = "white", TA = "addMomentum()", subset = 'last 2 months')
chartSeries(SPX_data_xts, theme = "white", TA = "addMomentum()", subset = 'last 2 months')

# Chart with momentum indicator (lag 5)
chartSeries(WIG20_data_xts, theme = "white", TA = "addMomentum(5)", subset = 'last 2 months')
chartSeries(DAX_data_xts, theme = "white", TA = "addMomentum(5)", subset = 'last 2 months')
chartSeries(SPX_data_xts, theme = "white", TA = "addMomentum(5)", subset = 'last 2 months')

# Chart with momentum indicator (lag 25)
chartSeries(WIG20_data_xts, theme = "white", TA = "addMomentum(25)", subset = 'last 12 months')
chartSeries(DAX_data_xts, theme = "white", TA = "addMomentum(25)", subset = 'last 2 months')
chartSeries(SPX_data_xts, theme = "white", TA = "addMomentum(25)", subset = 'last 2 months')

# Rate of Return (RoR) calculation
WIG20_data$ror <- c(5233, diff(WIG20_data$Close)/head(WIG20_data$Close, -1))
WIG20_data$increase <- 0

DAX_data$ror <- c(5233, diff(DAX_data$Close)/head(DAX_data$Close, -1))
DAX_data$increase <- 0

SPX_data$ror <- c(5233, diff(SPX_data$Close)/head(SPX_data$Close, -1))
SPX_data$increase <- 0

# Sample investment strategy for WIG20 index
#################################################
library(ggplot2)

startIdx <- 5233
cash <- 25000
position <- 0
capital_history_WIG20 <- data.frame(Time = numeric(), Capital = numeric())

for (i in startIdx:nrow(WIG20_data)) {
  if (position == 1) {
    cash <- cash * (1 + WIG20_data$ror[i])
  }
  if (WIG20_data$mom_lag5[i-1] <= 0 & WIG20_data$mom_lag5[i] > 0) {
    position <- 1
    cash <- cash - 5
  }
  if (WIG20_data$mom_lag5[i-1] >= 0 & WIG20_data$mom_lag5[i] < 0) {
    position <- 0
    cash <- cash - 5
  }
  
  capital_history_WIG20[i - startIdx + 1, ] <- c(cash)
}

capital_history_WIG20$Data <- WIG20_data$Date[startIdx:nrow(WIG20_data)]

ggplot(capital_history_WIG20, aes(x = Data, y = Capital)) +
  geom_line() +
  labs(title = "Dynamic", x = "Date", y = "Capital")

# Sample investment strategy for DAX index
#################################################
startIdx <- 13631
cash <- 25000
position <- 0
capital_history_DAX <- data.frame(Time = numeric(), Capital = numeric())

for (i in startIdx:nrow(DAX_data)) {
  if (position == 1) {
    cash <- cash * (1 + DAX_data$ror[i])
  }
  if (DAX_data$mom_lag5[i-1] <= 0 & DAX_data$mom_lag5[i] > 0) {
    position <- 1
    cash <- cash - 5
  }
  if (DAX_data$mom_lag5[i-1] >= 0 & DAX_data$mom_lag5[i] < 0) {
    position <- 0
    cash <- cash - 5
  }
  
  capital_history_DAX[i - startIdx + 1, ] <- c(cash)
}

capital_history_DAX$Data <- DAX_data$Date[startIdx:nrow(DAX_data)]

ggplot(capital_history_DAX, aes(x = Data, y = Capital)) +
  geom_line() +
  labs(title = "Dynamic", x = "Date", y = "Capital")

# Sample investment strategy for SPX index
#################################################
startIdx <- 36588
cash <- 50000
position <- 0
capital_history_SPX <- data.frame(Time = numeric(), Capital = numeric())

for (i in startIdx:nrow(SPX_data)) {
  if (position == 1) {
    cash <- cash * (1 + SPX_data$ror[i])
  }
  if (SPX_data$mom_lag5[i-1] <= 0 & SPX_data$mom_lag5[i] > 0) {
    position <- 1
    cash <- cash - 5
  }
  if (SPX_data$mom_lag5[i-1] >= 0 & SPX_data$mom_lag5[i] < 0) {
    position <- 0
    cash <- cash - 5
  }
  
  capital_history_SPX[i - startIdx + 1, ] <- c(cash)
}

capital_history_SPX$Data <- SPX_data$Date[startIdx:nrow(SPX_data)]

ggplot(capital_history_SPX, aes(x = Data, y = Capital)) +
  geom_line() +
  labs(title = "Dynamic", x = "Date", y = "Capital")

# Combine capital histories
combined_capital <- merge(merge(capital_history_WIG20, capital_history_DAX, by = "Data", all = TRUE), capital_history_SPX, by = "Data", all = TRUE)

# Merge data
merged_data <- merge(capital_history_WIG20, capital_history_DAX, by = "Data", all = TRUE)

combined_capital[is.na(combined_capital)] <- 0

# Fill NA values
combined_capital_filled <- na.locf(combined_capital, fromLast = FALSE)

combined_capital_filled[combined_capital_filled == 0] <- NA

combined_capital_filled$Total_Capital <- rowSums(combined_capital_filled[, c("Capital.x", "Capital.y", "Capital")], na.rm = TRUE)

tail(combined_capital_filled$Total_Capital, 1)

# Final plot of total capital
ggplot(combined_capital_filled, aes(x = Data, y = Total_Capital)) +
  geom_line() +
  labs(title = "Dynamic", x = "Date", y = "Capital")





combined_capital <- merge(merge(capital_history_WIG20, capital_history_DAX, by = "Data", all = TRUE), capital_history_SPX, by = "Data", all = TRUE)
merged_data <- merge(capital_history_WIG20, capital_history_DAX, by = "Data", all = TRUE)

combined_capital[is.na(combined_capital)] <- 0

combined_capital_filled[combined_capital_filled == 0] <- NA


combined_capital_filled <- na.locf(combined_capital_filled, fromLast = FALSE)

tail(combined_capital_filled$Total_Capital, 1)


combined_capital_filled$Total_Capital <- rowSums(combined_capital_filled[, c("Capital.x", "Capital.y", "Capital")])


combined_capital_filled$Total_Capital <- rowSums(combined_capital_filled[, c("Capital.x", "Capital.y", "Capital")], na.rm = TRUE)


# Final plot of total capital
tail(combined_capital_filled$Total_Capital, 1)
ggplot(combined_capital_filled, aes(x = Data, y = Total_Capital)) +
  geom_line() +
  labs(title = "Dinamic", x = "DATA", y = "CAPITAL")

