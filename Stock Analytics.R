##read the data
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")
##convert the date format
IBM$Date <- as.Date(IBM$Date,"%m/%d/%y")
GE$Date <- as.Date(GE$Date,"%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date,"%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date,"%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date,"%m/%d/%y")
##What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)
##What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)
##What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)
##plot the stock prices to see if we can visualize trends in stock prices during this time period
plot(CocaCola$Date,CocaCola$StockPrice,type = "l" ,col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
##In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
abline(v = as.Date(c("2000-03-01")),lwd = 2 )
##In the time period shown in the plot, which stock generally has lower values?
mean(CocaCola$StockPrice) < mean(ProcterGamble$StockPrice)
##look at how the stock prices changed from 1995-2005 for all five companies
plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type = "l", col = "red", ylim = c(0,201))
lines(IBM$Date[301:432],IBM$StockPrice[301:432], col = "yellow")
lines(GE$Date[301:432],GE$StockPrice[301:432], col = "blue")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col = "green")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "black")

##Which stock fell the most right after the technology bubble burst in March 2000?
abline(v = as.Date("2000-03-01"), lwd = 2)

##calculate the mean stock price of IBM, sorted by months
tapply(IBM$StockPrice, months(IBM$Date), mean)
