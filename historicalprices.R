## Load historical price data from .csv files 

setwd("/Users/darasosulski/Desktop/Insight Project")

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

## Normalize all price data... 
files = list.files(pattern="*.csv")
myfiles = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

myprices <- myfiles[,c("Adj.Close")]


## Scale = scales data so mean is 0 and SD is 1
scaledprices <- scale(myprices)  

sortscaled <- split(scaledprices,rep(1:14,each=39))
scaled <- as.data.frame(sortscaled)

sortprices <- split(myprices,rep(1:14,each=39))
prices <- as.data.frame(sortprices)


## Plot all historical time series on the same axes... 

x <- (1:39)
colnames(scaled) <- c("Apple","Bloomsbury","Capital One","Centrica","Chipotle","Facebook","Fever Tree","Goldman","HSBC","McDonald's","Microsoft","New York Times","Schroders","Twitter")
scaledframe <- cbind(x,scaled)
scaledmelted <- melt(scaledframe, id.vars = 'x')
ggplot(scaledmelted, aes(x, value, colour = variable)) + geom_line() + xlab("Day") + ylab("Scaled Price")

colnames(prices) <- c("Apple","Bloomsbury","Capital One","Centrica","Chipotle","Facebook","Fever Tree","Goldman","HSBC","McDonald's","Microsoft","New York Times","Schroders","Twitter")
pricesframe <- cbind(x,prices)
pricesmelted <- melt(pricesframe, id.vars = 'x')
ggplot(pricesmelted, aes(x, value, colour = variable)) + geom_line() + xlab("Day") + ylab("Scaled Price")



## Plot PCA for normalized historical price data

fit <- princomp(scaled,cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
fit$scores
biplot(fit)




