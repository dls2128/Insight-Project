## Insight data science fellowship project 

## Load libraries that will be used for analysis... 
library(stringr)
library(ggplot2)
library(reshape)
library(reshape2)
library(RCurl)
library(tm)
library(SnowballC)
library(wordcloud)


## Load all webscraped data into R...
## Requires the prior preparation of a .csv file containing the URLs of all websites to be scraped... 
addresses <- read.csv("/Users/darasosulski/Desktop/Insight Project/WSJlinks")

for (i in addresses){
        WSJ.text <- getURL(i)
}

WSJ.df <- as.data.frame(WSJ.text)

## Save raw HTML to individual .txt files...
outpathA <- "/Users/darasosulski/Desktop/Insight Project"

x <- 1:nrow(WSJ.df)

for(i in x) {
        write(as.character(WSJ.df[i,1]), file = paste(outpathA,"/",i,".txt",sep = ""))
}


## Load all .txt files containing webscraped data... 
setwd("/Users/darasosulski/Desktop/Insight Project")
temp = list.files(pattern="*.txt")
for (i in 1:length(temp)) assign(temp[i], readLines(temp[i]))


## Go through each .txt file to extract keywords of interest... 
textdata <- vector("list",length(temp))

for (i in 1:length(temp)) { 
text <- readLines((temp[i]))
string <- text
      
trader <- str_extract(text,"trader")
trader <- trader[!is.na(trader)]
tradernum <- length(trader)

china <- str_extract(text,"China")
china <- china[!is.na(china)]
chinanum <- length(china)

energy <- str_extract(text,"energy")
energy <- energy[!is.na(energy)]
energynum <- length(energy)

oil <- str_extract(text,"oil")
oil <- oil[!is.na(oil)]
oilnum <- length(oil)

migrant <- str_extract(text,"migrant")
migrant <- migrant[!is.na(migrant)]
migrantnum <- length(migrant)

interest <- str_extract(text,"interest")
interest <- interest[!is.na(interest)]
interestnum <- length(interest)

russia <- str_extract(text,"Russia")
russia <- russia[!is.na(russia)]
russianum <- length(russia)

rate <- str_extract(text,"rate")
rate <- rate[!is.na(rate)]
ratenum <- length(rate)

obama <- str_extract(text,"Obama")
obama <- obama[!is.na(obama)]
obamanum <- length(obama)

shooting <- str_extract(text,"shooting")
shooting <- shooting[!is.na(shooting)]
shootingnum <- length(shooting)

madmen <- str_extract(text,"Mad Men")
madmen <- madmen[!is.na(madmen)]
madmennum <- length(madmen)

weather <- str_extract(text,"weather")
weather <- weather[!is.na(weather)]
weathernum <- length(weather)

isis <- str_extract(text,"ISIS")
isis <- isis[!is.na(isis)]
isisnum <- length(isis)

deflategate <- str_extract(text,"deflategate")
deflategate <- deflategate[!is.na(deflategate)]
deflategatenum <- length(deflategate)

dollar <- str_extract(text,"dollar")
dollar <- dollar[!is.na(dollar)]
dollarnum <- length(dollar)

gold <- str_extract(text,"gold")
gold <- dollar[!is.na(gold)]
goldnum <- length(gold)

hillary <- str_extract(text,"Hillary")
hillary <- dollar[!is.na(hillary)]
hillarynum <- length(hillary)

bank <- str_extract(text,"bank")
bank <- dollar[!is.na(bank)]
banknum <- length(bank)

water <- str_extract(text,"water")
water <- dollar[!is.na(water)]
waternum <- length(water)

greece <- str_extract(text,"Greece")
greece <- dollar[!is.na(greece)]
greecenum <- length(greece)

goldman <- str_extract(text,"Goldman")
goldman <- dollar[!is.na(goldman)]
goldmannum <- length(goldman)

baltimore <- str_extract(text,"Baltimore")
baltimore <- dollar[!is.na(baltimore)]
baltimorenum <- length(baltimore)

gun <- str_extract(text,"gun")
gun <- dollar[!is.na(gun)]
gunnum <- length(gun)

wordcount <- c(tradernum, chinanum, energynum, oilnum, migrantnum, interestnum, russianum, ratenum, obamanum, madmennum, weathernum, isisnum, deflategatenum, dollarnum, goldnum, hillarynum, banknum, waternum, greecenum, goldmannum, baltimorenum, gunnum)
textdata[[i]] <- wordcount 
}


## Convert list of keyword frequency to data frame... 
textdataframe <- do.call(data.frame, lapply(textdata, unlist))
transdata <- t(textdataframe)
transdataframe <- as.data.frame(transdata)


## Plot wordcloud illustrating the frequency of keywords in webscraped data... 
txt.corpus <- Corpus(DirSource("/Users/darasosulski/Desktop/Insight Project/corpus/corpustwo"))
txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
txt.corpus <- tm_map(txt.corpus, stripWhitespace); #inspect(docs[1])
txt.corpus <- tm_map(txt.corpus, stemDocument)

par(mar = rep(2, 4))

wordcloud(txt.corpus, scale=c(4,0.5), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))


## Load historical stock price data from .csv files... 
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

files = list.files(pattern="*.csv")
myfiles = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myprices <- myfiles[,c("Adj.Close")]
sortprices <- split(myprices,rep(1:14,each=39))
prices <- as.data.frame(sortprices)


## Scale all price data so mean of values is 0 and SD is 1...
scaledprices <- scale(myprices)  
sortscaled <- split(scaledprices,rep(1:14,each=39))
scaled <- as.data.frame(sortscaled)


## Plot frequency of keywords by day...
x <- (1:39)
colnames(transdata) <- c("Trader","China","Energy","Oil","Migrant","Interest","Russia","Rate","Obama","Mad Men","Weather","ISIS","Deflategate","Dollar","Gold","Hillary","Bank","Water","Greece","Goldman","Baltimore","Gun")
transmelted <- melt(transdata, id.vars = 'row.names')

require(grid)
## ggplot(melted, aes(X2,value, colour = X2)) + geom_line() + xlab("Word") + ylab("Frequency")
m <- ggplot(transmelted, aes(value, X2, colour = X2)) + geom_line() + xlab("Frequency") + ylab("Word") + theme(legend.key.size=unit(0.3,"cm"))
print(m)

transmelted2 <- melt(transdata, id.vars = 'row.names')
b <- rep.int(x,22)
transmelted2$X1 <- b
n <- ggplot(transmelted2, aes(X1, value, colour = X2)) + geom_line() + xlab("Frequency") + ylab("Word") + theme(legend.key.size=unit(0.3,"cm"))
print(n)


## Plot all historical stock price data... 
x <- (1:39)
colnames(scaled) <- c("Apple","Bloomsbury","Capital One","Centrica","Chipotle","Facebook","Fever Tree","Goldman","HSBC","McDonald's","Microsoft","New York Times","Schroders","Twitter")
scaledframe <- cbind(x,scaled)
scaledmelted <- melt(scaledframe, id.vars = 'x')
o <- ggplot(scaledmelted, aes(x, value, colour = variable)) + geom_line() + xlab("Day") + ylab("Scaled Price") + theme(legend.key.size=unit(0.4,"cm"))
print(o)

colnames(prices) <- c("Apple","Bloomsbury","Capital One","Centrica","Chipotle","Facebook","Fever Tree","Goldman","HSBC","McDonald's","Microsoft","New York Times","Schroders","Twitter")
pricesframe <- cbind(x,prices)
pricesmelted <- melt(pricesframe, id.vars = 'x')
p <- ggplot(pricesmelted, aes(x, value, colour = variable)) + geom_line() + xlab("Day") + ylab("Price") + theme(legend.key.size=unit(0.4,"cm"))
print(p)


## Calculate and plot PCA for scaled historical price data...
pca <- princomp(scaled,cor=TRUE)
summary(pca)
loadings(pca)
plot(pca,type="lines")
pca$scores
biplot(pca)


## Out of curiosity, look at cross-correlograms between each keyword/price combination... 
#wordnames <- colnames(transdataframe)
#pricenames <- colnames(prices)
 
#y <- 1:length(wordnames)

## Create nested loops to calculate cross-correlograms for all keyword/price combinations... 
# for (i in y){
# xcorr <- ccf(transdataframe$wordnames[i], prices$pricenames[i], lag.max=NULL, type=c("correlation"), plot=TRUE)
# }

colnames(transdataframe) <- c("Trader","China","Energy","Oil","Migrant","Interest","Russia","Rate","Obama","Mad Men","Weather","ISIS","Deflategate","Dollar","Gold","Hillary","Bank","Water","Greece","Goldman","Baltimore","Gun")

par(mar = rep(2, 4))
xcorr <- ccf(transdataframe$Trader, prices$Apple, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Apple")
xcorr <- ccf(transdataframe$Trader, prices$Twitter, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Twitter")
xcorr <- ccf(transdataframe$Trader, prices$Facebook, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Facebook")
xcorr <- ccf(transdataframe$Trader, prices$Chipotle, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Chipotle")
xcorr <- ccf(transdataframe$Trader, prices$Schroders, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Schroders")
xcorr <- ccf(transdataframe$Trader, prices$HSBC, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. HSBC")
xcorr <- ccf(transdataframe$Trader, prices$Goldman, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Trader vs. Goldman")

xcorr <- ccf(transdataframe$Obama, prices$Apple, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Apple")
xcorr <- ccf(transdataframe$Obama, prices$Twitter, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Twitter")
xcorr <- ccf(transdataframe$Obama, prices$Facebook, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Facebook")
xcorr <- ccf(transdataframe$Obama, prices$Chipotle, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Chipotle")
xcorr <- ccf(transdataframe$Obama, prices$Schroders, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Schroders")
xcorr <- ccf(transdataframe$Obama, prices$HSBC, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. HSBC")
xcorr <- ccf(transdataframe$Obama, prices$Goldman, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Obama vs. Goldman")

xcorr <- ccf(transdataframe$Oil, prices$Apple, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Apple")
xcorr <- ccf(transdataframe$Oil, prices$Twitter, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Twitter")
xcorr <- ccf(transdataframe$Oil, prices$Facebook, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Facebook")
xcorr <- ccf(transdataframe$Oil, prices$Chipotle, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Chipotle")
xcorr <- ccf(transdataframe$Oil, prices$Schroders, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Schroders")
xcorr <- ccf(transdataframe$Oil, prices$HSBC, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. HSBC")
xcorr <- ccf(transdataframe$Oil, prices$Goldman, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Oil vs. Goldman")

xcorr <- ccf(transdataframe$Interest, prices$Apple, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Apple")
xcorr <- ccf(transdataframe$Interest, prices$Twitter, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Twitter")
xcorr <- ccf(transdataframe$Interest, prices$Facebook, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Facebook")
xcorr <- ccf(transdataframe$Interest, prices$Chipotle, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Chipotle")
xcorr <- ccf(transdataframe$Interest, prices$Schroders, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Schroders")
xcorr <- ccf(transdataframe$Interest, prices$HSBC, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. HSBC")
xcorr <- ccf(transdataframe$Interest, prices$Goldman, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Interest vs. Goldman")

xcorr <- ccf(transdataframe$Balitmore, prices$Apple, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Apple")
xcorr <- ccf(transdataframe$Balitmore, prices$Twitter, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Twitter")
xcorr <- ccf(transdataframe$Balitmore, prices$Facebook, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Facebook")
xcorr <- ccf(transdataframe$Balitmore, prices$Chipotle, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Chipotle")
xcorr <- ccf(transdataframe$Balitmore, prices$Schroders, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Schroders")
xcorr <- ccf(transdataframe$Balitmore, prices$HSBC, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. HSBC")
xcorr <- ccf(transdataframe$Balitmore, prices$Goldman, lag.max=NULL, type=c("correlation"), plot=TRUE)
title("Balitmore vs. Goldman")


