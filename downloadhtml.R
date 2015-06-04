## Before running code, gather the URLs for all websites we're interested in 
## into one .csv file

## Downloads content from all HTML sources listed in .csv file... 

library(RCurl)

addresses <- read.csv("/Users/darasosulski/Desktop/Insight Project/WSJlinks")

for (i in addresses){
        WSJ.text <- getURL(i)
}

WSJ.df <- as.data.frame(WSJ.text)

## Save raw HTML to individual text files
outpathA <- "/Users/darasosulski/Desktop/Insight Project"

x <- 1:nrow(WSJ.df)

for(i in x) {
        write(as.character(WSJ.df[i,1]), file = paste(outpathA,"/",i,".txt",sep = ""))
}


