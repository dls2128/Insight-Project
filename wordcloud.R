## wordcloud

## count <- textcnt(string,n=1L,method="string") # Count number of instances of all words in text

library(tm)
library(SnowballC)
library(wordcloud)

txt.corpus <- Corpus(DirSource("/Users/darasosulski/Desktop/Insight Project/corpus/corpustwo"))
txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
txt.corpus <- tm_map(txt.corpus, stripWhitespace); #inspect(docs[1])
txt.corpus <- tm_map(txt.corpus, stemDocument)

par(mar = rep(2, 4))

wordcloud(txt.corpus, scale=c(4,0.5), max.words=200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE)

