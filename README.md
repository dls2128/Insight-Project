# Insight-Project

# Code and related files created for Insight Data Science Fellowship Interview project

# Requires the following files, located in the same folder, to run:

# 1. A .csv file where each row represents the URL of the text you would like to scrape from the web 
# 2. Date-matched .csv files containing historical price data for each stock you would like to analyze 
# (downloaded in this example from Yahoo!: https://uk.finance.yahoo.com/q/hp?s=YHOO)
# 3. A folder called "corpus" containing the .txt files you would like to use to generate your wordcloud
# 4. The R script called "insightdatascienceproject.R"

# OTHER NOTES: 
# You will have to change the working directories (setwd is used in "insightdatascienceproject.R") 
# to suit the folders you will be using on your computer

# You will also have to have the following packages installed for code to run: stringr, ggplot2, reshape,
# reshape2, RCurl, tm, SnowballC, wordcloud (can install by typing install.packages("packagename") in command line)

# It is also a good idea to change the format of the webscraped text that results from the 
# execution of this code to plain text encoding, or you will get a lot of words in your wordcloud
# related to html formatting and other assorted nonsense 

# All text scraped from the Wall Street Journal's headline archive
# (e.g. http://www.wsj.com/public/page/archive-2015-5-1.html)






