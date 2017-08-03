library(rvest)
library(XML)
library(curl)
library(dplyr)
library(tm)
library(wordcloud)
library(readr)

## Catalog of UA and UULE
#UA is used to change User Interface this will help to not get the ip block
#from google
ua <- read_csv("ua.csv",col_names = FALSE) 
names(ua)<-c("id","string")

# UULE is used to change the region of the search, For example,
# if you want to search ads as someone from florida you need
# the UULE code for florida. The file uule.csv has all the uule
# this was scrapped from google 
uule <- read_csv("uule.csv")

# this function will help up clean the corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'),xtrakw))
  return(corpus)
}

# with this function we generate the word cloud
getWordCloud <- function(x){
  AdsTitleSrc<-VectorSource(x)
  AdsTitleCorpus <- VCorpus(AdsTitleSrc)
  AdsTitleCorpus<-clean_corpus(AdsTitleCorpus)
  AdsTitleTdm<-TermDocumentMatrix(AdsTitleCorpus)
  AdsTitleM<- as.matrix(AdsTitleTdm)
  AdsTitlesCnt<-AdsTitleM %>% rowSums()
  AdsTitleDf<-data.frame(word=names(AdsTitlesCnt),freq=AdsTitlesCnt)
  rownames(AdsTitleDf)<-NULL
  return(AdsTitleDf)
}

# Initializing vectors
AdsSnippet<-c()
AdsUrlTot<-c()
AdsTitleTol<-c()
Stitles<-c()
Surl<-c()
Ssnippet<-c()

# State from where you want to make the search
# KeyWord you want to scrap
state<-'FL'
KeyWord<-c("medicare supplemental")

# Start Scrapping
n=1
for(i in 1:5){
  print(i)
  Sys.sleep(sample(1, 1) * 0.3) ## Random wait to avoid been detected as machine by google.
  index<-sample(1:11,1)
  uar<-ua$string[index]
  StateName<-state.name[which(state.abb==state)]
  StateUule<-uule$uule[uule$Name==StateName]
  KeyWord<-strsplit(KeyWord,split = " ",fixed = TRUE) %>% unlist() 
  xtrakw<-KeyWord
  KeyWord<-paste(KeyWord,collapse = "+")
  
  url<-paste("https://www.google.com/search?q=",trimws(KeyWord),"&ip=0.0.0.0","&uule=",(StateUule),sep="",collapse = "")
  
  html <- read_html(curl(url,handle = new_handle("useragent" = uar)))
  ### Here we star to scrap by css components
  
  ###Organic
  titles <- html_nodes(html, "#search h3.r")
  SearchTitles<-html_text(titles)
  snippet <- html_nodes(html, "#search span.st")
  SearchSnippet<-trimws(html_text(snippet),which = "both")
  urls <- html_nodes(html, "#search cite")
  SearchUrl<-html_text(urls)
  
  ### ADS
  ads_titles<-html_nodes(html, "li.ads-ad h3")
  AdsTitles<-html_text(ads_titles)
  for(i in seq_along(AdsTitles)){
    AdsTitles[i]<-paste("[",i,"],",AdsTitles[i],collapse = "",sep="") 
  }
  
  ads_url<-html_nodes(html, "cite._WGk")
  AdsUrl<-html_text(ads_url)
  for(i in seq_along(AdsUrl)){
    AdsUrl[i]<-paste("[",i,"],",AdsUrl[i],collapse = "",sep="")
  }
  
  ellip<-html_nodes(html, "div.ellip.ads-creative")
  AdsEllip<-html_text(ellip)
  for(i in seq_along(AdsEllip)){
    AdsEllip[i]<-paste("[",i,"],",AdsEllip[i],collapse = "",sep="")
  }
  
  AdsSnippet<-c(AdsSnippet,AdsEllip)
  AdsTitleTol<-c(AdsTitleTol,AdsTitles)
  AdsUrlTot<-c(AdsUrlTot,AdsUrl)
  Stitles<-c(Stitles,SearchTitles)
  Ssnippet<-c(Ssnippet,SearchSnippet)
  Surl<-c(Surl,SearchUrl)
  n=n+1
}

ads_data <- list(url=Surl,
                 ssnippet=Ssnippet,
                 stitles=Stitles,
                 aurl=AdsUrlTot,
                 atitle=AdsTitleTol,
                 asnippet=AdsSnippet,
                 state = state,
                 keyword=KeyWord,
                 date = Sys.time(),
                 searches = n)

WordCloudData<-getWordCloud(unique(ads_data$asnippet))

wordcloud(words=WordCloudData$word,freq=WordCloudData$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


