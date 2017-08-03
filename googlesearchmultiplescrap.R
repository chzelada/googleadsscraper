library(rvest)
library(XML)
library(curl)
library(dplyr)
library(tm)
library(wordcloud)
library(regex)
library(readr)
ua <- read_csv("ua.csv",col_names = FALSE)
uule <- read_csv("uule.csv")
names(ua)<-c("id","string")
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'),xtrakw))
  return(corpus)
}

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

AdsSnippet<-c()
AdsUrlTot<-c()
AdsTitleTol<-c()
Stitles<-c()
Surl<-c()
Ssnippet<-c()
n=1
for(i in 1:5){
  print(i)
  Sys.sleep(sample(1, 1) * 0.3)
  index<-sample(1:11,1)
  uar<-ua$string[index]
  state<-'FL'
  StateName<-state.name[which(state.abb==state)]
  StateUule<-uule$uule[uule$Name==StateName]
  
  KeyWord<-c("medicare supplemental")
  KeyWord<-strsplit(KeyWord,split = " ",fixed = TRUE) %>% unlist() 
  xtrakw<-KeyWord
  KeyWord<-paste(KeyWord,collapse = "+")
  
  url<-paste("https://www.google.com/search?q=",trimws(KeyWord),"&ip=0.0.0.0","&uule=",(StateUule),sep="",collapse = "")
  
  html <- read_html(curl(url,handle = new_handle("useragent" = uar)))
  
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
searches = n
)

WordCloudData<-getWordCloud(unique(ads_data$asnippet))

wordcloud(words=WordCloudData$word,freq=WordCloudData$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


