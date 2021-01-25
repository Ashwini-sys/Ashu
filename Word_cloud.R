### Word cloud 
cd <-read.csv("D:/data science/Project1stOnCereals/cereals_data (1).csv", stringsAsFactors=TRUE)
wt_cal<-cd[,c(1,2,4,14)]
wt_cal
View(wt_cal)
#Build corpus
library(tm)
corpus<-iconv(wt_cal$name, to='UTF-8', sub = "byte")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
#clean tm_map(0)
corpus<-tm_map(corpus,tolower)
inspect(corpus)
corpus<-tm_map(corpus,removeNumbers)
#corpus<-tm_map(corpus,removePunctuation)
inspect(corpus)
cleanset<-tm_map(corpus,removeWords,stopwords("en"))
cleanset<-tm_map(corpus,removeWords,c("total","100%"))
inspect(cleanset[1:5])

#term document matrix
tdm<-TermDocumentMatrix(cleanset)
tdm

tdm<-as.matrix(tdm)
tdm[1:10,1:20]

##bar plot
w<-rowSums(tdm)
#w<-subset(w,w>=25)
barplot(w,las=2,col=rainbow(50))

##Word cloud
library(wordcloud)
w<-sort(rowSums(tdm), decreasing = T)
set.seed(222)
wordcloud(words = names(w),freq = w,max.words = 1500, random.order = FALSE,colors = brewer.pal(10,"Dark2"),scale =c(3,.3) )


