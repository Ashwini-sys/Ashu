##Top 5 source of protein

cd <- read.csv("D:/data science/Project1stOnCereals/cereals_data (1).csv", stringsAsFactors=TRUE)
wt_pro<-cd[,c(1,5,14)]
wt_pro$protein<-wt_pro$protein/wt_pro$weight
View(wt_pro$protein)
wt_proTop<-head(wt_pro[order(wt_pro$protein, decreasing=TRUE), ], 10)
wt_proTop1<-wt_pro[,c(1,2)]
wt_pro_matrix<-as.matrix(wt_proTop1)
View(wt_proTop1)
barplot(wt_pro_matrix)
str(wt_pro_matrix)
hist(wt_pro_matrix$protein)
#barplot(wt_pro_matrix$names,las=2,col=rainbow(50))

