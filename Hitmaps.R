

cd_hitmap <- read.csv("G:/Suman/batch34/project____circulatepls/cereals_data.csv", stringsAsFactors=TRUE)
cd_hitmap$sodium<-cd_hitmap$sodium/100
cd_hitmap<-data.matrix(cd_hitmap)
View(cd_hitmap)


cd_hitmap<-cd_hitmap[,4:10]
#cd_hitmap<-as.numeric(cd_hitmap)
cd_hitmap
heatmap(cd_hitmap)




