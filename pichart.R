
data1<- read.csv("D:/data science/Project1stOnCereals/cereals_data (1).csv", stringsAsFactors=TRUE)
view(data1)
install.packages("RColorBrewer")
library(RColorBrewer)




data1$mfrFlag <- as.numeric(!is.na(data1$calories))
pichartdata<-tapply(data1$mfrFlag , data1$mfr,sum)
pichartdata
pichartdata<-as.matrix(pichartdata)
pie(pichartdata[,1],main = " Manufacturer of cereal",label=pichartdata[,1],col = rainbow(length(pichartdata)))

