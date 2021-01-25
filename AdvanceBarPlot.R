install.packages("tidyverse")
library(tidyverse)
cd <- read.csv("G:/Suman/batch34/project____circulatepls/cereals_data1.csv", stringsAsFactors=TRUE)
view(cd)
#selecting data for plot
Imp_nut<-cd[,c(4,5,9,10,12)]
view(Imp_nut)
Imp_nut_top<-head(Imp_nut[order(Imp_nut$calories, decreasing=TRUE), ], 20)
view(Imp_nut_top)
Imp_nut_top<-as.matrix(Imp_nut_top)
mode(Imp_nut_top)="numeric"

#Imp_nut_top<-
max_cal<-max(Imp_nut$calories)
mx_cal_name<-filter(cd,cd$calories==160)$name

barplot(Imp_nut_top,beside = TRUE,col = rainbow(50))
barplot(table(Imp_nut1),beside = T,)

#gathering columns together

##df_nut=gather(Imp_nut_top,Nut_type,length,1:4)
#head(df_nut)
#view(df_nut)

#Grouping and summarizing data
#group by
# df_nut_grp<-group_by(df_nut,Nut_type)
# head(df_nut_grp)
# df_nut_grp<-group_by(df_nut,Nut_type,)