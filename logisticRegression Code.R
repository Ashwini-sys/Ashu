cs2m <- read.csv("D:/data science/notes of data sci. class/all basic and data manupulation notes/cs2m.csv", stringsAsFactors=TRUE)
View(cs2m)
model<- glm(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH, data = cs2m, family = binomial())
model
predict<-predict(model, type = "response")
head(predict, 3)
cs2m$predict<-predict
cs2m$predictROUND<- round(predict, digits = 0)
table(cs2m$DrugR, predict>=0.5)

creditset <- read.csv("D:/data science/linear regression/creditset.csv", stringsAsFactors=TRUE)
model<-glm(default10yr~ income+age+loan+LTI, data = creditset, family = binomial())
model
predict<-predict(model, type = "response")
head(predict, 3)
creditset$predict<-predict
creditset$predictROUND<- round(predict, digits = 0)
table(creditset$default10yr, predict>=0.05)
# addition of correct predictor
1524+281=1805
#overall accuracy is above total divide by total no of obs=0.925
1850/2000=0.925

1524+2+193+281
