student <- read.csv("D:/data science/Decision Tree Notes/student.csv", stringsAsFactors=TRUE)
#View(student)
names(student)
nrow(student)
ncol(student)
str(student)
select_rows<- sample(1:nrow(student), round(0.2*nrow(student)), replace = F)
stuTest<- student[select_rows, ]
stuTrain<- student[-(select_rows), ]
#install.packages("tree")
library(tree)
modelRegTree<- tree(Mark~ Motivation + Gender + Age, data = stuTrain)
plot(modelRegTree)
text(modelRegTree, pretty = 0, cex = 0.75)
pred<- predict(modelRegTree, newdata = stuTest ) 
head(pred, nrow = 5)
head(pred, n= 5)
ME<- sum(stuTest$Mark - pred)/nrow(stuTest)
ME
RSS<- sum(stuTest$Mark - pred)^2
RSS
RMSE<- sqrt(RSS/nrow(stuTest))
RMSE
MAPE<- sum(abs(stuTest$Mark-pred)/student$Mark)*100
MAPE

#install.packages("caTools")
library(caTools)
library(tree)
set.seed(1)
split<- sample.split(student$Grade, SplitRatio = 0.70)
studentTrain<- subset(student, split == TRUE)
studentTest<- subset(student, split == FALSE)
table(student$Grade)

table(studentTrain$Grade)
table(studentTest$Grade)

prop.table(table(studentTest$Grade))
table(studentTest$Grade)

prop.table(table(studentTest$Grade))
modelClassTree<- tree(Grade ~ Motivation + Age + Gender, data = studentTrain)
plot(modelClassTree)
#print(tree)
text(modelClassTree, pretty = 0, cex = 0.75)

pred2<- predict(modelClassTree, newdata = studentTest, type = "class")

conf<- table(studentTest$Grade, pred2)
conf

OAA<- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]+conf[6,6])/sum(conf)
OAA


#logistic regression
#install.packages("nnet")
library(nnet)
student1 <-  read.csv("D:/data science/Decision Tree Notes/student.csv", stringsAsFactors=TRUE)

library(caTools)
library(tree)
set.seed(1)
split<-sample.split(student1$Grade,SplitRatio = 0.70)
studentTrain1<-subset(student1,split==TRUE)
studentTest1<-subset(student1,split==FALSE)
table(student1$Grade)
table(studentTrain1$Grade)
table(studentTest1$Grade)
prop.table(table(studentTest1$Grade))
table(studentTest1$Grade)

####Logistic reression model#we can not 

model21<-multinom(Grade ~ Motivation+Age+Gender, data = studentTrain1)
summary(model21)

###Accurecy
###prediction
predict(model21,studentTrain1)

predict(model21,studentTrain1[c(1,10,15),],type = 'prob')
#Accuracy of model

cm<-table(predict(model21),studentTrain1$Grade)
print(cm)

1-sum(diag(cm))/sum(cm)  ##wrong prediction

#correct prediction
sum(diag(cm))/sum(cm)
