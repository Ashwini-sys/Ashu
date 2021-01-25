student <- read.csv("D:/data science/Decision Tree Notes/student.csv", stringsAsFactors=TRUE)
#View(student)
str(student)
select_rows<- sample(1:nrow(student), round(0.2*nrow(student)), replace = F)
stuTest<- student[select_rows, ]
stuTrain<- student[-(select_rows), ]
modelRegTree<- lm(Mark~ Motivation + Gender + Age, data = stuTrain)
summary(modelRegTree)

modelRegTree<- lm(Mark~ Motivation + Gender + Age, data = stuTest)
summary(modelRegTree)

model_lr<-lm(Mark~Motivation+Gender+Age, data = stuTrain)
model_lr
summary(model_lr)

pred_2<-predict(model_lr,newdata = stuTest)
pred_2
#head(pred_1,nrow=5)
ME_LR<-sum(stuTest$Mark-pred_2)/nrow(stuTest)
ME_LR
RSS_LR<-sum(stuTest$Mark-pred_2)^2
RSS_LR
RMSE_LR<-sqrt(RSS_LR/nrow(stuTest))
RMSE_LR
MAPE_LR<-sum(abs(stuTest$Mark-pred_2)/stuTest$Mark)*300
MAPE_LR

#RMSE is more here than DT's RMSE value

