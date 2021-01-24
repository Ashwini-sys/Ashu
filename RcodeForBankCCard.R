`BankCreditCard.(1)` <-  read.csv("D:/data science/LogisticRegression/project3rd/BankCreditCard (1).csv", stringsAsFactors=TRUE)
BCCard<-  read.csv("D:/data science/LogisticRegression/project3rd/BankCreditCard (1).csv", stringsAsFactors=TRUE)
View(BCCard)
str(BCCard)
summary(BCCard)

#BCCard$Gender
a = table(BCCard$Gender)
a
barplot(a, col = c("Blue", "red"),
        main = "Gender",
        names.arg =  c("M", "F"))

#BCCard$Academic_Qualification
b=table(BCCard$Academic_Qualification)
b
barplot(b, col = c("Blue", "red", "green", "yellow", "pink", "black"),
        main = "Academic_Qualification",
        names.arg =  c("ug", "g", "pg", "pf", "ot", "un"))

#BCCard$Marital
c= table(BCCard$Marital)
c
barplot(c, col = c("Blue", "red", "green", "yellow"),
        main = "Marital",
        names.arg =  c("NA", "M", "s", "dkn"))

#BCCard$Age_Years continuous(numerical) 
par(mfrow= c(2,1))
hist(BCCard$Age_Years, col = "Blue")
boxplot(BCCard$Age_Years, horizontal = T, col = "Blue")
UB<-quantile(BCCard$Age_Years,0.75)+1.5*IQR(BCCard$Age_Years)
UB
LB<-quantile(BCCard$Age_Years,0.25)-1.5*IQR(BCCard$Age_Years)
LB
BCCard$Age_Years[BCCard$Age_Years>UB]<-UB
BCCard$Age_Years[BCCard$Age_Years<LB]<-LB
#library(psych)

##############################################################################
#color palate code
library(RColorBrewer)
brewer.pal.info
##############################################################################
#BCCard$Repayment_Status_Jan
d= table(BCCard$Repayment_Status_Jan)
d
barplot(d, col = brewer.pal(6,'Set3'),
        main = "Repayment_Status_Jan",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md", "6 md"))

#BCCard$Repayment_Status_Feb
e= table(BCCard$Repayment_Status_Feb)
e
barplot(e, col = brewer.pal(6,'PiYG'),
        main = "Repayment_Status_Feb",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md", "6 md"))
str(BCCard)

#BCCard$Repayment_Status_March
f= table(BCCard$Repayment_Status_March)
f
barplot(f, col = brewer.pal(6,'RdBu'),
        main = "Repayment_Status_March",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md", "6 md"))
str(BCCard)

#BCCard$Repayment_Status_April
g= table(BCCard$Repayment_Status_April)
g
barplot(g, col = brewer.pal(6,'OrRd'),
        main = "Repayment_Status_April",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md", "6 md"))
str(BCCard)

#BCCard$Repayment_Status_May
h= table(BCCard$Repayment_Status_May)
h
barplot(h, col = brewer.pal(6,'YlGnBu'),
        main = "Repayment_Status_May",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md"))

#BCCard$Repayment_Status_June
i= table(BCCard$Repayment_Status_June)
i
barplot(h, col = brewer.pal(6,'YlGnBu'),
        main = "Repayment_Status_June",
        names.arg =  c("no delay", "1 md", "2 md", "3 md", "4 md", "5 md"))

#BCCard$Credit_Amount# need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Credit_Amount, col = "Blue")
boxplot(BCCard$Credit_Amount, horizontal = T, col = "Blue")
UB<-quantile(BCCard$Credit_Amount,0.75)+1.5*IQR(BCCard$Credit_Amount)
UB
LB<-quantile(BCCard$Credit_Amount,0.25)-1.5*IQR(BCCard$Credit_Amount)
LB
BCCard$Credit_Amount[BCCard$Credit_Amount>UB]<-UB
BCCard$Credit_Amount[BCCard$Credit_Amount<LB]<-LB

#BCCard$Jan_Bill_Amount # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Jan_Bill_Amount, col = "Blue")
boxplot(BCCard$Credit_Amount, horizontal = T, col = "Blue")
UB<-quantile(BCCard$Jan_Bill_Amount,0.75)+1.5*IQR(BCCard$Jan_Bill_Amount)
UB
LB<-quantile(BCCard$Jan_Bill_Amount,0.25)-1.5*IQR(BCCard$Jan_Bill_Amount)
LB
BCCard$Jan_Bill_Amount[BCCard$Jan_Bill_Amount>UB]<-UB
BCCard$Jan_Bill_Amount[BCCard$Jan_Bill_Amount<LB]<-LB
library(psych)
describe(BCCard$Jan_Bill_Amount^2)

#BCCard$Feb_Bill_Amount # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Feb_Bill_Amount, col = "Blue")
boxplot(BCCard$Feb_Bill_Amount, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Feb_Bill_Amount,0.75)+1.5*IQR(BCCard$Feb_Bill_Amount)
UB
LB<-quantile(BCCard$Feb_Bill_Amount,0.25)-1.5*IQR(BCCard$Feb_Bill_Amount)
LB
BCCard$Feb_Bill_Amount[BCCard$Feb_Bill_Amount>UB]<-UB
BCCard$Feb_Bill_Amount[BCCard$Feb_Bill_Amount<LB]<-LB

#BCCard$March_Bill_Amount # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$March_Bill_Amount, col = "Blue")
boxplot(BCCard$March_Bill_Amount, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$March_Bill_Amount,0.75)+1.5*IQR(BCCard$March_Bill_Amount)
UB
LB<-quantile(BCCard$March_Bill_Amount,0.25)-1.5*IQR(BCCard$March_Bill_Amount)
LB
BCCard$March_Bill_Amount[BCCard$March_Bill_Amount>UB]<-UB
BCCard$March_Bill_Amount[BCCard$March_Bill_Amount<LB]<-LB

#BCCard$April_Bill_Amount # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$April_Bill_Amount, col = "Blue")
boxplot(BCCard$April_Bill_Amount, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$April_Bill_Amount,0.75)+1.5*IQR(BCCard$April_Bill_Amount)
UB
LB<-quantile(BCCard$April_Bill_Amount,0.25)-1.5*IQR(BCCard$April_Bill_Amount)
LB
BCCard$April_Bill_Amount[BCCard$April_Bill_Amount>UB]<-UB
BCCard$April_Bill_Amount[BCCard$April_Bill_Amount<LB]<-LB

#BCCard$May_Bill_Amount # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$May_Bill_Amount, col = "Blue")
boxplot(BCCard$May_Bill_Amount, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$May_Bill_Amount,0.75)+1.5*IQR(BCCard$May_Bill_Amount)
UB
LB<-quantile(BCCard$May_Bill_Amount,0.25)-1.5*IQR(BCCard$May_Bill_Amount)
LB
BCCard$May_Bill_Amount[BCCard$May_Bill_Amount>UB]<-UB
BCCard$May_Bill_Amount[BCCard$May_Bill_Amount<LB]<-LB

#BCCard$June_Bill_Amount # need to remove outliers

par(mfrow= c(2,1))
hist(BCCard$June_Bill_Amount, col = "Blue")
boxplot(BCCard$June_Bill_Amount, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$June_Bill_Amount,0.75)+1.5*IQR(BCCard$June_Bill_Amount)
UB
LB<-quantile(BCCard$June_Bill_Amount,0.25)-1.5*IQR(BCCard$June_Bill_Amount)
LB
BCCard$June_Bill_Amount[BCCard$June_Bill_Amount>UB]<-UB
BCCard$June_Bill_Amount[BCCard$June_Bill_Amount<LB]<-LB

#BCCard$Previous_Payment_Jan # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_Jan, col = "Blue")
boxplot(BCCard$Previous_Payment_Jan, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_Jan,0.75)+1.5*IQR(BCCard$Previous_Payment_Jan)
UB
LB<-quantile(BCCard$Previous_Payment_Jan,0.25)-1.5*IQR(BCCard$Previous_Payment_Jan)
LB
BCCard$Previous_Payment_Jan[BCCard$Previous_Payment_Jan>UB]<-UB
BCCard$Previous_Payment_Jan[BCCard$Previous_Payment_Jan<LB]<-LB

#Previous_Payment_Feb need to discuss
#BCCard$Previous_Payment_Feb # need to remove outliers , need to discuss
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_Feb, col = "Blue")
boxplot(BCCard$Previous_Payment_Feb, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_Feb,0.75)+1.5*IQR(BCCard$Previous_Payment_Feb)
UB
LB<-quantile(BCCard$Previous_Payment_Feb,0.25)-1.5*IQR(BCCard$Previous_Payment_Feb)
LB
BCCard$Credit_Amount[BCCard$Previous_Payment_Feb>UB]<-UB
BCCard$Credit_Amount[BCCard$Previous_Payment_Feb<LB]<-LB

#BCCard$Previous_Payment_March # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_March, col = "Blue")
boxplot(BCCard$Previous_Payment_March, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_March,0.75)+1.5*IQR(BCCard$Previous_Payment_March)
UB
LB<-quantile(BCCard$Previous_Payment_March,0.25)-1.5*IQR(BCCard$Previous_Payment_March)
LB
BCCard$Previous_Payment_March[BCCard$Previous_Payment_March>UB]<-UB
BCCard$Previous_Payment_March[BCCard$Previous_Payment_March<LB]<-LB

#BCCard$Previous_Payment_April # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_April, col = "Blue")
boxplot(BCCard$Previous_Payment_April, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_April,0.75)+1.5*IQR(BCCard$Previous_Payment_April)
UB
LB<-quantile(BCCard$Previous_Payment_April,0.25)-1.5*IQR(BCCard$Previous_Payment_April)
LB
BCCard$Previous_Payment_April[BCCard$Previous_Payment_April>UB]<-UB
BCCard$Previous_Payment_April[BCCard$Previous_Payment_April<LB]<-LB

#BCCard$Previous_Payment_May # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_May, col = "Blue")
boxplot(BCCard$Previous_Payment_May, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_May,0.75)+1.5*IQR(BCCard$Previous_Payment_May)
UB
LB<-quantile(BCCard$Previous_Payment_May,0.25)-1.5*IQR(BCCard$Previous_Payment_May)
LB
BCCard$Previous_Payment_May[BCCard$Previous_Payment_May>UB]<-UB
BCCard$Previous_Payment_May[BCCard$Previous_Payment_May<LB]<-LB

#BCCard$Previous_Payment_June # need to remove outliers
par(mfrow= c(2,1))
hist(BCCard$Previous_Payment_June, col = "Blue")
boxplot(BCCard$Previous_Payment_June, 
        horizontal = T, 
        col = "Blue")
UB<-quantile(BCCard$Previous_Payment_June,0.75)+1.5*IQR(BCCard$Previous_Payment_June)
UB
LB<-quantile(BCCard$Previous_Payment_June,0.25)-1.5*IQR(BCCard$Previous_Payment_June)
LB
BCCard$Previous_Payment_June[BCCard$Previous_Payment_June>UB]<-UB
BCCard$Previous_Payment_June[BCCard$Previous_Payment_June<LB]<-LB

#chi sq test
chisq.test(BCCard$Default_Payment , BCCard$Gender, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Academic_Qualification, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Marital, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_Jan, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_Feb, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_March, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_April, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_May, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Repayment_Status_June, correct = F)
chisq.test(BCCard$Default_Payment , BCCard$Previous_Payment_Jan, correct = F)

t.test(BCCard$Credit_Amount~BCCard$Default_Payment, var.equal=T)

#Model 1 removed +April_Bill_Amount+Previous_Payment_March(not significant)
model1<- glm(Default_Payment~Credit_Amount+Gender+Jan_Bill_Amount+Jan_Bill_Amount+Jan_Bill_Amount
             +Feb_Bill_Amount+March_Bill_Amount+Marital+Repayment_Status_Jan+Repayment_Status_Feb
             +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May+Repayment_Status_June+Previous_Payment_Jan
             +Previous_Payment_Feb+Previous_Payment_April+Previous_Payment_May+Previous_Payment_June, data = BCCard, family = binomial)

summary(model1)

predict1<- predict(model1, type="response")
head(predict1, 3)
BCCard$predict<-predict1
BCCard$predict1ROUND<-round(predict1, digits = 0)
table(BCCard$Default_Payment, predict1>=0.2)


###################################################################################################

#Model Academic_Qualification removed +Age_Years as both are not significant
model2<- glm(Default_Payment~Credit_Amount+Gender
           +Marital+Repayment_Status_Jan+Repayment_Status_Feb
            +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May
            +Repayment_Status_June, data = BCCard, family = binomial)
summary(model2)
library("HH")
vif(model2)

predict<- predict(model2, type="response")
head(predict, 3)
BCCard$predict<-predict
BCCard$predictROUND<-round(predict, digits = 0)
table(BCCard$Default_Payment, predict>=0.5)
table(BCCard$Default_Payment, predict>=0.2)
table(BCCard$Default_Payment, predict>=0.1)
table(BCCard$Default_Payment, predict>=0.08)

#splitting the data into training and test data
set.seed(2)
install.packages("caTools")
library(caTools)
split<-sample.split(BCCard,SplitRatio = 0.7)
split
train<-subset(BCCard,BCCard="TRUE")
test<-subset(BCCard,BCCard="FALSE")
train
test

##Train and predict
numberrows<-nrow(BCCard)
train<-head(BCCard,numberrows*.7)
View(train)
predict<-tail(BCCard,numberrows*.3)
View(predict)

#april June_bill_amount
model3<- glm(Default_Payment~Credit_Amount+Gender++Marital+Repayment_Status_Jan
             +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May
             +Repayment_Status_June, data = BCCard, family = binomial)

summary(model3)

set.seed(2)
install.packages("caTools")
library(caTools)
split<-sample.split(BCCard,SplitRatio = 0.7)
split
train<-subset(BCCard,BCCard="TRUE")
test<-subset(BCCard,BCCard="FALSE")
train
test

predict3<- predict(model3, type="response")
head(predict3, 3)
BCCard$predict<-predict3
BCCard$predictROUND<-round(predict3, digits = 0)
table(BCCard$Default_Payment, predict3>=0.2)
#table(BCCard$Default_Payment, predict3>=0.2)

###ROC plot for model3
install.packages("pROC")
library(pROC)
install.packages("randomForest")
library(randomForest)
set.seed(50)
roc(BCCard$Default_Payment, model3$fitted.values, plot = T)
roc.df <- data.frame((tpp=BCCard$specificities)*100,
thresolds=BCCard$thresolds)
head(roc.df)
tail(roc.df)
roc.df[roc.df$tpp>0.6 & roc.df$tpp <0.8]
roc(BCCard$Default_Payment, model3$fitted.values, plot = T, 
    legacy.axes = T,
    percent = T,
    xlab = "false positive percentage",
    ylab = "True Positive percentage",
    col = "#377eb8",
    lwd = 4, print.auc = T)

#plot.roc(BCCard$Default_Payment,rf.model3$votes[,1], percent = T,col = "#4daf4a", lwd=4,print.auc = T, add = T, print.auc, y=40)


model4<- glm(Default_Payment~Gender+
             Credit_Amount+Marital+Repayment_Status_Jan+Repayment_Status_Feb
             +Repayment_Status_March+Repayment_Status_May
             +Repayment_Status_June, data = BCCard, family = binomial, data = train)  
summary(model4)

###ROC plot for model4
#install.packages("pROC")
library(pROC)
#install.packages("randomForest")
library(randomForest)
set.seed(50)
roc(BCCard$Default_Payment, model4$fitted.values, plot = T)
roc.df <- data.frame((tpp=BCCard$specificities)*100,
                     thresolds=BCCard$thresolds)
head(roc.df)
tail(roc.df)
roc.df[roc.df$tpp>0.6 & roc.df$tpp <0.8]
roc(BCCard$Default_Payment, model3$fitted.values, plot = T, 
    legacy.axes = T,
    percent = T,
    xlab = "false positive percentage",
    ylab = "True Positive percentage",
    col = "#377eb8",
    lwd = 4, print.auc = T)

predict4<- predict(model4, type="response")
head(predict4, 3)
BCCard$predict4<-predict
BCCard$predictROUND<-round(predict4, digits = 0)
table(BCCard$Default_Payment, predict4>=0.2)


22337+2223/(22337+1027+4413+2223)

22357+2190/(22357+1007+4446+2190)


#########################################################################
install.packages("ROCR")
library(ROCR)
pred<- predict(model4, BCCard)
head(pred)
head(BCCard)
hist(pred)
pred<- prediction(pred, BCCard$Default_Payment)
eval<- performance(pred, "acc")
eval
plot(eval)
max <- which.max(slot(eval, "y.values")[[1]])
acc<- slot(eval, "y.values")[[1]][max]
acc
cut<- slot(eval, "x.values")[[1]][max]
cut
print(c(Accuracy=acc, cutoff=cut))

########################################
install.packages("smotefamily")
install.packages("ROSE")
library(smotefamily)
library(ROSE)
model8<- glm(Default_Payment~Gender+Marital+Repayment_Status_Jan+
               Repayment_Status_Feb+Repayment_Status_March+
               Repayment_Status_April+Repayment_Status_May+
               Repayment_Status_June, data = train1,
             family = binomial)
summary(model8)

model8<- glm(Default_Payment~Gender+Marital+Repayment_Status_Jan+
               Repayment_Status_Feb+Repayment_Status_March+
               Repayment_Status_April+Repayment_Status_May+
               Repayment_Status_June, data = train2,
             family = binomial)
summary(model8)

model8<- glm(Default_Payment~Gender+Marital+Repayment_Status_Jan+
               Repayment_Status_Feb+Repayment_Status_March+
               Repayment_Status_April+Repayment_Status_May+
               Repayment_Status_June, data = test2,
             family = binomial)
summary(model8)

library(caTools)
split<-sample.split(BCCard,SplitRatio = 0.7)
split
train2<-subset(BCCard,split="TRUE")
test2<-subset(BCCard,split="FALSE")
train2
test2
pridict_price<-predict(model8,train)
pridict_price
