insurance <- read.csv("D:/data science/linear regression/insurance.csv", stringsAsFactors=TRUE)
View(insurance)
str(insurance)
b= table(insurance$sex)
b
barplot(b, col = c("blue", "red"))
c=table(insurance$children)
c
barplot(c, col = c("blue", "red", "yellow", "green", "purple", "pink"))

d=table(insurance$smoker)
d
barplot(d, col = c("blue", "red"))

a=table(insurance$region)
a
barplot(a, col = c("blue", "red", "yellow", "green"))
par(mfrow = c(2,1))
library(dplyr)
hist(insurance$age, col = "blue")
boxplot(insurance$age, col = "blue", horizontal = T)
summary(insurance$age)

hist(insurance$bmi, col = "green")
boxplot(insurance$bmi, col = "green", horizontal = T)
summary(insurance$bmi)

hist(insurance$charges, col = "red")
boxplot(insurance$charges, col = "red", horizontal = T)
summary(insurance$charges)

cor(insurance[c("age", "bmi", "children", "charges")])
library("psych")
pairs(insurance[c("age", "bmi", "children", "charges")])

library("psych")
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

ins_model<- lm(charges~ age+children+bmi+sex+smoker+region, data = insurance)
ins_model
summary(ins_model)
insurance$age2<-insurance$age^2
insurance$bmi30<- ifelse(insurance$bmi >= 30, 1, 0)

hist(insurance$age2, col = "purple")
boxplot(insurance$age2, col = "purple", horizontal = T)
summary(insurance$age2)

e = table(insurance$bmi30)
e
barplot(e, col = c("blue", "red"))

##as per R^2 value this is better model
ins_model2 <- lm(charges ~ age+age2+children+bmi+sex+bmi30*smoker+region, 
                 data = insurance)
summary(ins_model2)
