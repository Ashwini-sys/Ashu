slr <- read.csv("D:/data science/linear regression/slr.csv", stringsAsFactors=TRUE)
View(slr)
str(slr)
plot(slr$Advt, slr$Sales)
mod<-lm(slr$Sales~slr$Advt) 
mod
summary(mod)
pred<-predict(lm(slr$Sales~slr$Advt))
pred

error<- residuals(lm(slr$Sales~slr$Advt))
error
summary(error)
as.matrix(error)
hist(error, col = "red")
boxplot(error, col = "red", horizontal = T)
plot(slr$Advt, error, main = "Linearity", col ="red")
obs_no<- c(1:12)
slr$obs_no<- NULL
slr$obs_no<- obs_no
slr
View(slr)
plot(slr$obs_no, error, main = "Independance of error", col = "green")

fit<-lm(mpg~ disp+hp+wt+drat, data = mtcars)
fit
summary(fit)

ki<- lm(mtcars$mpg ~ mtcars$disp+mtcars$hp+mtcars$wt+mtcars$drat)  
ki
summary(ki)

ti<- lm(mtcars$mpg~ mtcars$hp +mtcars$wt)
ti
summary(ti)

hi<- lm(mtcars$mpg~mtcars$hp)
hi
summary(hi)

fi<-lm(mtcars$mpg~ mtcars$wt)
fi
summary(fi)
install.packages("HH")
library("HH")
vif(fit)
vif(fit)>5
