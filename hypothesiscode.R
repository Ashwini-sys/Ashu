# 22 nov
library(gmodels)
CrossTable(cs2m$Prgnt, cs2m$AnxtyLH)

#One sample t.test
t.test(cs2m$Age , mu = 40, conf.level = 0.95)

t.test(cs2m$BP, mu = 173)
t.test(cs2m$BP, mu = 120) 

#import file grades, paired sample t.test
dim(grades)
t.test(grades$quiz1, grades$quiz2, paired = T)
mean(grades$quiz1)
mean(grades$quiz2)
7.47-7.98
diff = grades$quiz1 - grades$quiz2
diff
t.test(diff)
sd = sd(diff)
sd
### with mu = 0
t.test(x = grades$quiz1, y = grades$quiz2, alternative = "two.sided", mu = 0, paired = T)
t.test(grades$quiz1, grades$quiz2, paired = T)

#Independent Samples t test by assuming equal variance
t.test(cs2m$BP~cs2m$AnxtyLH)
t.test(cs2m$BP~cs2m$AnxtyLH, var.equal = T)
##28 Nov
##proportion
#A researcher believes that market size of diesel cars is 30%. For testing his belief, he had taken a sample of 
#130 cars and found 50 diesel cars. Ho: p=0.30
# Ans:more then 30% is the size of diesel cars,P value is less then 0.05:Rejected
prop.test(50,130, p=0.30, alternative = "two.sided", 
          conf.level = 0.95, correct = F)

#A researcher has found 10 stressed faculties out of a sample of 40 at Christ college 
#and 22 out of 50 at St. John.Ho: p1-p2 = 0
#Ans:More than P value is more than 0.05:Accepted need to ask
prop.test(c(10,22), c(40,50) ,alternative = "two.sided", 
          conf.level = 0.95, correct = F)

##import file salescity
salescity <- read.csv("D:/data science/hypothesis notes/salescity.csv", stringsAsFactors=TRUE)
View(salescity)
plot(sales~city, data = salescity, col = "blue")
plot(sales~city, data = salescity, col = heat.colors(4))

results<-aov(sales~city, data = salescity)
summary(results)
##Ans: The mean sales across betn all cities is, P value is less than 0.05,
#alternative hypothesis One cities sales 
#is different than all other cities sales

##second method like same as above

# showing differance between some groups
TukeyHSD(results)
plot(sales~city, data = salescity, col = heat.colors(4))
#chisquare test for association between two categorical variables
chisq.test(cs2m$AnxtyLH,cs2m$DrugR, correct = F)
##21 pg no running
library("gmodels")
CrossTable(cs2m$AnxtyLH, cs2m$DrugR)

16*15/30

##exercise

##Test whether there is significance diff between mean scores 
#of quiz1 and quiz3
##1)
t.test(x = grades$quiz1, y = grades$quiz3, 
       alternative = "two.sided", mu = 0, paired = T)


t.test(grades$quiz1, grades$quiz3, paired = T)
####Test whether there is significance diff between
#mean scores of quiz1 and quiz3
##2)
t.test(grades$quiz1, grades$quiz3, paired = T)
mean(grades$quiz1)
diff = grades$quiz1 - grades$quiz3
diff
t.test(diff)
sd = sd(diff)
sd

##3)
#Test whether Drug r and  anxiety levels were significantly associated
t.test(cs2m$DrugR~cs2m$AnxtyLH)

##4)
##Test whether sales of all cities is same
#file anovae
anovae <- read.csv("D:/data science/hypothesis notes/anovae.csv", stringsAsFactors=TRUE)
View(anovae)
plot(sale~city, data = anovae, col = "blue")

t.test(cs2m$BP~cs2m$DrugR)
t.test(cs2m$Age~cs2m$Prgnt)
t.test(cs2m$Age~cs2m$DrugR)

50/130
prop.test(60,140, p=0.20, alternative = "two.sided", 
          conf.level = 0.95, correct = F)
