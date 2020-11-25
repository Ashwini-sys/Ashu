2+3

# importing file cs2m
cs2m <- read.csv("D:/notes of data sci. class/cs2m.csv",
                 stringsAsFactors=TRUE)
#View(cs2m)
dim(cs2m)
str(cs2m)
summary(cs2m)

cs2m$Prgnt <- as.factor(cs2m$Prgnt)
str(cs2m)

cs2m$AnxtyLH <- as.factor(cs2m$AnxtyLH)
str(cs2m)

cs2m$DrugR <- as.factor(cs2m$DrugR)

# age

hist(cs2m$Age,
     col = "red",
     xlab = "Age", 
     ylab = "Counts",
     main = "Histogram of Age")
#in R 15k packages/library
# you probably need 60 libraries

install.packages("psych")

library(psych)

describe(cs2m$Age)

boxplot(cs2m$Age,
        horizontal = T,
        col = "red",
        xlab = "Age")
b = c(21, 22, 20.5, 22, 23, 24.5, 24.7, 25, 31, 27, 26.5, 25.3)

hist(b)
boxplot(b,horizontal = T)

a = table(cs2m$AnxtyLH)
a

barplot(a,
        names.arg = c("Low", "High"),
        col = c("green", "red"),
        xlab = "Anxiety Levels",
        ylab = "Counts",
        main = "Barplot of Anxiety")
# for all continuous columns/Vars

ss = cs2m[ ,c(3,2,1)]
str(ss)

library(psych)

#complete cases

str(cs2m)
cs2m$Prgnt <- as.numeric(cs2m$Prgnt)
cs2m$AnxtyLH <- as.numeric(cs2m$AnxtyLH)
cs2m$DrugR <- as.numeric(cs2m$DrugR)
View(cs2m)

##need to import file t

str(t)
summary(t)
t_complete = t[complete.cases(t),]
summary(t_complete)
dim(t_complete)

#installing package dplyr

install.packages("dplyr")
library(dplyr)
cs2m_mutate <- mutate(cs2m, Chlstr_bp = Chlstrl/BP)
head(cs2m_mutate)

##creating new variable by another method 

cs2m_1$Chlstr_bp <-  cs2m_1$Chlstrl/cs2m_1$BP
View(cs2m_1)


m = cs2m
j = m
dim(j)

#installing package reshape

install.packages("reshape")
library(reshape)

j = rename(j, c(DrugR = 'Reaction',
                Prgnt = 'Pregnant'))
variable.names(j)
names(j)

names(j)[5] = "Anxiety"
names(j)[2] = "Cholesterol"

variable.names(j)

library(dplyr)
cs2m_asce<- arrange(cs2m, Age)
head(cs2m_asce)

cs2m_desc = arrange(cs2m, desc(Chlstrl))
head(cs2m_desc)

##ploting plot for quiz gpa final

grades1 <- subset(grades, select = c(quiz1, gpa, final))
head(grades1)

library(psych)

pairs.panels(ss)
a= grades[, c(13, 18, 19)]
a
pairs.panels(a)
variable.names(grades)
names(grades)
colnames(grades)

m = as.matrix(cs2m)

#plotting heatmap

heatmap(m)
mm = as.matrix(mtcars)
heatmap(mm, col = rainbow(100))

grades4<- select(grades, quiz1, gpa, final)
grades4
#2 stands for column
apply(cs2m, 2, mean)

##giving error by this type
mean(cs2m)
##correct code
mean(cs2m$BP)
# 1 stands for row
apply(cs2m, 1, mean)

#avg of of the columns by cylinder

by(mtcars[,-2], mtcars$cyl, colMeans)

#one variable's mean across a categorical variable

tapply(cs2m$BP, cs2m$Prgnt, mean)
tapply(grades$gpa, grades$ethnicity, mean)

#Select only final > 60 and view few top rows 

final_60 <- subset(grades, final>60)
head(final_60)

#Compare box plots of final of all 105 observations and with final>60
##boxplot red

boxplot(grades$final, 
        main = "Box Plot of final", 
        col = "red")

##boxplot blue
boxplot(final_60$final, 
        main = "Box Plot of final>60", 
        col = "Blue")

##Compare correlation between final and gpa in all 105 observations and in subset final > 60

cor.test(grades$gpa, grades$final)
cor.test(final_60$gpa, final_60$final)

#cs2m file; Age between 20 & 32 filter

cs2m_1<- filter(cs2m, Age>20 & Age<32)
cs2m_1

#cs2m file; Age between 20 & 32 subset

cs2m_2<- filter(cs2m, Age>20 & Age<32)
cs2m_2

# using subset with small change

cs2m_3 <- subset(cs2m, Age > 19 & Age < 31)
cs2m_3

#for white

ethnicity_white <- subset(grades, ethnicity == 4)
head(ethnicity_white)


boxplot(ethnicity_white$final, main = "Box Plot of final for WHITES", 
        col = "green")

#for hispanic

ethnicity_hispanic <-subset(grades, ethnicity == 5)
head(ethnicity_hispanic)
boxplot(ethnicity_hispanic$final, main = "Box Plot of 
        final for Hispanic",
        col = "red")
# code for merge
#concatenate
#1 step
ethnicity_hispanic <- subset(grades, ethnicity == 4 | ethnicity == 5)
head(ethnicity_hispanic)
#2nd type
ethnicity_hispanic <- subset(grades, ethnicity == 4:5)
ethnicity_hispanic

#3nd type
filter(grades, ethnicity == 4 | ethnicity == 5)
View(ethnicity_hispanic )


#Transform final with square root and re code as new variable

grades$sqrtfinal <-sqrt(grades$final)
head(grades)

#histogram for final

hist(grades$final,
     col = "red",
     xlab = "grades$final", 
     ylab = "Frequency",
     main = "Histogram of Final")

#histogram for sqrtfinal

hist(grades$sqrtfinal,
     col = "blue",
     xlab = "grades$sqrtfinal", 
     ylab = "Frequency",
     main = "Histogram of SQRTFinal")

#compare histogram of final and sqrtfinal 
##need to ask

cor.test(grades$final, grades$sqrtfinal)
cor.test(final_60$final, final_60$sqrtfinal)

#Convert final into two categories of final 
#[one, <60 and second >60, 60 will fall in > 60]

grades$catgryfinal <- ifelse(grades$final<60, yes = "final<60",
                             no = "final>60")
head(grades)

#Convert final into categories with increment of 5
#[40-45=1, 46-50=2, 51-55=3, 56-60=4, 61-65=5, 66-70=6, 71-75=7]

##need to ask
grades$final_cat <- cut(grades$final, breaks =seq(40, 75, 5),
                        labels =c("final1", "final2", 
                                  "final3", "final4", 
                                  "final5", "final6", 
                                  "final7"))
head(grades)
table(grades$catgryfinal)

table(grades$final_cat)

#Create new variable agecat as
#categories of Age

install.packages("readr")

library("readr")
k <- read.csv("D:/notes of data sci. class/cs2m.csv")
str(k)
summary(k$Age)
m=k
summary(m)
View(m)
#within function
m <- within(m, {
  agecat<-NA
  agecat[Age>=15 & Age <=25] <- 'Low'
  agecat[Age>=26 & Age <=40] <- 'Middle'
  agecat[Age>41] <- 'High'}) 

head(m, 3)

m <- within(m, {
  agecat <- NA
  agecat[Age>=15 & Age <=25] <- 'Low'
  agecat[Age>=26 & Age <=40] <- 'Middle'
  agecat[Age>41] <- 'High'}) 

head(m,3)

#Converting ethnicity into two categories
#[category 1= 1, 3& 5; category 2 = 2 & 4

grades$cateth<-grades$ethnicity

grades$cateth[grades$cateth == 1| grades$cateth == 3|
                grades$cateth ==5] =1
grades$cateth [grades$cateth == 2| grades$cateth == 4] =2

View(grades)

sam<- sample(x=1:nrow(grades), size = 0.2*nrow(grades))
grade20<-grades[sam, ]
head(grade20)
grade20
sam

summarise(cs2m, mean_age = mean(Age, na.rm = T),
          median_age = median(Age, na.rm = T ) )


#Pipe for doing many things in one go

df<- cs2m %>%
  filter(Prgnt == 1)%>%        
  select(BP, Age, DrugR)%>%
  mutate(BP_Age = BP/Age)
head(df)
df

#Pipe for doing many things in one go

library("dplyr")

grades%>%
  group_by(ethnicity)%>%        
  summarise(avg_gpa = mean(gpa),
            avg_final = mean(final),
            avg_total = mean(total)) 

tapply(grades$gpa, grades$ethnicity, mean)

library("dplyr")
grades%>% group_by(ethnicity, gender)%>%
  select(gpa, ethnicity, final, gender)%>%
  summarise(avggpa = mean(gpa),
            avgfinal = mean(final))
library("dplyr")
grades%>%group_by(ethnicity, gender)%>%
  select(gpa, ethnicity, final, gender)%>%
  summarise(avggpa = mean(gpa),
            avgfinal = mean(final))%>%
  filter(avggpa>2.25)

library("dplyr")
mtcars%>% group_by(cyl, gear)%>%
  select(mpg, cyl, wt, gear, am)%>%
  summarise(avgmpg = mean(mpg),
            avgwt = mean(wt))%>%
  filter(avgmpg>15)

#***************missing values*************

#datasets
datasets::ability.cov

data<- c(88, 95, 85, NA, 76, 69, 78, NA, 70, 68)
data
#Find Mean
mean(data)
#calculation of mean considers all 
#values..two NA values are creating problem
str(data)
summary(data)
##
mean(data, na.rm = T)
median(data)
median(data, na.rm = T) 
sd(data)
sd(data, na.rm = T)

is.na(data)
!is.na(data)
data[!is.na(data)]
imp <- read.csv("D:/notes of data sci. class/imp.csv", 
                stringsAsFactors=TRUE)
View(imp)

dim(imp)
str(imp)        
summary(imp)
imp$variable3<- as.factor(imp$variable3)
str(imp)
summary(imp)
table(imp$variable3)
imp$variable3[is.na(imp$variable3)]<- 2
summary(imp)

hist(imp$variable1, col= "red")

hist(imp$variable2, col= "green")

mean(imp$variable1)
mean(imp$variable1, na.rm = T)

imp$variable1[is.na(imp$variable1)]<- mean(imp$variable1, na.rm = T)
imp$variable1
summary(imp)
summary(imp$variable1)

median(imp$variable2)
median(imp$variable2, na.rm = T)
imp$variable2[is.na(imp$variable2)]<-median(imp$variable2, na.rm = T)
imp$variable2
summary(imp)
summary(imp$variable2)

install.packages("VIM")
library("VIM")
imp=read.csv("D:/notes of data sci. class/imp.csv")
imp

imp<- kNN(imp)
p<- c(sample(60:100, 20), 15, 23, 150, 168)
p
str(p)
library("dplyr")

boxplot(p, 
        main = "Box Plot of p", 
        col = "red")

LB<- quantile(p, 0.25) - 1.5*IQR(p)
LB

quantile(p,0.25)

#diff method 

quantile(p, 0.75)

UB<- quantile(p, 0.75)+ 1.5*IQR(p)
UB

p<- p[p>LB &p<UB]
p
str(p)

boxplot(p,main = "Box plot of P",   
        col="blue")

q<- c(sample(60:100, 20), 15, 23, 150, 168)
q

boxplot(q,main = "Box plot of q",   
        col="red")

quantile(q, 0.25)

quantile(q, 0.75)

LB<- quantile(q, 0.25)- 1.5*IQR(q)
LB

UB<- quantile(q, 0.75)+ 1.5*IQR(q)
UB

q[q > UB]<- UB
q[q < LB]<- LB
q
#box plot without outliers

boxplot(q,main = "Box plot of q",   
        col="green")

framingham <- read.csv("D:/notes of data sci. class/framingham.csv") 

View(framingham)
fram<- framingham
dim(fram)
str(fram)

boxplot(fram$BMI, col = "purple")

UB<- quantile(fram$BMI, 0.75, na.rm = T)+
  1.5*IQR(fram$BMI, na.rm = T) 
UB
LB<- quantile(fram$BMI, 0.25, na.rm = T)-
  1.5*IQR(fram$BMI, na.rm = T) 
LB
length(fram$BMI[fram$BMI < LB | fram$BM > UB])

fram$BMI[fram$BMI > UB]<-UB
fram$BMI[fram$BMI < LB]<-LB

#box plot without outlier

boxplot(fram$BMI, col = "blue")

