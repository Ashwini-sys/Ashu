install.packages("ISLR")
library(ISLR)
attach(Carseats)
dim(Carseats)
str(Carseats)
#View(Carseats)
write.csv(Carseats, 'D:/data science/Decision Tree Notes/carseats1.csv')

#sales is continuous data
#if Sales<=8, no else yes
#new variable high
High<- ifelse(Sales<=8, 'No', 'Yes')
dim(Carseats)
str(High)
# include high in data carseats
Carseats<- data.frame(Carseats, High)
dim(Carseats)
names(Carseats)
#bulid tree
library(tree)
#convert high into factor
Carseats$High<- as.factor(Carseats$High)

tree.carseats<- tree(High~. -Sales, Carseats)
#(no comma after high~)

summary(tree.carseats)

