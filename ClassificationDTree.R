#install.packages("ISLR")
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
#?ifelse
dim(Carseats)
str(High)
# include high in data carseats
Carseats<- data.frame(Carseats, High)
dim(Carseats)
names(Carseats)
#bulid tree
library(tree)
# converted 'High' to factors 
Carseats$High <- as.factor(Carseats$High)
#(no comma after high~)
tree.carseats<- tree(High~. -Sales, Carseats)
summary(tree.carseats)
#plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

#train and test
set.seed(2)
train<- sample(1:nrow(Carseats),200)
Carseats.test<- Carseats[-train, ]
High.test<- High[-train]

tree.carseats1<- tree(High~. -Sales, Carseats, subset = train)
tree.pred<- predict(tree.carseats1, Carseats.test, type = 'class')
table(tree.pred, High.test)

(104+50)/2
summary(tree.carseats1)

#pruning tree
set.seed(3)
#cv stands for CROSS VALIDATION
#It will create trees with 1 to 19 terminal nodes (19 is chosen by tree!)
#FUN = prune.misclass means that classification error rate be the guiding rule for pruning

cv.carseats<- cv.tree(tree.carseats1, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
#plotting error
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b',
     col = "red",
     lwd = 2)
plot(cv.carseats$k, cv.carseats$dev,
     type = 'b',
     col = 'blue',
     lwd = 2 )
#prune the trees to 9 classes
prune.carseats<- prune.misclass(tree.carseats1, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred1<- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred1, High.test)
(97+58)/200

#bagging
#install.packages("randomForest")
library(randomForest)
set.seed(1)
#Carseats=Select(Carseats,-13)

bag.carseats<- randomForest(High~. -Sales, Carseats, subset = train,
                            mtry = 10, importance = T)
dim(Carseats) #400 12 (1 is high another is sales)
importance(bag.carseats) # which variables are imp
# above code price shelvoc and age are top three

varImpPlot(bag.carseats, col= 'red', pch = 10, cex = 1.25)
bag.carseats

test.pred.bag<- predict(bag.carseats, newdata = Carseats[-train, ],
                        type = 'class')
table(test.pred.bag, High.test)
(104+61)/2

sqrt(10)


set.seed(1)
rf.carseats<- randomForest(High~. -Sales, Carseats,
                           subset = train,
                           mtry = 3,
                           importance = T
                           )
dim(Carseats)
importance(bag.carseats)
test.pred.rf<- predict(rf.carseats, newdata = Carseats[-train, ],
                        type = 'class')
table(test.pred.rf, High.test)
(109+58)/2

