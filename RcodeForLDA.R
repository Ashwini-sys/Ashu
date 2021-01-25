iris100_2 <- read.csv("D:/data science/Linear Descriminant Analysis/iris100_2.csv", stringsAsFactors=TRUE)
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("MASS")
library(tidyverse)
library(caret)
library(MASS)
theme_set(theme_classic())
iris <- read.csv("D:/data science/Linear Descriminant Analysis/iris100_2.csv", stringsAsFactors=TRUE)
dim(iris)
str(iris)

#split the data into train(80%) and test(20%)
set.seed(123)
training.samples<- iris$Species %>%
  createDataPartition(p = 0.8, list = F)
train.data<- iris[training.samples, ]
test.data<- iris[-training.samples, ]

#Estimate Preprocessing parameters
preproc.param<- train.data %>%
  preProcess(method = c("center", "scale"))
preproc.param

#transform the data using the estimated parameters
train.transformed<- preproc.param %>% predict(train.data)
train.transformed

test.transformed<- preproc.param %>% predict(test.data)
test.transformed

library(MASS)
model<- lda(Species~., data = train.transformed)
model
#formula we use in LDA Z= c+x1b1+x2b2+x3b3+x4b4
predictions<- model %>% predict(test.transformed)
predictions

y_prdict = predictions$class
y_prdict

y_obs = test.data$Species
y_obs

library(gmodels)
CrossTable(y_obs, y_prdict)

#Accuracy 0s and 1s are 10 10, 10/10 =1 correctly classified
mean(predictions$class==test.transformed$Species)
#seq from -3 to 3
y= seq(-2.925, 3, 0.075)
length(y)
head(y, 10)
lda.data<- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, y))+geom_point(aes(color = Species))

data("iris")
view(iris)
plot(model)

