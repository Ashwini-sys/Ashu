#Project 1
str(cereals_data)
summary(cereals_data)
data=cereals_data[, c(4:16)]
describe(data)
hist(cereals_data$calories)
boxplot(cereals_data$calories, horizontal = T)
cereals_data$name=as.numeric(cereals_data$name)
cereals_data$mfr=as.numeric(cereals_data$mfr)
cereals_data$type=as.numeric(cereals_data$type)
a=as.matrix(cereals_data)
heatmap(a)
table(cereals_data$calories)
data=cereals_data[, c(4:16)]
library(psych)
pairs.panels(data)
hist(cereals_data$protein)
boxplot(cereals_data$protein, horizontal = T)
table(cereals_data$protein)
summary(cereals_data$protein)
describe(cereals_data$protein)

hist(cereals_data$fat)
boxplot(cereals_data$fat, horizontal = T)
table(cereals_data$fat)
summary(cereals_data$fat)
describe(cereals_data$fat)

relation=cereals_data[, c(4:6)]
pairs.panels(relation)

hist(cereals_data$sodium)
boxplot(cereals_data$sodium, horizontal = T)
table(cereals_data$sodium)
summary(cereals_data$sodium)
describe(cereals_data$sodium)

hist(cereals_data$fiber)
boxplot(cereals_data$fiber, horizontal = T)
table(cereals_data$fiber)
summary(cereals_data$fiber)
describe(cereals_data$fiber)

hist(cereals_data$carbo)
boxplot(cereals_data$carbo, horizontal = T)
table(cereals_data$carbo)
summary(cereals_data$carbo)
describe(cereals_data$carbo)

relation=cereals_data[, c(4, 7,8)]
pairs.panels(relation)

hist(cereals_data$sugars)
boxplot(cereals_data$sugars, horizontal = T)
table(cereals_data$sugars)
summary(cereals_data$sugars)
describe(cereals_data$sugars)

hist(cereals_data$potass)
boxplot(cereals_data$potass, horizontal = T)
table(cereals_data$potass)
summary(cereals_data$potass)
describe(cereals_data$potass)

relation=cereals_data[, c(4, 9, 10, 11)]
pairs.panels(relation)

hist(cereals_data$vitamins)
boxplot(cereals_data$vitamins, horizontal = T)
table(cereals_data$vitamins)
summary(cereals_data$vitamins)
describe(cereals_data$vitamins)

hist(cereals_data$shelf)
boxplot(cereals_data$shelf, horizontal = T)
table(cereals_data$shelf)
summary(cereals_data$shelf)
describe(cereals_data$shelf)

hist(cereals_data$weight)
boxplot(cereals_data$weight, horizontal = T)
table(cereals_data$weight)
summary(cereals_data$weight)
describe(cereals_data$weight)

relation=cereals_data[, c(4, 12, 13, 14)]
pairs.panels(relation)

hist(cereals_data$cups)
boxplot(cereals_data$cups, horizontal = T)
table(cereals_data$cups)
summary(cereals_data$cups)
describe(cereals_data$cups)

hist(cereals_data$rating)
boxplot(cereals_data$rating, horizontal = T)
table(cereals_data$rating)
summary(cereals_data$rating)
describe(cereals_data$rating)

relation=cereals_data[, c(4, 15, 16)]
pairs.panels(relation)

is.na(cereals_data)
table(cereals_data$sugars)
mean(cereals_data$sugars, na.rm=T)
library(VIM)
cereals_data=kNN(cereals_data)

summary(cereals_data)
describe(cereals_data$sugars)
boxplot(cereals_data$sugars, horizontal = T)
table(cereals_data$sugars)
hist(cereals_data$sugars)


describe(cereals_data$potass)
boxplot(cereals_data$potass, horizontal = T)
table(cereals_data$potass)
hist(cereals_data$potass)

describe(cereals_data$carbo)
boxplot(cereals_data$carbo, horizontal = T)
table(cereals_data$carbo)
hist(cereals_data$carbo)


relation=cereals_data[, c(4, 9, 10, 11)]
pairs.panels(relation)

LB=quantile(cereals_data$calories,0.25)-1.5*IQR(cereals_data$calories)
LB
UB=quantile(cereals_data$calories,0.75)+1.5*IQR(cereals_data$calories)
UB
cereals_data$calories[cereals_data$calories>UB]=UB
cereals_data$calories[cereals_data$calories<LB]=LB
cereals_data$calories
boxplot(cereals_data$calories, horizontal=T ,col="green")
str(cereals_data$calories)
summary(cereals_data$calories)
hist(cereals_data$calories)
describe(cereals_data$calories)
table(cereals_data$calories)

LB=quantile(cereals_data$protein,0.25)-1.5*IQR(cereals_data$protein)
LB
UB=quantile(cereals_data$protein,0.75)+1.5*IQR(cereals_data$protein)
UB
cereals_data$protein[cereals_data$protein>UB]=UB
cereals_data$protein[cereals_data$protein<LB]=LB
cereals_data$protein
boxplot(cereals_data$protein, horizontal=T ,col="green")
summary(cereals_data$protein)
hist(cereals_data$protein)
describe(cereals_data$protein)
table(cereals_data$protein)

relation=cereals_data[, c(4, 5, 6)]
pairs.panels(relation)


LB=quantile(cereals_data$sodium, 0.25)-1.5*IQR (cereals_data$sodium)
UB=quantile(cereals_data$sodium, 0.75)+1.5 * IQR(cereals_data$sodium)
cereals_data$sodium[cereals_data$sodium<LB]=LB
cereals_data$sodium[cereals_data$sodium>UB]=UB
boxplot(cereals_data$sodium, horizontal=T ,col="green")
summary(cereals_data$sodium)
hist(cereals_data$sodium)
describe(cereals_data$sodium)
table(cereals_data$sodium)

UB=quantile(cereals_data$fiber, 0.75)+1.5*IQR (cereals_data$fiber)
cereals_data$fiber[cereals_data$fiber>UB]=UB
boxplot(cereals_data$fiber, horizontal=T ,col="green")
summary(cereals_data$fiber)
hist(cereals_data$fiber)
describe(cereals_data$fiber)
table(cereals_data$fiber)

relation=cereals_data[, c(4, 7, 8)]
pairs.panels(relation)

UB=quantile(cereals_data$potass, 0.75)+1.5*IQR (cereals_data$potass)
cereals_data$potass[cereals_data$potass>UB]=UB
boxplot(cereals_data$potass, horizontal=T ,col="green")
summary(cereals_data$potass)
hist(cereals_data$potass)
describe(cereals_data$potass)
table(cereals_data$potass)

relation=cereals_data[, c(4, 9, 10, 11)]
pairs.panels(relation)

LB=quantile(cereals_data$vitamins, 0.25)-1.5*IQR (cereals_data$vitamins)
UB=quantile(cereals_data$vitamins, 0.75)+1.5*IQR (cereals_data$vitamins)
cereals_data$vitamins[cereals_data$vitamins<LB]=LB
cereals_data$vitamins[cereals_data$vitamins>UB]=UB
boxplot(cereals_data$vitamins, horizontal=T ,col="green")
summary(cereals_data$vitamins)
hist(cereals_data$vitamins)
describe(cereals_data$vitamins)
table(cereals_data$vitamins)

LB=quantile(cereals_data$weight, 0.25)-1.5*IQR (cereals_data$weight)
UB=quantile(cereals_data$weight, 0.75)+1.5*IQR (cereals_data$weight)
cereals_data$weight[cereals_data$weight<LB]=LB
cereals_data$weight[cereals_data$weight>UB]=UB
boxplot(cereals_data$weight, horizontal=T ,col="green")
summary(cereals_data$weight)
hist(cereals_data$weight)
describe(cereals_data$weight)
table(cereals_data$weight)

relation=cereals_data[,c(4,12,13,14)]
pairs.panels(relation)


UB=quantile(cereals_data$cups, 0.75)+1.5*IQR (cereals_data$cups)
cereals_data$cups[cereals_data$cups>UB]=UB
boxplot(cereals_data$cups, horizontal=T ,col="green")
summary(cereals_data$cups)
hist(cereals_data$cups)
describe(cereals_data$cups)
table(cereals_data$cups)

UB=quantile(cereals_data$rating, 0.75)+1.5*IQR (cereals_data$rating)
cereals_data$rating[cereals_data$rating>UB]=UB
boxplot(cereals_data$rating, horizontal=T ,col="green")
summary(cereals_data$rating)
hist(cereals_data$rating)
describe(cereals_data$rating)
table(cereals_data$rating)

relation=cereals_data[,c(4,15,16)]
pairs.panels(relation)

