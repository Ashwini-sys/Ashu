#Project 2
data=Property_Price_Train
summary(Property_Price_Train)
library(psych)
View(mtcars)
table(data$Condition1)
table(data$Condition2)

relation=data[,c(18,19,20,21,27,81)]
pairs.panels(relation)
mod=lm(data$Sale_Price~data$House_Type)
summary(mod)
cor(data$Brick_Veneer_Area, data$Sale_Price)

plot(data$Overall_Material, data$Sale_Price, main = "Scatterplot")
is.factor(data$Condition1)
str(data)
#one hot coding, cato

plot(data$Exterior_Condition, data$Sale_Price)

table(Property_Price_Train$Roof_Quality)

mod=lm(data$Sale_Price~data$Overall_Material+data$Zoning_Class+
         data$Road_Type)
summary(mod)
