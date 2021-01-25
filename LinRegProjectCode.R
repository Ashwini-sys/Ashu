
Property_Price <- read.csv("D:/data science/Project2ndlinearregression/Property_Price.csv", header=FALSE, stringsAsFactors=TRUE)
View(Property_Price)

Pr <- read.csv("D:/data science/Project2ndlinearregression/Property_Price.csv", header=FALSE, stringsAsFactors=TRUE)
View(Pr)

library("VIM")
Pr<-kNN(Pr)
Ma<- lm(Pr$Sale_Price~ Pr$Foundation_Type +
          Pr$Basement_Height + Pr$Basement_Condition 
        + Pr$Exposure_Level +Pr$BsmtFinType1 + 
          Pr$BsmtFinSF1)
Ma
summary(Ma)

Mb<- lm(Pr$Sale_Price ~ Pr$Total_Basement_Area+
          Pr$BsmtUnfSF + Pr$BsmtFinSF1) 
Mb
summary(Mb)

mod= lm(Pr$Sale_Price ~ Pr$Total_Basement_Area+Pr$BsmtUnfSF +Pr$BsmtFinSF1)
mod
summary(mod)

plot(Sale_Price~Foundation_Type, data = Pr, col = "red")

results<-aov (Sale_Price~Foundation_Type , data = Pr)
summary(results)

TukeyHSD(results)

##Zoning class
plot(Sale_Price~Zoning_Class, data = Pr, col = "red")
results<-aov (Sale_Price~Zoning_Class , data = Pr)
summary(results)
TukeyHSD(results)

#Lane_Type 
plot(Sale_Price~Lane_Type , data = Pr, col = "red")
results<-aov (Sale_Price~Lane_Type  , data = Pr)
summary(results)
TukeyHSD(results)


# suman's code
PPT<-read.csv("D:/data science/projectlinearregression/Property_Price.csv", stringsAsFactors=TRUE)
str(PPT)
library(VIM)
PPT<-kNN(PPT) #NA value removed
## Numerical Column analysis
str(PPT)
#Building_Class
par(mfrow=c(2,1))
bul_class=table(PPT$Building_Class)
barplot(bul_class)
boxplot(PPT$Building_Class,horizontal = T,col = 'blue')
#3 outliers

UB<-quantile(PPT$Building_Class,0.75)+1.5*IQR(PPT$Building_Class)
UB
PPT$Building_Class[PPT$Building_Class>UB]<-UB

#Lot_Extent(SQUARE WILL HELP)
Lot_Extent1=table(PPT$Lot_Extent)
barplot(Lot_Extent1)
boxplot(PPT$Lot_Extent,horizontal = T,col = 'blue')

UB<-quantile(PPT$Lot_Extent,0.75)+1.5*IQR(PPT$Lot_Extent)
UB
LB<-quantile(PPT$Lot_Extent,0.25)-1.5*IQR(PPT$Lot_Extent)
LB
PPT$Lot_Extent[PPT$Lot_Extent>UB]<-UB
PPT$Lot_Extent[PPT$Lot_Extent<LB]<-LB



#Lot_Size(Log or Square, oout liers not fixing)
Lot_Size1=table(PPT$Lot_Size)
barplot(Lot_Size1)
boxplot(PPT$Lot_Size,horizontal = T,col = 'blue')

#Overall_Material(square)
Overall_Material1=table(PPT$Overall_Material)
barplot(Overall_Material1)
boxplot(PPT$Overall_Material,horizontal = T,col = 'blue')

#House_Condition(square will increase)

barplot(table(PPT$House_Condition))
boxplot(PPT$Overall_Material,horizontal = T,col = 'blue')

#"Construction_Year",(log or square root)
barplot(table(PPT$Construction_Year))
boxplot(PPT$Construction_Year,horizontal = T,col = 'blue')

LB<-quantile(PPT$Construction_Year,0.25)-1.5*IQR(PPT$Construction_Year)
LB
PPT$Construction_Year[PPT$Construction_Year<LB]<-LB


##"Remodel_Year",(end poit have higher value, squareroot)
barplot(table(PPT$Remodel_Year))
boxplot(PPT$Remodel_Year,horizontal = T,col = 'blue')


#"Brick_Veneer_Area", very less magnitude, can be checked for outliers
barplot(table(PPT$Brick_Veneer_Area))
boxplot(PPT$Brick_Veneer_Area,horizontal = T,col = 'blue')


#"BsmtFinSF1",
barplot(table(PPT$BsmtFinSF1))
boxplot(PPT$BsmtFinSF1,horizontal = T,col = 'blue')


#"BsmtFinSF2",
barplot(table(PPT$BsmtFinSF2))
boxplot(PPT$BsmtFinSF2,horizontal = T,col = 'blue',main="BsmtFinSF2")


#"BsmtUnfSF",
barplot(table(PPT$BsmtUnfSF))
boxplot(PPT$BsmtUnfSF,horizontal = T,col = 'blue')
UB<-quantile(PPT$BsmtUnfSF,0.75)+1.5*IQR(PPT$BsmtUnfSF)
UB
PPT$BsmtUnfSF[PPT$BsmtUnfSF>UB]<-UB

#"Total_Basement_Area"
barplot(table(PPT$Total_Basement_Area))
boxplot(PPT$Total_Basement_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$Total_Basement_Area,0.75)+1.5*IQR(PPT$Total_Basement_Area)
UB
PPT$Total_Basement_Area[PPT$Total_Basement_Area>UB]<-UB
LB<-quantile(PPT$Construction_Year,0.25)-1.5*IQR(PPT$Construction_Year)
LB
PPT$Construction_Year[PPT$Construction_Year<LB]<-LB

## "First_Floor_Area",(square  )
barplot(table(PPT$First_Floor_Area))
boxplot(PPT$First_Floor_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$First_Floor_Area,0.75)+1.5*IQR(PPT$First_Floor_Area)
UB
PPT$First_Floor_Area[PPT$First_Floor_Area>UB]<-UB


##"Second_Floor_Area", square may help
barplot(table(PPT$Second_Floor_Area))
boxplot(PPT$Second_Floor_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Second_Floor_Area,0.75)+1.5*IQR(PPT$Second_Floor_Area)
UB
PPT$Second_Floor_Area[PPT$Second_Floor_Area>UB]<-UB


#"LowQualFinSF",  squareroot or similar ....
barplot(table(PPT$LowQualFinSF))
boxplot(PPT$LowQualFinSF,horizontal = T,col = 'blue')

#"Grade_Living_Area",
barplot(table(PPT$Grade_Living_Area))
boxplot(PPT$Grade_Living_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Grade_Living_Area,0.75)+1.5*IQR(PPT$Grade_Living_Area)
UB
PPT$Grade_Living_Area[PPT$Grade_Living_Area>UB]<-UB


##"Underground_Full_Bathroom", squarerooot
barplot(table(PPT$Underground_Full_Bathroom))
barplot(table(sqrt(PPT$Underground_Full_Bathroom)))

boxplot(PPT$Underground_Full_Bathroom,horizontal = T,col = 'blue')
UB<-quantile(PPT$Underground_Full_Bathroom,0.75)+1.5*IQR(PPT$Underground_Full_Bathroom)
UB
PPT$Underground_Full_Bathroom[PPT$Underground_Full_Bathroom>UB]<-UB



#"Underground_Half_Bathroom",  need to study later
barplot(table(PPT$Underground_Half_Bathroom))
boxplot(PPT$Underground_Half_Bathroom,horizontal = T,col = 'blue')


#"Full_Bathroom_Above_Grade", square
barplot(table(PPT$Full_Bathroom_Above_Grade))
boxplot(PPT$Full_Bathroom_Above_Grade,horizontal = T,col = 'blue')

#"Half_Bathroom_Above_Grade",
barplot(table(PPT$Half_Bathroom_Above_Grade))
boxplot(PPT$Half_Bathroom_Above_Grade,horizontal = T,col = 'blue')


#"Bedroom_Above_Grade", upper md lover limit not required
barplot(table(PPT$Bedroom_Above_Grade))
boxplot(PPT$Bedroom_Above_Grade,horizontal = T,col = 'blue')


#UB<-quantile(PPT$Bedroom_Above_Grade,0.75)+1.5*IQR(PPT$Bedroom_Above_Grade)
UB
#PPT$Bedroom_Above_Grade[PPT$Bedroom_Above_Grade>UB]<-UB
#LB<-quantile(PPT$Bedroom_Above_Grade,0.25)-1.5*IQR(PPT$Bedroom_Above_Grade)
LB
#PPT$Bedroom_Above_Grade[PPT$Bedroom_Above_Grade<LB]<-LB



##"Kitchen_Above_Grade",
barplot(table(PPT$Kitchen_Above_Grade))
boxplot(PPT$Kitchen_Above_Grade,horizontal = T,col = 'blue')


#"Rooms_Above_Grade", perfect , if require can remoe outliers
barplot(table(PPT$Rooms_Above_Grade))
boxplot(PPT$Rooms_Above_Grade,horizontal = T,col = 'blue')



#"Fireplaces",
barplot(table(PPT$Fireplaces))
boxplot(PPT$Fireplaces,horizontal = T,col = 'blue')
UB<-quantile(PPT$Fireplaces,0.75)+1.5*IQR(PPT$Fireplaces)
UB
PPT$Fireplaces[PPT$Fireplaces>UB]<-UB


#"Garage_Built_Year",
barplot(table(PPT$Garage_Built_Year))
boxplot(PPT$Garage_Built_Year,horizontal = T,col = 'blue')

#"Garage_Size",
barplot(table(PPT$Garage_Size))
boxplot(PPT$Garage_Size,horizontal = T,col = 'blue')

UB<-quantile(PPT$Garage_Size,0.75)+1.5*IQR(PPT$Garage_Size)
UB
PPT$Garage_Size[PPT$Garage_Size>UB]<-UB


##"Garage_Area"  need to relook

barplot(table(PPT$Garage_Area))
boxplot(PPT$Garage_Area,horizontal = T,col = 'blue')

##"W_Deck_Area",
barplot(table(PPT$W_Deck_Area))
boxplot(PPT$W_Deck_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$W_Deck_Area,0.75)+1.5*IQR(PPT$W_Deck_Area)
UB
PPT$W_Deck_Area[PPT$W_Deck_Area>UB]<-UB
LB<-quantile(PPT$W_Deck_Area,0.25)-1.5*IQR(PPT$W_Deck_Area)
LB
PPT$W_Deck_Area[PPT$W_Deck_Area<LB]<-LB




#"Open_Lobby_Area"
barplot(table(PPT$Open_Lobby_Area))
boxplot(PPT$Open_Lobby_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$Open_Lobby_Area,0.75)+1.5*IQR(PPT$Open_Lobby_Area)
UB
PPT$Open_Lobby_Area[PPT$Open_Lobby_Area>UB]<-UB
LB<-quantile(PPT$Open_Lobby_Area,0.25)-1.5*IQR(PPT$Open_Lobby_Area)
LB
PPT$Open_Lobby_Area[PPT$Open_Lobby_Area<LB]<-LB


#"Enclosed_Lobby_Area",
barplot(table(PPT$Enclosed_Lobby_Area))
boxplot(PPT$Enclosed_Lobby_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Enclosed_Lobby_Area,0.75)+1.5*IQR(PPT$Enclosed_Lobby_Area)
UB
PPT$Enclosed_Lobby_Area[PPT$Enclosed_Lobby_Area>UB]<-UB
LB<-quantile(PPT$Enclosed_Lobby_Area,0.25)-1.5*IQR(PPT$Enclosed_Lobby_Area)
LB
PPT$Enclosed_Lobby_Area[PPT$Enclosed_Lobby_Area<LB]<-LB



#"Three_Season_Lobby_Area",  ... log
barplot(table(PPT$Three_Season_Lobby_Area))
boxplot(PPT$Three_Season_Lobby_Area,horizontal = T,col = 'blue')

LB<-quantile(PPT$Three_Season_Lobby_Area,0.25)-1.5*IQR(PPT$Three_Season_Lobby_Area)
LB
PPT$Three_Season_Lobby_Area[PPT$Three_Season_Lobby_Area<LB]<-LB



#"Screen_Lobby_Area"   ## need too check
barplot(table(PPT$Screen_Lobby_Area))
boxplot(PPT$Screen_Lobby_Area,horizontal = T,col = 'blue')


#"Pool_Area",

barplot(table(PPT$Pool_Area))
boxplot(PPT$Pool_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Pool_Area,0.75)+1.5*IQR(PPT$Pool_Area)
UB
PPT$Pool_Area[PPT$Pool_Area>UB]<-UB



#"Miscellaneous_Value",
barplot(table(PPT$Miscellaneous_Value))
boxplot(PPT$Miscellaneous_Value,horizontal = T,col = 'blue')
UB<-quantile(PPT$Pool_Area,0.75)+1.5*IQR(PPT$Pool_Area)
UB
PPT$Miscellaneous_Value[PPT$Miscellaneous_Value>UB]<-UB



#"Month_Sold",   square may give better
barplot(table(PPT$Month_Sold))
boxplot(PPT$Month_Sold,horizontal = T,col = 'blue')
UB<-quantile(PPT$Month_Sold,0.75)+1.5*IQR(PPT$Month_Sold)
UB
PPT$Month_Sold[PPT$Month_Sold>UB]<-UB



#"Year_Sold", square
barplot(table(PPT$Year_Sold))
boxplot(PPT$Year_Sold,horizontal = T,col = 'blue')
UB<-quantile(PPT$Year_Sold,0.75)+1.5*IQR(PPT$Year_Sold)
UB
PPT$Year_Sold[PPT$Year_Sold>UB]<-UB
#"Sale_Price"
##Test for all numeric column
library("psych")
pairs.panels(PPT[c("Building_Class","Lot_Extent","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Sale_Price")])
                   
pairs.panels(PPT[c("Garage_Size","Garage_Area","W_Deck_Area","Open_Lobby_Area","Sale_Price")])

pairs.panels(PPT[c("Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])

cor(PPT[c("Building_Class","Lot_Extent","Lot_Size","Overall_Material","House_Condition","Construction_Year","Remodel_Year","Brick_Veneer_Area","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","Total_Basement_Area","First_Floor_Area","Second_Floor_Area","LowQualFinSF","Grade_Living_Area","Underground_Full_Bathroom","Underground_Half_Bathroom","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])

##Model 1

model3_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade
              +Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade
              +Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Road_Type+Utility_Type+
                Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature, data=PPT)
summary(model3_lm)

#catogrical
#Road_Type+Utility_Type+Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature

model4_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom
              +Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces
              +Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area
              +Pool_Area+Month_Sold+Year_Sold+Road_Type+Utility_Type+Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature, data=PPT)

summary(model4_lm)

#catogrical
#Road_Type+Utility_Type+Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature

model4_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)

summary(model4_lm)

#model 5

model5_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type
              +Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)

summary(model5_lm)

##My model
##Total numerical columns + selected Categorical column by anova
Ma<- lm(PPT$Sale_Price~ PPT$Zoning_Class + PPT$Lane_Type+ PPT$Property_Shape + PPT$Land_Outline+ PPT$Lot_Configuration+ PPT$Neighborhood+ PPT$Condition1+ PPT$Condition2 + PPT$House_Type+ PPT$House_Design+ PPT$Roof_Design+ PPT$Roof_Quality+ PPT$Exterior1st+ PPT$Exterior2nd+ PPT$Brick_Veneer_Type+ PPT$Exterior_Material+ PPT$Exterior_Condition+ PPT$Foundation_Type+ PPT$Basement_Height+ PPT$Basement_Condition + PPT$Exposure_Level 
        + PPT$BsmtFinType1 + PPT$BsmtFinType2+ PPT$Heating_Type+ PPT$Heating_Quality+ PPT$Air_Conditioning+ PPT$Electrical_System+ PPT$Kitchen_Quality+ PPT$Functional_Rate+ PPT$Fireplace_Quality+ PPT$Garage+ PPT$Garage_Finish_Year+ PPT$Garage_Quality+ PPT$Garage_Condition+ PPT$Pavedd_Drive
        + PPT$Pool_Quality+ PPT$Fence_Quality+ PPT$Sale_Type+ PPT$Sale_Condition
        + PPT$Building_Class + PPT$Lot_Extent+ PPT$Lot_Size + PPT$Overall_Material + PPT$House_Condition 
        + PPT$Construction_Year + PPT$Remodel_Year + PPT$Brick_Veneer_Area + PPT$BsmtFinSF1 + PPT$BsmtFinSF2 + PPT$BsmtUnfSF + PPT$Total_Basement_Area + PPT$First_Floor_Area+ PPT$Second_Floor_Area + PPT$LowQualFinSF
        + PPT$Grade_Living_Area + PPT$Underground_Full_Bathroom + PPT$Underground_Half_Bathroom + PPT$Full_Bathroom_Above_Grade + PPT$Half_Bathroom_Above_Grade + PPT$Bedroom_Above_Grade + PPT$Kitchen_Above_Grade + PPT$Rooms_Above_Grade + PPT$Fireplaces
        + PPT$Garage_Built_Year + PPT$Garage_Size + PPT$Garage_Area + PPT$W_Deck_Area + PPT$Open_Lobby_Area + PPT$Enclosed_Lobby_Area + PPT$Three_Season_Lobby_Area + PPT$Screen_Lobby_Area + PPT$Pool_Area + PPT$Miscellaneous_Value + PPT$Month_Sold + PPT$Year_Sold)
Ma
summary(Ma)

#after descarding column by anova and pairs panel correlation
Mb<- lm(PPT$Sale_Price~ PPT$Zoning_Class + PPT$Lane_Type+ PPT$Property_Shape + PPT$Land_Outline+ PPT$Lot_Configuration+ PPT$Neighborhood+ PPT$Condition1+ PPT$Condition2 + PPT$House_Type+ PPT$House_Design+ PPT$Roof_Design+ PPT$Roof_Quality+ PPT$Exterior1st+ PPT$Exterior2nd+ PPT$Brick_Veneer_Type+ PPT$Exterior_Material+ PPT$Exterior_Condition+ PPT$Foundation_Type+ PPT$Basement_Height+ PPT$Basement_Condition + PPT$Exposure_Level 
  + PPT$BsmtFinType1 + PPT$BsmtFinType2+ PPT$Heating_Type+ PPT$Heating_Quality+ PPT$Air_Conditioning+ PPT$Electrical_System+ PPT$Kitchen_Quality+ PPT$Functional_Rate+ PPT$Fireplace_Quality+ PPT$Garage+ PPT$Garage_Finish_Year+ PPT$Garage_Quality+ PPT$Garage_Condition+ PPT$Pavedd_Drive
  + PPT$Pool_Quality+ PPT$Fence_Quality+ PPT$Sale_Type+ PPT$Sale_Condition
  + PPT$Lot_Extent+ PPT$Lot_Size + PPT$Bedroom_Above_Grade + PPT$Bedroom_Above_Grade_imp + PPT$Fireplaces + PPT$Garage_Built_Year + PPT$Garage_Size + PPT$W_Deck_Area + PPT$Screen_Lobby_Area + PPT$Month_Sold + PPT$Year_Sold)

Mb
summary(Mb)

library("psych")
describe(PPT$Garage_Area)
describe(PPT)
summary(PPT)


##sold year,+W_Deck_Area removed
model7_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Pool_Area+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)


summary(model7_lm)

##
##sold year,+W_Deck_Area removed
model8_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Bedroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Screen_Lobby_Area+Pool_Area+Month_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Zoning_Class+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)


summary(model8_lm)


##sold year,+W_Deck_Area removed
PPT$Open_Lobby_Area_sq<-PPT$Open_Lobby_Area^2
PPT$Enclosed_Lobby_Area_sq<-PPT$Enclosed_Lobby_Area^2
##+Garage_Built_Year+Screen_Lobby_Area
#Bedroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+

PPT$areacom<-PPT$Garage_Built_Year*PPT$Rooms_Above_Grade

cor(PPT[c("Sale_Price","Garage_Built_Year","Half_Bathroom_Above_Grade","Rooms_Above_Grade")])
model9_lm<-lm(Sale_Price~areacom+Garage_Built_Year+Garage_Size+Fireplaces+Pool_Area+Month_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Zoning_Class+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)


summary(model9_lm)
View(PPT)


#PPT$cond<-PPT$Garage_ConditionPPT$Condition1

#PPT$years<-PPT$Garage_Built_Year*PPT$Garage_Finish_Year

cor(PPT[c("Sale_Price","Heating_Quality","Roof_Quality","Garage_Quality","Fence_Quality")])

SUBMODEL20<-lm(Sale_Price~Exterior_Condition+Exterior_Material,data = PPT)
summary(SUBMODEL20)

#PPT$System <- PPT$Electrical_System +PPT$Air_Conditioning
#28 variable
PPT$Bsmt<- PPT$Basement_Height+PPT$BsmtFinType1model9_lm<-lm(Sale_Price~areacom+Garage_Size+Fireplaces+Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Zoning_Class+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=PPT)

summary(model9_lm)

#pairs.panels(PPT[c("Garage_Size","Fireplaces","Month_Sold","Land_Outline","Lot_Configuration","Property_Slope","Neighborhood","House_Type","Heating_Quality","Zoning_Class","Roof_Quality","Brick_Veneer_Type","Pool_Area","Exterior_Material","Foundation_Type","Basement_Height","Exposure_Level","BsmtFinType1","Heating_Type","Air_Conditioning","Electrical_System","Functional_Rate","Garage","Garage_Quality","Pavedd_Drive","Sale_Type","Sale_Condition","Sale_Price")])

#splitting the data into training and test data
set.seed(2)
install.packages("caTools")
library(caTools)
split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test

##Train and predict
numberrows<-nrow(PPT)
train1<-head(PPT,numberrows*.7)
predict1<-tail(PPT,numberrows*.3)

PPT$allsqr<-(Garage_Size*Fireplaces*Month_Sold)^2 ##Multiple R-squared:  0.8865, Adjusted R-squared:  0.8709
model12_lm_train<-lm(Sale_Price~areacom*Fireplaces*Garage_Size*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=train1)
summary(model12_lm_train)
#str(model11_lm)
PPT$areacom

model13_lm_predict<-lm(Sale_Price~areacom*Fireplaces*Garage_Size*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=predict1)
summary(model13_lm_predict)

pred<-predict(model11_lm,train)

##final - Neighborhood to be added , Brick_Veneer_Area->to be removed
PPT$allsqr<-(Garage_Size*Fireplaces*Month_Sold)^2
model14_lm_train<-lm(Sale_Price~areacom*Fireplaces*Garage_Size+Exterior_Material+Full_Bathroom_Above_Grade+Kitchen_Quality+Brick_Veneer_Area+Foundation_Type+Basement_Height+Garage+Sale_Type, data=train1)
summary(model14_lm_train)

##--House_Type, Heating_Quality, Roof_Quality, Brick_Veneer_Type, to be added ->Exposure_Level, to be added -> BsmtFinType1,Functional_Rate to be added -> Air_Conditioning,Air_Conditioning,Heating_Type,Pavedd_Drive,Garage_Quality,Sale_Condition(less effective ),
PPT$gr_com<-PPT$Garage_Size*PPT$Brick_Veneer_Area
fit=lm(Sale_Price~areacom*Fireplaces*gr_com+BsmtFinSF1+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = PPT)
summary(fit)

##
train1<-head(PPT,numberrows*.7)
View(train1)
predict1<-tail(PPT,numberrows*.3)
train1$Sale_Price2<-train1$Sale_Price
barplot(table(train1$Sale_Price2))
boxplot(train1$Sale_Price2,horizontal = T,col = 'blue')
UB<-quantile(train1$Sale_Price2,0.75)+1.5*IQR(train1$Sale_Price2)
UB
train1$Sale_Price2[train1$Sale_Price2>UB]<-UB
LB<-quantile(train1$Sale_Price2,0.25)-1.5*IQR(train1$Sale_Price2)
LB
train1$Sale_Price2[train1$Sale_Price2<LB]<-LB

##
#"Fireplaces",
barplot(table(PPT$Fireplaces))
boxplot(PPT$Fireplaces,horizontal = T,col = 'blue')
UB<-quantile(PPT$Fireplaces,0.75)+1.5*IQR(PPT$Fireplaces)
UB
PPT$Fireplaces[PPT$Fireplaces>UB]<-UB

##final model rohit
fit=lm(Sale_Price2~areacom*Fireplaces*gr_com+BsmtFinSF1+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = train1)
summary(fit)
library("HH")
vif(fit)
vif(fit)>5
error<- residuals(lm(Sale_Price2~areacom*Fireplaces*gr_com+BsmtFinSF1+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = train1))
error
summary(error)

hist(error, col = "red" )
boxplot(error,col = "red", horizontal = T)
describe(error)


##areacom*Fireplaces*gr_com

PPT$Fire<-sqrt(PPT$Fireplaces)
PPT$Bsmt1<- (PPT$BsmtFinSF1)^2 
barplot(PPT$Bsmt1)
#Editable last model
fit=lm(Sale_Price2~BS+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = train1)
summary(fit)
c= table(PPT$Remodel_Year)
c
