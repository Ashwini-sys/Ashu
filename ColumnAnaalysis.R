

cereals_data = cereals_data <- read.csv("D:/data science/Project1stOnCereals/cereals_data (1).csv", stringsAsFactors=TRUE)
cald = cereals_data <- read.csv("D:/data science/Project1stOnCereals/cereals_data (1).csv", stringsAsFactors=TRUE)

View(cald)
str(cald)
summary(cald)
dim(cald)

#analysis for calories
cald_calories = cald$calories
cald_calories = table(cald$calories)
cald_calories
library("dplyr")
boxplot(cald_calories, 
        col = "blue",
        main = "boxplot for calories")
boxplot(cald_calories, col = "red",
        main = "boxplot for calories after outlier treatment")
hist(cald_calories, col = "red")        
#analysis for protein 
cald_protein = cald$protein
summary(cald_protein)
cald_protein = table(cald$protein)
cald_protein
hist(cald_protein, col = "blue")
boxplot(cald_protein, 
        col = "red",
        main = "boxplot for protein")
#analysis for fat
cald_fat = cald$fat
summary(cald_fat)
cald_fat = table(cald$fat)
cald_fat
hist(cald_fat, col = "blue")
boxplot(cald_fat, 
        col = "red",
        main = "boxplot for protein")
#analysis for sodium
cald_sodium = cald$sodium
summary(cald_sodium)
cald_sodium = table(cald$sodium)
cald_sodium
hist(cald_sodium, col = "green")
boxplot(cald_sodium, 
        col = "red",
        main = "boxplot for protein")

#analysis for fiber
cald_fiber = cald$fiber
summary(cald$fiber)
cald_fiber = table(cald$fiber)
cald_fiber
hist(cald_fiber, col = "red")
boxplot(cald_fiber, 
        col = "red",
        main = "boxplot for protein")

#analysis for carbohydrates
cald_carbo = cald$carbo
summary(cald$carbo)
cald_carbo = table(cald$carbo)
cald_carbo
hist(cald_carbo, col = "blue")
boxplot(cald_fiber, 
        col = "red",
        main = "boxplot for protein")

#analysis for sugar
cald_sugars = cald$sugars
summary(cald$sugars)
cald_sugars = table(cald$sugars)
cald_sugars
hist(cald_sugars, col = "green")
boxplot(cald_sugars, 
        col = "red",
        main = "boxplot for protein")

#analysis for Potass
cald_potass = cald$potass
summary(cald$potass)
cald_potass = table(cald$potass)
cald_potass
hist(cald_potass, col = "blue")
boxplot(cald_potass, 
        col = "red",
        main = "boxplot for potass")

#analysis for vitamin
cald_vitamins = cald$vitamins
summary(cald$vitamins)
cald_vitamins = table(cald$vitamins)
cald_vitamins
hist(cald_vitamins, col = "red")
boxplot(cald_potass, 
        col = "red",
        main = "boxplot for vitamins")

#analysis for shelf
cald_shelf = cald$shelf
summary(cald$shelf)
cald_shelf = table(cald$shelf)
cald_shelf
hist(cald_shelf, col = "green")
boxplot(cald_shelf, 
        col = "red",
        main = "boxplot for shelf")

#analysis for weight
cald_weight = cald$weight
summary(cald$weight)
cald_weight = table(cald$weight)
cald_weight
hist(cald_weight, col = "blue")
boxplot(cald_weight, 
        col = "red",
        main = "boxplot for weight")

##analysis for cups
cald_cups = cald$cups
summary(cald$cups)
cald_cups = table(cald$cups)
cald_cups
hist(cald_cups, col = "red")
boxplot(cald_weight, 
        col = "red",
        main = "boxplot for cups")


##analysis for ratings
cald_rating = cald$rating
summary(cald$rating)
cald_rating = table(cald$rating)
cald_rating
hist(cald_rating, col = "green")
boxplot(cald_rating, 
        col = "red",
        main = "boxplot for rating")


