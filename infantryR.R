names <- c('joel', 'chris', 'julie', 'mary', 'sprina')
names
percent <- c('85', '88' ,'92', '95', '89')
percent
lunch <- c('biryani', 'chicken kabab', 'biryani', 'chicken kabab', 'veg pulao')
lunch
str(lunch)
yummy <- as.factor(lunch)
yummy
str(yummy)
joy<- data.frame(names, percent, yummy)
joy
joy$yummy
joy$names
joy$percent
joy[[3]]
joy[[2]]
joy[[1]]
joy[["yummy"]]
infantry <- read.csv("D:/notes of data sci. class/infantry.csv")
View(infantry)
targets <- read.csv("D:/notes of data sci. class/targets.csv")
View(targets)
steve<- merge(x = infantry, y = targets)
steve
View(steve)
write.csv(steve,"C:/Users/khile/Desktop/steve.csv")
View(steve)
