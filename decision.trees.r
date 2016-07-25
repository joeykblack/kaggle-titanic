setwd("C:/workspace/kaggle_titanic")
train <- read.csv("C:/workspace/kaggle_titanic/train.csv")
test <- read.csv("C:/workspace/kaggle_titanic/test.csv")

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data=train, 
             method="class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages("RGtk2", depen=T)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))

