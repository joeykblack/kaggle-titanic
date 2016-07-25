setwd("C:/workspace/kaggle_titanic")
train <- read.csv("C:/workspace/kaggle_titanic/train.csv")
test <- read.csv("C:/workspace/kaggle_titanic/test.csv")

library(rpart)


test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name, split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

table(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

table(combi$FamilySize)

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, seb="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train,
             method="class")
