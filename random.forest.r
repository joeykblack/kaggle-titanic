setwd("C:/workspace/kaggle_titanic")
train <- read.csv("C:/workspace/kaggle_titanic/train.csv")
test <- read.csv("C:/workspace/kaggle_titanic/test.csv")
# combi in feature.r

sample(1:10, replace=TRUE)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

# remove blanks
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"

# remove NA
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] = median(combi$Fare, na.rm=TRUE)

# factor size must be < 32
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

# randomForest
install.packages('randomForest')
library(randomForest)
set.seed(415)
summary(train)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                          Embarked + Title + FamilySize + FamilyID2,
                    data=train,
                    importance=TRUE,
                    ntree=2000)
varImpPlot(fit)


# conditional inference trees
install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                                      Embarked + Title + FamilySize + FamilyID,
               data = train,
               controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type="response")
submit <- data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file="cforest.csv", row.names = FALSE)
