# Titanic Data set
# Exercise
library(caret)
library(randomForest)
library(lava)
library(dplyr)

library(ggplot2)

# Read file
train <- read.csv("train.csv")
test <- read.csv("test.csv")

PassengerId <- test$PassengerId

# Separate Names
train$LastName <- gsub("^(.*), .*?\\..*$", "\\1", train$Name)
train$Title <- trim(gsub("^.*,(.*?)\\..*$", "\\1", train$Name))
train$Title <- lava::trim(train$Title)
train$FirstName <- gsub("^.*,.*?\\.[[:space:]]([[:graph:]]*).*$", "\\1", train$Name)
train$FirstName <- gsub("[\\(|\\)]", "", train$FirstName)

train$Title[train$Title=="Col"] = "Mr"

ggplot(data=as.data.frame(table(train$Title)), aes(Var1, Freq)) +
              geom_bar(stat="identity", fill="red")
            
train[c("Capt","Col"), Title]

# Convert passenger class to factor
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)

test$Pclass <- as.factor(test$Pclass)

# Remove NAs
trainComplete <- train[complete.cases(train), ]

# Fit
fitComplete <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                          data=trainComplete)

# Split test into complete and age missing
test[is.na(test$Age),]$Age <- -1
test[is.na(test$Fare),]$Fare <- -1

#Try to train a random forest model
predictComplete <- predict(fitComplete, test)

test$Survived <- predictComplete

output <- test[, -c(2, 3, 4, 5, 6, 7)]

write.csv(output, "output.csv", row.names=FALSE)