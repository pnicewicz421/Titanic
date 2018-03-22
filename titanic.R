# Titanic Data set
# Exercise
library(caret)
library(randomForest)
library(lava)
library(dplyr)
library(plyr)

library(ggplot2)

# Read file
train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$Survived = NA
full = rbind(train, test)

# Separate Names
full$LastName <- gsub("^(.*), .*?\\..*$", "\\1", full$Name)
full$Title <- trim(gsub("^.*,(.*?)\\..*$", "\\1", full$Name))
full$Title <- lava::trim(full$Title)
full$FirstName <- gsub("^.*,.*?\\.[[:space:]]([[:graph:]]*).*$", "\\1", full$Name)
full$FirstName <- gsub("[\\(|\\)]", "", full$FirstName)

# Titles will be used to estimage age
# Reduce unusual titles to the four major buckets
full$Title[full$Title=="Col"] = "Mr"
full$Title[full$Title=="Capt"] = "Mr"
full$Title[full$Title=="Don"] = "Mr"
full$Title[full$Title=="Mme"] = "Mrs"
full$Title[full$Title=="Dr" & full$Sex=="female"] = "Mrs"
full$Title[full$Title=="Dr" & full$Sex=="male"] = "Mrs"
full$Title[full$Title=="Jonkheer"] = "Mr"
full$Title[full$Title=="Lady"] = "Mrs"
full$Title[full$Title=="Dona"] = "Mrs"
full$Title[full$Title=="Major"] = "Mr"
full$Title[full$Title=="Mlle"] = "Miss"
full$Title[full$Title=="Ms"] = "Mrs"
full$Title[full$Title=="Rev"] = "Mr"
full$Title[full$Title=="Sir"] = "Mr"
full$Title[full$Title=="the Countess"] = "Mrs"

# Group by Last Name or Ticket Number?
ByLastName <- full %>% group_by(LastName)

ggplot(data=as.data.frame(table(full$Title)), aes(Var1, Freq)) +
       geom_bar(stat="identity", fill="red") + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Find other missing features

# Impute missing values with the mean of each bucket
# Do the same for test set?
for (titlename in unique(full$Title)) {
    titleAge <- subset(full, Title==titlename & !is.na(Age), select=Age)
    meanTitleAge <- mean(titleAge$Age)
    print(paste0("Mean Age for a ", titlename, " is ", as.character(meanTitleAge))) 
    full$Age[full$Title==titlename & is.na(full$Age)] = meanTitleAge
}

# Break down ticket price by person
# Group by tickets
ticketNumbers <- plyr::count(full, c('Ticket'))
colnames(ticketNumbers)[2] <- "PeoplePerTicket"
full <- merge(full, ticketNumbers, by="Ticket", all.x=TRUE)
full <- full %>% dplyr::mutate(FarePerPerson = Fare / PeoplePerTicket)

# Impute the missing fare
MeanFare <- mean(subset(full, Pclass==3 & FarePerPerson < 19)$FarePerPerson)
full$Fare[is.na(full$Fare)] = MeanFare
full$FarePerPerson[is.na(full$FarePerPerson)] = MeanFare

# Group Siblings-Spouse and Parent-Child? and add self
full <- full %>% dplyr::mutate(TotalInGroup = SibSp + Parch + 1)

# Convert passenger class to factor
full$Pclass <- as.factor(full$Pclass)
full$Survived <- as.factor(full$Survived)
#full$TotalInGroup <- as.factor(full$TotalInGroup)

# Seperate into train and test sets again
test <- full[is.na(full$Survived),]
train <- full[!is.na(full$Survived),]

test$Set <- "test"
train$Set <- "train"
    
# Fit
#fit <- randomForest(Survived ~ Pclass + Sex + Age + TotalInGroup + Fare,
                          data=train)

fit <- rpart(Survived ~ Pclass + Sex + Age + TotalInGroup + Fare,
                    data=train)

#Try to train a random forest model
predict <- predict(fit, test)

test$Survived <- predict

#output <- rbind(train, test)

output <- test[, -c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]

write.csv(output, "output_3_17_2018.csv", row.names=FALSE)