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

test$Survived = NA
full = rbind(train, test)

# Separate Names
full$LastName <- gsub("^(.*), .*?\\..*$", "\\1", full$Name)
full$Title <- trim(gsub("^.*,(.*?)\\..*$", "\\1", full$Name))
full$Title <- lava::trim(full$Title)
full$FirstName <- gsub("^.*,.*?\\.[[:space:]]([[:graph:]]*).*$", "\\1", full$Name)
full$FirstName <- gsub("[\\(|\\)]", "", full$FirstName)

full$OtherName <- ifelse(grepl('^.+\\({1}.+[[:space:]](\\w*)\\){1}$', full$Name), 
       gsub('^.+\\({1}.+[[:space:]](\\w*)\\){1}$', '\\1', full$Name),
       "")
      
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

GetMissing <- function(group) {
  # order from highest to lowest
  group <- sort(group, decreasing=TRUE)
  if (length(group) > 1) {
    output <- group[1] - sum(group[2:length(group)])
  } else
    output <- group
}

# Start with the Group Number as the ticket number. 
# We will modify this field to join some groups
# that had separate ticket entries
#full$GroupNumber <- full$Ticket

# Get Number in Group

set.seed(1232313)

full$Ticket <- as.character(full$Ticket)

InGroups <- full %>% group_by_at(vars(LastName, Ticket)) %>%
  dplyr::mutate(GroupNumber=sample(1:1000000,1))


write.csv(InGroups, "output.csv", row.names=FALSE)


set.seed(1232313)
InGroups <- full %>% group_by(Ticket) %>% 
    dplyr::mutate(GroupNumber=sample(1:1000000,1))
                  
                  #NumLastNames = length(unique(LastName)),
                  #LastNames = ifelse(NumLastNames==1, LastName, paste0(LastName, collapse = ";")))

# ungroup

AdjustGroupNumber <- function(GroupNumber, LastName) {
  print (GroupNumber)
  print (LastName)
  print ("%%%%%%%%%%%")
  NumGroupNumbers <- length(unique(GroupNumber))
  if (NumGroupNumbers == 1) {
    GroupNumber
  } else {
    newGroupNumber = sample(1:1000000,1)
    gn <- unique(GroupNumber)
    for (each in gn) {
      print ("$$$$$$$$$$$$$")
      print(each)
      print(newGroupNumber)
      InGroups[which(InGroups$GroupNumber==each),]$GroupNumber <- newGroupNumber
    }
    newGroupNumber
  }
 }

InGroupsOutput <- InGroups %>% group_by(LastName) %>%
    mutate(GroupNumber=AdjustGroupNumber(GroupNumber, LastName))


#############
# GET ERROR #
#############

ErrorTable <- InGroups %>% 
  dplyr::group_by(GroupNumber) %>%
  dplyr::mutate(TotalSibSp = sum(SibSp),
                TotalParch = sum(Parch),
                MissingSibSp = TotalSibSp %% 2,
                MissingParch = TotalParch %% 2) %>%
  dplyr::filter(MissingSibSp == 1 | MissingParch == 1)

write.csv(ErrorTable, "errortable.csv", row.names=FALSE)


############
set.seed(1232313)
InGroups <- full %>% 
              dplyr::group_by(Ticket) %>%
              dplyr::mutate(GroupNumber=sample(1:1000000,1))

write.csv(InGroups, "output.csv", row.names=FALSE)

GroupOtherNames <- function(OtherName) {
  OtherName <- OtherName[which(nchar(OtherName)!=0)]
  if (length(OtherName) > 1) {
    paste0(OtherName, collapse=";") 
  } else if (length(OtherName) == 1) {
    OtherName
  } else if (length(OtherName) == 0) {
    ""
  }
}

GroupSummary <- InGroups %>% 
  dplyr::group_by(Ticket) %>%
  dplyr::mutate(NumberInGroup = n(),
                NumberInFamily = Sibsp + Parch + 1,
                  TotalSibSp = sum(SibSp),
                   TotalParch = sum(Parch),
                   GroupNumber = unique(GroupNumber),
                   LastNames = length(unique(LastName)),
                   LastName = ifelse(LastNames==1, LastName, paste0(LastName, collapse = ";")),
                   OtherName = GroupOtherNames(OtherName),
                   MissingSibSp = TotalSibSp %% 2,
                   MissingParch = TotalParch %% 2)

ErrorGroup <- as.list(GroupSummary %>% dplyr::filter(MissingSibSp > 0 | MissingParch > 0) %>% 
              select(GroupNumber))$GroupNumber

# match these suckers
# we are looking for families (where lastname is the same) with

ChooseGroupNumber <- function(GroupNumber) {
  if (length(unique(GroupNumber)) == 1) {
    unique(GroupNumber)
  } else {
    Err <- GroupNumber[which(GroupNumber %in% ErrorGroup)]
    if (length(Err)==1) {
      Err
    } else if (length(Err) > 1) {
      max(Err)
    } else {
      max(GroupNumber)
    }
  }
}

GroupSummary2 <- InGroups %>% 
  #dplyr::filter(GroupNumber %in% ErrorGroup)
  dplyr::group_by(LastName) %>%
  dplyr::mutate(GroupNumber = ChooseGroupNumber(GroupNumber))



ErrorGroup2 <- as.list(GroupSummary2 %>% dplyr::filter(MissingSibSp > 0 | MissingParch > 0) %>% 
                        select(GroupNumber))$GroupNumber


JoinedFamilies <- JoinedFamilies %>% mutate(LastName=strsplit(LastName, ";")) %>%
  tidyr::unnest(LastName)

JoinedFamilies <- JoinedFamilies %>% mutate(Ticket=strsplit(Ticket, ";")) %>%
  tidyr::unnest(Ticket)
  
  



JoinedFamilies <- JoinedFamilies %>% dplyr::filter(TotalSibSp%%2 != 0 | TotalParch%%2 != 0) 

JoinedFamilies <- JoinedFamilies %>% mutate(LastName=strsplit(LastName, ";")) %>%
  tidyr::unnest(LastName)

JoinedFamilies <- JoinedFamilies %>% mutate(Ticket=strsplit(Ticket, ";")) %>%
                  tidyr::unnest(Ticket)

# 23 left that are not grouped correctly ... manually reassign?

write.csv(JoinedFamilies, "outstanding.csv", row.names=FALSE)



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