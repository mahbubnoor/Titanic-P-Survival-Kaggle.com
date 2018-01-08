args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_studentID.R -out prediction.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
print(paste("output file:", out_f))
#print(paste("number of folds      :", n_fold))

options(warn=-1)
library(e1071)
#library(randomForest)

train <- read.csv("train.csv",header = T)
test <- read.csv("test.csv",header = T)
#train[is.na(train)] <-0
#test[is.na(test)]  <-0
#svm_mod <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = train) #5
library(rpart)
#library(party)
train_temp <- train
train$Survived <- NULL
full_data <-rbind(train, test)
# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
full_data$Embarked[c(62, 830)] <- "S"
# Factorize embarkment codes.
full_data$Embarked <- factor(full_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
full_data$Fare[1044] <- median(full_data$Fare, na.rm = TRUE)
# For missing age values We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time we give method = "anova" since we are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = full_data[!is.na(full_data$Age),], method = "anova")
full_data$Age[is.na(full_data$Age)] <- predict(predicted_age, full_data[is.na(full_data$Age),])

# Split the data back into a train set and a test set
train <- full_data[1:891,]
test <- full_data[892:1309,]

train$Survived <- train_temp$Survived
#rforest_model <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, 
#                     controls=cforest_unbiased(ntree=1000, mtry=3)) #
#rforest_model <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, 
#                     controls=cforest_unbiased(ntree=1000)) #12  gave .78468 score
#prediction <- predict(rforest_model, test, OOB=TRUE, type = "response")

svm_mod <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train) #14  gave .79904
prediction <- predict(svm_mod, test)
output<-data.frame(PassengerId = test$PassengerId, Survived= prediction)
#out_f='C:/Users/Mahbub/Desktop/R_learning/Homeworks/project/performance_14.csv' 
write.csv(output, file= out_f, quote = F, row.names = F)

