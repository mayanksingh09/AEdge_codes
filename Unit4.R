#UNIT 4#

#CART

stevens <- read.csv("./data/stevens.csv")
str(stevens)

#Split data
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)

Train <- subset(stevens, spl == TRUE) 
Test <- subset(stevens, spl == FALSE)

library(rpart)
library(rpart.plot)

#CART model on stevens data
SteventsTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25) #class builds classification tree instead of regression trees

prp(SteventsTree) #Plot the tree

PredictCART <- predict(SteventsTree, newdata = Test, type = "class") #class to be written when making predictions for a CART model


#Confusion matrix
table(Test$Reverse, PredictCART)

(41+71)/(41+36+22+71) #Overall accuracy

#ROC curve for CART model

library(ROCR)
PredictROC <- predict(SteventsTree, newdata = Test) #Predictions in terms of probabilities instead of outcomes, probability of 0 & probability of 1

pred <- prediction(PredictROC[,2], Test$Reverse) #ROC prediction fn
perf <- performance(pred, "tpr", "fpr")
plot(perf) #ROC Curve

auc <- as.numeric(performance(pred, "auc")@y.values) #AUC value of the curve


#CART with minbucket 5
SteventsTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 5) 

prp(SteventsTree2) #Plot the tree

SteventsTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 100) 
prp(SteventsTree3) #Plot the tree



#RANDOM FORESTS

##Works building a large number of CART trees

library(randomForest)

#To create a classification model convert the dependent variable to a factor
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)

StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest <- predict(StevensForest, newdata = Test)

#Confusion matrix
table(Test$Reverse, PredictForest)

(40+74)/(40+37+19+74) #Overall accuracy


#Cross validation for CART model
library(caret)
library(e1071)


#Number of folds required

numFolds <- trainControl(method = "cv", number = 10) #10 folds

cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01)) #cp parameters to test from 0.01 to 0.5 in increments of 0.01

#Cross validation using train fn
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

#CART Model with cp value obtained above
SteventsTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp = 0.19)

#Our predictions
PredictCV <- predict(SteventsTreeCV, newdata = Test, type = "class")

table(Test$Reverse, PredictCV)

(59+64)/(59+18+29+64) #Overall Accuracy

prp(SteventsTreeCV) #plot of the tree


#D2HAWKEYE STORY

claimsdata <- read.csv("./data/ClaimsData.csv")
str(claimsdata)
