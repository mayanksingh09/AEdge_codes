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

table(claimsdata$bucket2009)/nrow(claimsdata) #Proportion of patients in various cost buckets

###Goal: Predict cost bucket patient falls into in 2009 using a CART model

##Splitting data
library(caTools)

set.seed(88)
spl = sample.split(claimsdata$bucket2009, SplitRatio = 0.6) 

ClaimsTrain <- subset(claimsdata, spl == T)
ClaimsTest <- subset(claimsdata, spl == F)

table(ClaimsTrain$diabetes)/nrow(ClaimsTrain) #proportion with diabetes diagnosis


## Baseline Method and Penalty Matrix

### Will predict that cost bucket for a patient in 2009 is the same as it was in 2008

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest) #Baseline accuracy

### Penalty Matrix (Actual outcomes on left, predicted on top)

PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = T, nrow = 5) #Penalty of getting the predicted outcomes wrong


as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix #multiplying classification matrix by penalty matrix

PenaltyError <- sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest) #penalty error


#122978/nrow(ClaimsTest)

#sum(as.matrix(table(ClaimsTest$bucket2009)) * PenaltyMatrix[,1])/nrow(ClaimsTest) #Penalty error


## CART Model to predict healthcost
### Multiclass classification 

library(rpart)
library(rpart.plot)

ClaimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005) #cp value determined by the cross validation as taught earlier

prp(ClaimsTree) #plot of the classification tree

PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest) #Accuracy

PenaltyError <- sum(table(ClaimsTest$bucket2009, PredictTest)*PenaltyMatrix)/nrow(ClaimsTest)


## New model taking into account the penalty for each wrong prediction (from the penalty matrix)
ClaimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix)) 

PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest) #Accuracy - Reduced a little

PenaltyError <- sum(table(ClaimsTest$bucket2009, PredictTest)*PenaltyMatrix)/nrow(ClaimsTest) #Reduced Penalty Error




#BOSTON HOUSING DATA

##Regression trees applied to house prices and locations

boston <- read.csv("./data/boston.csv")
str(boston)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19) #plotting Census Tracts lying along the river

points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", pch = 19) #Plotting MIT

summary(boston$NOX) #Summary of Nitrous Oxide pollution

points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col = "green", pch = 19) #plot for places with > mean pollution

plot(boston$LON, boston$LAT)
summary(boston$MEDV)

points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19) #Above average housing prices


#linear regression on the housing data
latlonlm <- lm(MEDV ~ LAT + LON, data = boston)

summary(latlonlm) #Not really a useful model

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col = "blue", pch = "$") #Linear regression model isn't doing a great job

## Using Regression Trees instead

library(rpart)
library(rpart.plot)

latlontree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree) #predicts a number instead of an outcome, in this case mean of the house price in that bucket

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

fittedvalues <- predict(latlontree)

points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col = "blue", pch = "$") #Much better than the linear regression

## New tree with lesser min bucket

latlontree <- rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree) #plotting the tree
text(latlontree) #adding the text

plot(boston$LON, boston$LAT)
abline(v = -71.07) #vertical line at -71.07
abline(h = 42.21) #horizontal line
abline(h = 42.17)

points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)


#Predicting house prices
library(caTools)

set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
train <- subset(boston, split == T)
test <- subset(boston, split == F)

#linear model
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)

summary(linreg)

linreg.pred <- predict(linreg, newdata = test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2) #Sum of Squared errors

#CART model
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)

tree.pred <- predict(tree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2) #Sum of Squared errors for CART model


#CART model using Cross validation
library(caret)
library(e1071)

## Ten fold cross validation
tr.control <- trainControl(method = "cv", number = 10)

cp.grid <- expand.grid(.cp = (0:10)*0.001)

#finding out the best value of cp using cross validation
tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

best.tree <- tr$finalModel #Best tree

prp(best.tree)

best.tree.pred <- predict(best.tree, newdata = test)

best.tree.sse <- sum((best.tree.pred - test$MEDV)^2) #SSE has reduced as compared to the earlier tree

