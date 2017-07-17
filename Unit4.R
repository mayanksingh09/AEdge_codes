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




# ASSIGNMENT 1 - VOTING DATA

## Loading data
gerber <- read.csv('./data/gerber.csv')

table(gerber$voting)/nrow(gerber) #Ans 1.1

tapply(gerber$voting, gerber$civicduty, mean) #Ans 1.2
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$control, mean)


votinglog <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)

summary(votinglog) #Ans 1.3

votpred <- predict(votinglog, type = "response")

table(gerber$voting, votpred > 0.3)

(134513 + 51966)/(134513 + 100875 + 56730 + 51966) #Ans 1.4

table(gerber$voting, votpred > 0.5) 

235388/(235388+ 108696) #Ans 1.5

table(gerber$voting)

library(ROCR)
ROCRpred <- prediction(votpred, gerber$voting)

auc <- as.numeric(performance(ROCRpred, "auc")@y.values) #auc

library(rpart)
library(rpart.plot)
votingtree <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)

prp(votingtree) #Tree - None of the variables make a big enough impact for it to be split

votingtree2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)

prp(votingtree2)

votingtree3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp = 0.0)

prp(votingtree3) #Ans 2.4


## Ans 3
votingtree4 <- rpart(voting ~ control, data = gerber, cp = 0.0)
prp(votingtree4, digits = 6)

votingtree5 <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(votingtree5, digits = 6)

0.334176 - 0.290456

0.345818 - 0.302795 #Almost same


votinglog2 <- glm(voting ~ sex + control, data = gerber, family = binomial)
summary(votinglog2) #Women less likely to vote, negative coeff means larger values are predicted 0

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(votinglog2, newdata=Possibilities, type="response")

0.290456 - 0.2908065 #Ans 3.4

#Combination of Sex and Control terms

votinglog3 <- glm(voting ~ sex + control + sex:control, data = gerber, family = binomial)
summary(votinglog3) #Ans 3.5

predict(votinglog3, newdata=Possibilities, type="response")

(0.2904558 - 0.290456)*100000


#ASSIGNMENT 2 - LETTERS

letters <- read.csv("./data/letters_ABPR.csv")

letters$isB = as.factor(letters$letter == "B")


set.seed(1000)
library(caTools)
library(rpart)

split <- sample.split(letters$isB, SplitRatio = 0.5)

train <- subset(letters, split == T)
test <- subset(letters, split == F)


table(test$isB)/nrow(test) #Ans 1.1

CARTb <- rpart(isB ~. -letter, data = train, method = "class")
prp(CARTb)

Predlet <- predict(CARTb, newdata = test, type = "class")

table(test$isB, Predlet)

(1118 + 340)/(1118+57+43+340) #Ans 1.2


##Random forest model

library(randomForest)

set.seed(1000)
RandomForestb <- randomForest(isB ~. -letter, data = train)
PredRFlet <- predict(RandomForestb, newdata = test)

table(test$isB, PredRFlet)
(1165 + 374)/nrow(test) #Ans 1.3

letters$letter = as.factor( letters$letter )

set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, split == T)
test <- subset(letters, split == F)

table(test$letter)

401/nrow(test) #Ans 2.1

##Second CART Model
CARTlet <- rpart(letter ~. -isB, data = train, method = "class")
prp(CARTlet)

Predlet2 <- predict(CARTlet, newdata = test, type = "class")
table(test$letter, Predlet2)

(348 + 318 + 363 + 340)/nrow(test) #Ans 2.2


##Second RF Model

set.seed(1000)
RandomForestlet <- randomForest(letter ~. -isB, data = train)
PredRFlet2 <- predict(RandomForestlet, newdata = test)

table(test$letter, PredRFlet2)

(390 + 380 + 393 + 364)/nrow(test) #Ans 2.3


#ASSIGNMENT 3 - EARNINGS

census <- read.csv('./data/census.csv')

set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == T)
test <- subset(census, split == F)

earningslog <- glm(over50k ~. , data = train, family = binomial)
summary(earningslog)

Predearning <- predict(earningslog, newdata = test, type = "response")

table(test$over50k, Predearning > 0.5)
(9051+1888)/nrow(test)

table(test$over50k) #Ans 1.2
9713/nrow(test)


##AUC
ROCRpred <- prediction(Predearning, test$over50k)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values) #auc


##CART Model Earnings

earningsCART <- rpart(over50k ~. , data = train, method = "class")
prp(earningsCART)

PredearningCART <- predict(earningsCART, newdata = test, type = "class")
table(test$over50k, PredearningCART)

(9243 + 1596)/nrow(test) #Ans 2.4

PredearningCARTprob <- predict(earningsCART, newdata = test, type = "prob")

ROCRpred <- prediction(PredearningCARTprob[,2], test$over50k)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf) #Ans 2.5 The probabilities from the CART model take only a handful of values
auc <- as.numeric(performance(ROCRpred, "auc")@y.values) # Ans 2.6

## Down sampling the training set
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
earningRF <- randomForest(over50k ~., data = trainSmall)
PredearningRF <- predict(earningRF, newdata = test, type = "prob")

table(test$over50k, PredearningRF[,2] > 0.5)
(9586 + 1086)/nrow(test) #Ans 3.1

## Variable used maximum number of times in the RF model
vu = varUsed(earningRF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(earningRF$forest$xlevels[vusorted$ix])) #Ans 3.2

varImpPlot(earningRF)


## CART behavior
library(caret)
library(e1071)


### cp value by cross validation
set.seed(2)
numFolds <- trainControl(method = "cv", number = 10) #10 folds
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(over50k ~. , data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

earningsCART2 <- rpart(over50k ~. , data = train, method = "class", cp = 0.002)
predCART2 <- predict(earningsCART2, newdata = test, type = "class")

table(test$over50k, predCART2)
(9178 + 1838)/nrow(test) #Ans 4.2

library(rpart.plot)
prp(earningsCART2) #Ans 4.3 - 18


#ASSIGNMENT 4 - STATE DATA


data(state)
statedata <- data.frame(state.x77)
str(statedata)


## First LM
statelin <- lm(Life.Exp ~. ,data = statedata)
summary(statelin) #Ans 1.1

SSE <- sum(statelin$residuals^2) #Ans 1.2


## Second LM
statelin2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(statelin2) #Ans 1.3

SSE <- sum(statelin2$residuals^2)


## CART Model

stateCART <- rpart(Life.Exp ~. , data = statedata)
prp(stateCART)

predCART <- predict(stateCART)

SSE <- sum((statedata$Life.Exp - predCART)^2)


## CART Model2 

stateCART2 <- rpart(Life.Exp ~. , data = statedata, minbucket = 5)
prp(stateCART2)

predCART2 <- predict(stateCART2)

SSE <- sum((statedata$Life.Exp - predCART2)^2)


## CART Model3
stateCART3 <- rpart(Life.Exp ~ Area , data = statedata, minbucket = 1)
prp(stateCART3)

predCART3 <- predict(stateCART3)

SSE <- sum((statedata$Life.Exp - predCART3)^2) #Ans 2.6


## Cross Validation

set.seed(111)
numFolds <- trainControl(method = "cv", number = 10) #10 folds
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Life.Exp ~. , data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid) #Ans 3.1

stateCART4 <- rpart(Life.Exp ~., data = statedata, cp = 0.12)
prp(stateCART4) #Ans 3.2
predCART4 <- predict(stateCART4)
SSE <- sum((statedata$Life.Exp - predCART4)^2) #Ans 3.3

#Ans 3.4 - Model with the best cp

set.seed(111)
train(Life.Exp ~ Area , data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

stateCART5 <- rpart(Life.Exp ~ Area, data = statedata, cp = 0.02)
prp(stateCART5, digits = 6) #Ans 3.6
predCART5 <- predict(stateCART5)

SSE <- sum((statedata$Life.Exp - predCART5)^2) #Ans 3.7 Area variable not as predictive as murder rate
