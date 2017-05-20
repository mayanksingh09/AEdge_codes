"UNIT 3"

#Model for Healthcare Quality

quality <- read.csv("./data/quality.csv")
str(quality)

table(quality$PoorCare) #checking how many patients got good care/poor care

#Baseline use the most frequent outcome(mode) - in classification problems
##in this case baseline is that all patients recieved good care, as its more frequent

#accuracy for baseline model
98/131 #approx 78%


#randomly split in to train and test set

library(caTools)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75) #split the quality data in the % as specified, randomly (75% in training set)

qualitytrain <- subset(quality, split == TRUE)
qualitytest <- subset(quality, split == FALSE)

nrow(qualitytrain)

#logistic regression model
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualitytrain, family = binomial)

summary(QualityLog) #look at the coefficients, Significance codes and AIC Values

predictTrain <- predict(QualityLog, type = "response") #response to get answer in terms of probability
summary(predictTrain)

tapply(predictTrain, qualitytrain$PoorCare, mean) #average prediction across the two outcomes

###Question

QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualitytrain, family = binomial)
summary(QualityLog2)

###

#Classification/Confusion tables

#Threshold 0.5

table(qualitytrain$PoorCare, predictTrain > 0.5)

10/25 #Sensitivity/TPR

70/74 #Specificity/TNR

#Threshold 0.7

table(qualitytrain$PoorCare, predictTrain > 0.7)

8/25 #Sensitivity went down

73/74 #Specificity went up

table(qualitytrain$PoorCare, predictTrain > 0.2)

16/25 #Sensitivity went up

54/74 #Specificity went down


#Receiver Operator Characteristic (ROC) Curve

library(ROCR)

ROCRpred <- prediction(predictTrain, qualitytrain$PoorCare) #prediction fn from ROCR package

#performance fn defines what to plot on X & Y axes of ROC curve

ROCRperf <- performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf, colorize = T, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2, 1.7)) #ROC curve color coded by threshold values


#Using test data
predictTest <- predict(QualityLog, type = "response", newdata = qualitytest)


#Threshold 0.3
table(qualitytest$PoorCare, predictTest > 0.3)

(19 + 6)/32 #Overall accuracy of model

5/(19+5) #FPR

2/8 #FNR

6/8 #Sensitivity/TPR

19/24 #Specificity/TNR


###Question

ROCRpredTest = prediction(predictTest, qualitytest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

###


#FRAMINGHAM HEART STUDY

framingham <- read.csv("./data/framingham.csv")
str(framingham)

library(caTools)

set.seed(1000)

split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == F)

framinghamlog <- glm(TenYearCHD ~ ., data = train, family = binomial)

summary(framinghamlog)

predictTest <- predict(framinghamlog, type = "response", newdata = test)

#Confusion matrix

#Threshold 0.5
table(test$TenYearCHD, predictTest > 0.5)

(1069 + 11)/(1069 + 187 + 11 + 6) #Overall accuracy

(1069 + 6)/(1069 + 6 + 187 + 11) #Baseline model accuracy

#Out of sample AUC

library(ROCR)

ROCRpred <- prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values) #AUC value

11/(187+11) #Sensitivity

1069/(1069+6) #Specificity


#ELECTION PREDICTION

polling <- read.csv("./data/PollingData.csv")
str(polling)

table(polling$Year) #check for missing data

summary(polling) #Missing values

#Multiple Imputation

library(mice)

#Limit the dataframe to only 4 polling related variables before performing Multiple Imputation

simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]

summary(simple)

set.seed(144)
imputed <- complete(mice(simple))

polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA

summary(imputed)

polling <- read.csv("./data/PollingData_Imputed.csv") #the imputed values weren't same so used this instead

summary(polling)

#Splitting into training and testing set
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)

table(Train$Republican)

#computing smart baseline model using sign

table(sign(Train$Rasmussen))

table(Train$Republican, sign(Train$Rasmussen)) #Comparing baseline model with actual results


#Addressing Multicollinearity

cor(polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])

#Build model with just one variable
#Add variable most highly correlated with the outcome, republican

mod1 <- glm(Republican ~ PropR, data = Train, family = binomial)
summary(mod1)

pred1 <- predict(mod1, type = "response")

#Confusion matrix
table(Train$Republican, pred1 >= 0.5)


#Add the two least correlated variables together
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = binomial)
summary(mod2)

pred2 <- predict(mod2, type = "response")

table(Train$Republican, pred2 >= 0.5)


#Evaluate model on test set

#use the smart baseline model first

table(Test$Republican, sign(Test$Rasmussen))


#use mod2
TestPrediction <- predict(mod2, type = "response", newdata = Test)

table(Test$Republican, TestPrediction >= 0.5)

subset(Test, TestPrediction >= 0.5 & Republican == 0)
