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

