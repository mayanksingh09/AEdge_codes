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



#Assignment 1 - TOP 10

songs <- read.csv("./data/songs.csv")

nrow(subset(songs, year == 2010)) #Ans 1.1

nrow(subset(songs, artistname == "Michael Jackson")) #Ans 1.2


MJ <- subset(songs, artistname == "Michael Jackson")

subset(MJ, Top10 == 1)$songtitle #Ans 1.3

unique(songs$timesignature) #Ans 1.4

table(songs$timesignature)

songs$songtitle[which.max(songs$tempo)] #Ans 1.5


SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)

nrow(SongsTrain)

nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")

##removing some columns from data set
SongsTrain = SongsTrain[, !(names(SongsTrain) %in% nonvars)]

SongsTest = SongsTest[, !(names(SongsTest) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1) #Ans 2.2

cor(SongsTrain$loudness, SongsTrain$energy) #Ans 3.1

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial) #create model 2 with all variables except loudness
summary(SongsLog2) #Ans 3.2


SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predictTest <- predict(SongsLog3, type = "response", newdata = SongsTest)

table(SongsTest$Top10, predictTest > 0.45)
(309 + 19)/(309+5+40+19) #Ans 4.1

(309 + 5)/(309+5+40+19)#Ans 4.2

19/(19+40) #Ans 4.4 Sensitivity

309/(309+5)


##Assignment 2 - Parole Violations

parole <- read.csv("./data/parole.csv")

nrow(parole) #Ans 1.1

nrow(subset(parole, violator==1)) #Ans 1.2

str(parole)

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)


parolemod1 <- glm(violator~., data = parole, family = binomial)

summary(parolemod1)
exp(1.61) #Ans 4.2

e <- exp(-4.05 + 0.27 + 0.75 + (0.00655*50) - (0.076*3) + (0.053*12) + (0.33*1))

e/(e+1)

predictTest <- predict(parolemod1, newdata = test, type = "response")

max(predictTest)

table(test$violator, predictTest > 0.5)
12/(12+11)

169/(169+10)

(169+12)/(169+10+11+12)

(169+10)/(169+10+11+12)

library(ROCR)

ROCRpredTest = prediction(predictTest, test$violator)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values) #Ans 5.6


#Assignment 3 Loan Repayment

loans <- read.csv("./data/loans.csv")

prop.table(table(loans$not.fully.paid)) #Ans 1.1

summary(loans) #Ans 1.2

loans_imputed <- read.csv("./data/loans_imputed.csv")

##Missing value imputation
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

##Using the imputed dataset instead
loans <- loans_imputed

library(caTools)
set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train <- subset(loans, split == TRUE)
test <- subset(loans, split == FALSE)


loansmod1 <- glm(not.fully.paid ~., data = train, family = binomial)
summary(loansmod1) #Ans 2.1

test$predicted.risk <- predict(loansmod1, type = "response", newdata = test)

table(test$not.fully.paid, test$predicted.risk > 0.5)

(2400+3)/(2400+13+457+3) #Ans 2.3

prop.table(table(test$not.fully.paid))


library(ROCR)
ROCRpredTest <- prediction(test$predicted.risk, test$not.fully.paid)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values) #Ans 2.4


loansmod2 <- glm(not.fully.paid ~ int.rate, family = binomial, data = train)
summary(loansmod2)

predicted.risk2 <- predict(loansmod2, type = "respons", newdata = test)

max(predicted.risk2) #Ans 3.2

ROCRpredTest2 <- prediction(predicted.risk2, test$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest2, "auc")@y.values)


test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
max(test$profit) #Ans 5.1

highInterest <- subset(test[test$int.rate >= 0.15, ])

mean(highInterest$profit) #Ans 6.1

prop.table(table(highInterest$not.fully.paid))

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoan <- subset(highInterest, predicted.risk <= cutoff)
nrow(selectedLoan)

sum(selectedLoan$profit) #Ans 6.2

table(selectedLoan$not.fully.paid)
