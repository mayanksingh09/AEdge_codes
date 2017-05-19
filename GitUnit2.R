#Predicting the quality of wine using linear regression

#Predicts an outcome variable or dependent variable
#Predicts using a set of independent variables

#Dependent variable <- Typical price in 1990-91 wine auction

#Indepent variable <- Age, Weather (Avg Growing season temp, Harvest Rain, Winter Rain)

#One variable linear regression
## Uses one independent variable to predict the dependent variable


#Multiple Linear Regression
## Uses more than one independent variable to predict the dependent variable

setwd("/home/fractaluser/Documents/edX/Analytics Edge")

wine <- read.csv("./data/wine.csv")
str(wine)
summary(wine)


#one variable LR equation using AGST
model1 <- lm(Price ~ AGST, data = wine)

summary(model1)
#Residuals = Error terms
#Coeff 1st row intercept term, 2nd row independent variable
#Estimate = Beta0 (for Intercept row), Beta1 (for indpt row)
#Multiple R-squared = Normal R2 value
#Adjusted R-squared = R2 adjusted for the no. of variables used for the number of data points


model1$residuals #values of all the residuals
SSE <- sum(model1$residuals^2)


#adding Harvest Rain to model
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)

summary(model2)

SSE <- sum(model2$residuals^2) #much less than model1

#LR Model with all independent variables
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

summary(model3)
SSE <- sum(model3$residuals^2)

#Quick Question
q1 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(q1)

#Understanding the model

#Std. Error = How much the coefficient is likely to vary from the estimate value
#t-value = Estimate/(Std. Error)
#Pr(>|t|) -> lower the value for this term, lower the probability that the coefficient for the indpt variable is zero. small if abs(t-value) is large and vice versa.

#use the start system '***' & '**' & '*' signific

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4) #adjusted R2 increased


#Correlation
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine) #correlation b/w all the variables

#Multicollinearity

model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5) #R2 and adjusted R2 decreased

##Age chosen instead of Francepop because Age makes more intuitive sense in the model


#Making predictions
wineTest <- read.csv('./data/wine_test.csv')

str(wineTest)

##to make predictions
predictTest <- predict(model4, newdata = wineTest)

##R2 for the test set
SSE <- sum((wineTest$Price - predictTest)^2)

SST <- sum((wineTest$Price - mean(wine$Price))^2)

1 - SSE/SST #R2 on test set


#MONEYBALL
setwd("/home/fractaluser/Documents/edX/Analytics Edge")

baseball <- read.csv("./data/baseball.csv")
str(baseball)

moneyball <- subset(baseball, Year < 2002)
str(moneyball)

## predicting wins using LR b/w Wins and (difference of runs scored and runs allowed)

moneyball$RD <- moneyball$RS - moneyball$RA

plot(moneyball$RD, moneyball$W)

winsReg <- lm(W ~ RD, data = moneyball)
summary(winsReg)

## predicting Runs scored using On base percentage, Slugging percentage and Batting average

RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg) #batting average has negative coefficient because of multi-collinearity (BA vs OBP)

## predicting Runs scored using On base percentage, Slugging percentage
RunsReg <- lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg) #similar R2 all variables significant, Similar scale so OBP more imp than SLG (both important)


## predicting Runs allowed using Opponents On base percentage, Opponents Slugging percentage
RunsAllowedReg <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllowedReg)


## estimate team statistics for 2002 using player stats for 2001

#check slides

df<- data.frame()
OBP <- list(0.338, 0.391, 0.369, 0.313, 0.361)
SLG <- list(0.540, 0.450, 0.374, 0.447, 0.500)


RS <- list()

for (i in 1:5){
  RS[i] = -804.63 + 2737.77*as.numeric(OBP[i]) + 1584.91*as.numeric(SLG[i])
}

## question
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, wins2012)

## NBA Data

NBA <- read.csv("./data/NBA_train.csv")
str(NBA)

table(NBA$W, NBA$Playoffs)

## predict wins with difference in PS and PA

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)

winsReg <- lm(W ~ PTSdiff, data = NBA)
summary(winsReg)

## predict Points scored

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)
PointsReg$residuals
SSE <- sum(PointsReg$residuals^2) #finding sum of squared errors
RMSE <- sqrt(SSE/nrow(NBA)) #root mean square error

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA) #removed the one with the highest p-value
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)

PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE_4 <- sum(PointsReg4$residuals^2) #finding sum of squared errors
RMSE_4 <- sqrt(SSE_4/nrow(NBA)) #root mean square error is approximately the same

NBA_test <- read.csv("./data/NBA_test.csv")

PointsPredictions <- predict(PointsReg4, newdata = NBA_test)

## Use out of sample R2 to predict how well the model fares on the test data
## the earlier R2 value was the in sample R2 (how well the model fits the training data)

SSE <- sum((PointsPredictions - NBA_test$PTS)^2)
SST <- sum((mean(NBA$PTS)- NBA_test$PTS)^2)

R2 <- 1 - SSE/SST

RMSE <- sqrt(SSE/nrow(NBA_test))


# ASSIGNMENT 2

## CLIMATE CHANGE
climate_change <- read.csv("./data/climate_change.csv")
str(climate_change)

climate_change_train <- subset(climate_change, Year < 2007)
climate_change_test <- subset(climate_change, Year > 2006)

climateReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_change_train)
summary(climateReg)

cor(climate_change_train)

climateReg2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climate_change_train)
summary(climateReg2)

## using AIC to find out which model best suits the data, use step() in R for this

climateRegAIC <- step(climateReg)
summary(climateRegAIC)

climate_predict <- predict(climateRegAIC, newdata = climate_change_test)
SSE <- sum((climate_predict - climate_change_test$Temp)^2)
SST <- sum((mean(climate_change_train$Temp) - climate_change_test$Temp)^2)

R2 <- 1 - SSE/SST


## READING TEST SCORE

setwd("/home/fractaluser/Documents/edX/Analytics Edge")

pisaTrain <- read.csv("./data/pisa2009train.csv")
pisaTest <- read.csv("./data/pisa2009test.csv")


nrow(pisaTrain) #Ans 1.1

tapply(pisaTrain$readingScore,pisaTrain$male, FUN = mean) #Ans 1.2

summary(pisaTrain) #Ans 1.3

pisaTrain <- na.omit(pisaTrain)

pisaTest <- na.omit(pisaTest)

nrow(pisaTrain)
nrow(pisaTest)

pisaTrain$grade

str(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


lmScore <- lm(readingScore~., data = pisaTrain)

summary(lmScore) #Ans 3.1

sqrt(sum(lmScore$residuals^2)/(nrow(pisaTrain)-1)) 


predTest <- predict(lmScore, newdata = pisaTest)

summary(predTest) #Ans 4.1

SSE <- sum((predTest - pisaTest$readingScore)^2) #Ans 4.2

RMSE <- sqrt(SSE/nrow(pisaTest))

mean(pisaTrain$readingScore) ##Baseline model is mean #Ans 4.3

SST <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2) #Ans 4.3

R2 <- 1 - SSE/SST


## DETECTING FLU EPIDEMIC

FluTrain <- read.csv("./data/FluTrain.csv")
FluTrain[FluTrain$ILI == max(FluTrain$ILI), ] #First answer

FluTrain[FluTrain$Queries == max(FluTrain$Queries), ]

hist(FluTrain$ILI) #histogram of ILI #Ans 1.2

plot(log(FluTrain$ILI), FluTrain$Queries) #Ans 1.3 and Ans 2.1

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)

summary(FluTrend1) #Ans 2.2

log(1/cor(log(FluTrain$ILI), FluTrain$Queries))
cor(log(FluTrain$ILI), FluTrain$Queries)^2

exp(-0.5*cor(FluTrain$ILI, FluTrain$Queries))


FluTest <- read.csv("./data/FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17")

PredTest1[11] #Ans 3.1

(2.293422 - 2.187378)/2.293422 #Ans 3.2

SSE <- sum((PredTest1 - FluTest$ILI)^2)

RMSE <- sqrt(SSE/nrow(FluTest)) #Ans 3.3

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE) #Creating a 2 week lag dataset

FluTrain$ILILag2 = coredata(ILILag2) 


plot(log(FluTrain$ILILag2),log(FluTrain$ILI)) #Ans 4.2

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)

summary(FluTrend2) # Ans4.3

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE) #Creating a 2 week lag dataset
FluTest$ILILag2 = coredata(ILILag2) 

summary(FluTest$ILILag2) #Ans 5.1

FluTest$ILILag2[1] = FluTrain$ILI[416] #Ans 5.3
FluTest$ILILag2[2] = FluTrain$ILI[417] 

PredTest2 <- exp(predict(FluTrend2, newdata=FluTest))

SSE = sum((PredTest2-FluTest$ILI)^2)

RMSE <- sqrt(SSE/nrow(FluTest))


#ELANTRA SALES

elantradata <- read.csv("./data/elantra.csv")
elantradata$MonthFac <- as.factor(elantradata$Month)

elantratrain <- elantradata[elantradata$Year <= 2012,]

nrow(elantratrain) #Ans 1.1

elantratest <- elantradata[elantradata$Year > 2012,]
elantratrain$ElantraSales

elantralm1 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantratrain)

summary(elantralm1) #Ans 2.1 - 2.3

elantralm2 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantratrain)

summary(elantralm2)
110.69*4
 

elantralm3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + MonthFac, data = elantratrain)

summary(elantralm3)

cor(elantratrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")]) #Ans 5.1& 5.2

elantralm3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + MonthFac, data = elantratrain)

elantralm4 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + MonthFac, data = elantratrain)
summary(elantralm4)

elantrapred <- predict(elantralm4, newdata = elantratest)

SSE <- sum((elantrapred - elantratest$ElantraSales)^2) 

SST <- sum((mean(elantratrain$ElantraSales) - elantratest$ElantraSales)^2)

R2 <- 1 - SSE/SST #Ans 6.4

which.max(abs(elantrapred - elantratest$ElantraSales)) #Ans 6.5

elantratest$Month[5] #Ans 6.6