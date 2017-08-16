## Final Exam

## PARK VISITS ##

visits <- read.csv("./data/park_visits.csv")

visitsjul2016 <- subset(visits, (visits$Year == 2016 & visits$Month == 7))


#### Ans 1
which.max(table(visitsjul2016$ParkType)) 

visitsjul2016$ParkName[which.max(visitsjul2016$logVisits)]

#### Ans 2

which.max(tapply(visitsjul2016$logVisits, visitsjul2016$Region,mean))
which.min(tapply(visitsjul2016$logVisits, visitsjul2016$Region,mean))


#### Ans 3

cor(visitsjul2016$cost, visitsjul2016$logVisits)


#### Ans 4

ys <- subset(visits, visits$ParkName == "Yellowstone NP")

ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)


#### Ans 5
colSums(is.na(visits))

visits = visits[rowSums(is.na(visits)) == 0, ]
nrow(visits)

#### Ans 6

visits$Month = as.factor(visits$Month)

train <- subset(visits, visits$Year <= 2014)
test <- subset(visits, visits$Year >= 2015)


# Linear model 1
mod <- lm(logVisits ~ laglogVisits, data = train)
summary(mod)
predictions <- predict(mod, newdata = test)
SSE <- sum((test$logVisits - predictions)^2)
SST <- sum((test$logVisits - mean(visits$logVisits))^2)
1 - SSE/SST #R2 on test set

# Linear model 2
mod2 <- lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = train)
summary(mod2)
predictions2 <- predict(mod2, newdata = test)
SSE2 <- sum((test$logVisits - predictions2)^2)
SST2 <- sum((test$logVisits - mean(visits$logVisits))^2)
1 - SSE2/SST2 #R2 on test set

# Regression Trees
library(rpart)
library(rpart.plot)
treemod <- rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = train, cp = 0.05)

prp(treemod)

treepreds <- predict(treemod, newdata = test)
SSE3 <- sum((test$logVisits - treepreds)^2)
SST3 <- sum((test$logVisits - mean(visits$logVisits))^2)
1 - SSE3/SST3 #R2 on test set

# CART with Cross validation
library(caret)
library(e1071)

set.seed(201)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.0001, 0.0005, 0.0001))
train(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
1e-04

treemod2 <- rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = train, cp = 1e-04)
prp(treemod2)

treepreds2 <- predict(treemod2, newdata = test)
SSE4 <- sum((test$logVisits - treepreds2)^2)
SST4 <- sum((test$logVisits - mean(visits$logVisits))^2)
1 - SSE4/SST4 #R2 on test set


# Random forest model
library(randomForest)
set.seed(201)
RFmod <- randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = train)

RFpred <- predict(RFmod, newdata = test)
SSE5 <- sum((test$logVisits - RFpred)^2)
SST5 <- sum((test$logVisits - mean(visits$logVisits))^2)
1 - SSE5/SST5 #R2 on test set


## BANK TELEMARKETING SUCCESS ##

bank <- read.csv("./data/bank.csv")
mean(bank$age)

sort(tapply(bank$duration, bank$job, mean))

df <- data.frame(bank[c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])
cor(df)


set.seed(201)
library(caTools)
spl = sample.split(bank$y, 0.7)

training <- subset(bank, spl == T)
testing <- subset(bank, spl == F)

logmod <- glm(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data = training, family = binomial)
summary(logmod)

predicted <- predict(logmod, newdata = testing, type = "response")

table(testing$y, predicted >= 0.5)
(1273 + 44)/nrow(testing)
50 + 133
table(predicted >= 0.5)
table(testing$y)

ROCRpredTest = prediction(predicted, testing$y)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

ROCRperf <- performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf, colorize = T, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2, 1.7)) #ROC curve color coded by threshold values

library(caret)
set.seed(201)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.001, 0.05, 0.001))
train(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

treemodel <- rpart(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data = training, cp = 0.016)
prp(treemodel)

pred <- predict(treemodel, newdata = testing, type = "class")
table(testing$y, pred)
(1293 + 37)/nrow(testing)
