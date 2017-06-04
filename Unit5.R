#UNIT 5

##Natural Language Processing - How computers understand text
##Goal: To understand and derive meaning from Human language

##Sentiment mining
##Amazon Mechanical Turk



#SENTIMENT ANALYSIS

Sys.setlocale("LC_ALL", "C")

library(tm)
library(SnowballC)

tweets <- read.csv("./data/tweets.csv", stringsAsFactors = F) #always use stringsasfactors with text data

str(tweets)

tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

## Convert tweets to corpus tp process them

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content ## First tweet

## Converting all text to lowercase

corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content


## Removing punctutation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

## Removing stopwords

corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content


## Stem document
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content


## Matrix of frequency of word appearing in a tweet

frequencies <- DocumentTermMatrix(corpus)

## View matrix contents

inspect(frequencies[1000:1005, 505:515])


## Finding most popular words

findFreqTerms(frequencies, lowfreq = 20)

## Removing words that don't appear very often

sparse <- removeSparseTerms(frequencies, 0.995) #only keep terms that appear in 0.5% or more of the tweets

tweetsSparse <- as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) #to handle variables with numbers at the start of the name

tweetsSparse$Negative <- tweets$Negative

library(caTools)

set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse <- subset(tweetsSparse, split == T)
testSparse <- subset(tweetsSparse, split == F)


## Bulding a predictive model

library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~., data = trainSparse, method = "class")
prp(tweetCART)

predictCART <- predict(tweetCART, newdata = testSparse, type = "class")

table(testSparse$Negative, predictCART)

(294 + 18)/(nrow(testSparse)) #Accuracy of CART model


table(testSparse$Negative) #Baseline
300/nrow(testSparse)


## Random Forest Model

library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data = trainSparse)

predictRF <- predict(tweetRF, newdata = testSparse)

table(testSparse$Negative, predictRF)

(293 + 21)/nrow(testSparse) #Accuracy



## Logistic model

tweetLog <- glm(Negative ~ ., data = trainSparse, family = binomial)

predictLog <- predict(tweetLog, newdata = testSparse, type = "response")

table(testSparse$Negative, predictLog > 0.5)
(251 + 36)/nrow(testSparse)



#ENRON Recitation

emails <- read.csv("./data/energy_bids.csv", stringsAsFactors = F)
str(emails)

table(emails$responsive)

library(tm)

## Create corpus
corpus <- Corpus(VectorSource(emails$email))
corpus[[1]]$content


## Converting to lower
corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

## Remove punctuation

corpus <- tm_map(corpus, removePunctuation)

## Remove stop words

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]$content

## Stem document

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

## Document term Matrix

dtm <- DocumentTermMatrix(corpus)
dtm

dtm <- removeSparseTerms(dtm, 0.97) #remove terms that appear in less then 3% of the documents

labeledTerms <- as.data.frame(as.matrix(dtm))

labeledTerms$responsive <- emails$responsive
str(labeledTerms)

## Split data train-test

library(caTools)

set.seed(144)
spl <- sample.split(labeledTerms$responsive, 0.7)

train <- subset(labeledTerms, spl == T)
test <- subset(labeledTerms, spl == F)

library(rpart)
library(rpart.plot)
set.seed(144)
emailCART <- rpart(responsive ~. , data = train, method = "class")
prp(emailCART)

pred <- predict(emailCART, newdata = test)

table(test$responsive, pred[,2] >= 0.5)

(195 + 25)/nrow(test) #Accuracy CART Model

table(test$responsive)

215/nrow(test) #Baseline Accuracy

## The ROC curve

library(ROCR)

predROCR <- prediction(pred[,2], test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")

plot(perfROCR,colorize = T) #Plot ROC curve

performance(predROCR, "auc")@y.values #AUC value


## Assignment 1 - WIKIPEDIA Vandalism

wiki <- read.csv("./data/wiki.csv", stringsAsFactors = F)

wiki$Vandal <- as.factor(wiki$Vandal)

table(wiki$Vandal)


## Creating corpus

corpus <- Corpus(VectorSource(wiki$Added))
corpus[[1]]$content

## Removing stop words

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

dtmAdded <- DocumentTermMatrix(corpus)

sparseAdded <- removeSparseTerms(dtmAdded, 0.997) #Ans 1.3

wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))


## Steps repeated for words removed

corpus <- Corpus(VectorSource(wiki$Removed))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpus)

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997) #Ans 1.3

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsRemoved) #Ans 1.4

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal <- wiki$Vandal

set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)

train <- subset(wikiWords, split == T)
test <- subset(wikiWords, split == F)

table(test$Vandal)
618/nrow(test) #Ans 1.5

wikiCART <- rpart(Vandal ~. , data = train, method = "class")

predCART <- predict(wikiCART, newdata = test, type = "class")

table(test$Vandal, predCART)
(618 + 12)/nrow(test) #Real answer 1.6

prp(wikiCART) #Real answer 2 Ans1.7


## using website addresses

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)


wikiTrain2 = subset(wikiWords2, split ==TRUE)

wikiTest2 = subset(wikiWords2, split ==FALSE)

wikiCART2 <- rpart(Vandal ~. , data = wikiTrain2, method = "class")

predCART2 <- predict(wikiCART2, newdata = wikiTest2, type = "class")

table(wikiTest2$Vandal, predCART2)
(609 + 57)/nrow(wikiTest2) #Ans 2.2 (Real)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded) #Ans 2.3

set.seed(123)
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCART3 <- rpart(Vandal ~. , data = wikiTrain3, method = "class")

predCART3 <- predict(wikiCART3, newdata = wikiTest3, type = "class")

table(wikiTest3$Vandal, predCART3)

(514 + 248)/nrow(wikiTest2) #Ans 2.4

## 

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiWords3Train <- subset(wikiWords3, split == T)
wikiWords3Test <- subset(wikiWords3, split == F)

wikiCART4 <- rpart(Vandal ~. ,data = wikiWords3, method = "class")
predCART4 <- predict(wikiCART4, newdata = wikiWords3Test, type = "class")

table(wikiWords3Test$Vandal, predCART4)
(595 + 241)/nrow(wikiWords3Test) #Ans 3.1

prp(wikiCART4) #Ans 3.2


# Assignment 2 - CLINICAL TRIALS

clinic <- read.csv("./data/clinical_trial.csv", stringsAsFactors = F)

max(nchar(clinic$abstract)) #Ans 1.1

nrow(clinic[nchar(clinic$abstract) == 0,]) #Ans 1.2

clinic$title[which.min(nchar(clinic$title))] #Ans 1.3

corpusTitle <- Corpus(VectorSource(clinic$title))
corpusTitle[[1]]$content

corpusAbstract <- Corpus(VectorSource(clinic$abstract))

corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))


corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)


corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

corpusTitle[[1]]$content
corpusAbstract[[2]]$content


dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

ncol(dtmTitle) #Ans 2.1
ncol(dtmAbstract)

which.max(colSums(dtmAbstract)) #Ans 2.3

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)

dtm$trials <- clinic$trial

ncol(dtm) #Ans 3.2

library(caTools)

set.seed(144)

split <- sample.split(dtm$trials, SplitRatio = 0.7)

train <- subset(dtm, split == T)
test <- subset(dtm, split == F)

table(test$trials)

313/nrow(test) #Ans 3.3

trialCART <- rpart(trials ~. , data = train, method = "class")
prp(trialCART) #Ans 3.4

max(predict(trialCART, type = "prob")[,2]) #Ans 3.5

trainpred <- predict(trialCART, type = "prob")[,2]

table(train$trials, trainpred > 0.5)
(631 + 441)/nrow(train) #Ans 3.7

441/(441+131) #Ans 3.7

631/(631+99)



trialpred <- predict(trialCART, newdata = test, type = "prob")

table(test$trials, trialpred[,2] >= 0.5)

(261 + 162)/nrow(test) #Ans 4.1

library(ROCR)

predROCR <- prediction(pred[,2], test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")


trialROCR <- prediction(trialpred[,2], test$trials)

predROCR <- performance(trialROCR, "tpr", "fpr")

plot(predROCR,colorize = T) #Plot ROC curve

performance(trialROCR, "auc")@y.values #Ans 4.2


#Assignment 3 - SPAM EMAIL

emails <- read.csv("./data/emails.csv", stringsAsFactors = F)

nrow(emails) #Ans 1.1

table(emails$spam) #Ans 1.2

head(emails$text)

max(nchar(emails$text)) #Ans 1.5

which.min(nchar(emails$text)) #Ans 1.6

library(tm)

corpus <- Corpus(VectorSource(emails$text))

## Converting to lowercase

corpus <- tm_map(corpus, content_transformer(tolower))

## Remove punctuation

corpus <- tm_map(corpus, removePunctuation)

## Remove stopwords

corpus <- tm_map(corpus, removeWords, stopwords("english"))

## Stem words

corpus <- tm_map(corpus, stemDocument)


dtm <- DocumentTermMatrix(corpus)
ncol(dtm) #Ans 2.1

spdtm <- removeSparseTerms(dtm, 0.95)
ncol(spdtm) #Ans 2.2

emailsSparse <- as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse)) #Ans 2.3


emailsSparse$spam <- emails$spam

colnames(emailsSparse)[colSums(subset(emailsSparse, spam == 0)) >= 5000] #Ans 2.4

colnames(emailsSparse)[colSums(subset(emailsSparse, spam == 1)) >= 1000] #Ans 2.5 (Dependent variable spam not counted so 3)

emailsSparse$spam <- as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
split <- sample.split(emailsSparse, SplitRatio = 0.7)

train <- subset(emailsSparse, split == T)
test <- subset(emailsSparse, split == F)

spamLog <- glm(spam ~. , data = train, family = binomial)
predLog <- predict(spamLog)

library(rpart)
library(rpart.plot)

spamCART <- rpart(spam ~. ,data = train, method = "class")
predCART <- predict(spamCART)

library(randomForest)

set.seed(123)
spamRF <- randomForest(spam ~. , data = train)
predRF <- predict(spamRF, type = "prob")

sum(predLog < 0.00001) #Ans 3.1 (3046)

sum(predLog > 0.99999) #(954)

sum((predLog >= 0.00001 & predLog <= 0.99999)) #(10)

summary(spamLog) #Ans 3.2 (0)

prp(spamCART) #Ans 3.3

table(train$spam, predLog >= 0.5)
(2978 + 917)/nrow(train) #Ans 3.4 (0.9990025)


performance(prediction(predLog, train$spam), "auc")@y.values #Ans 3.5 (0.9999959)

predtrainCART <- predict(spamCART)

table(train$spam, predtrainCART[,2] >= 0.5)

(2900 + 894)/nrow(train) #Ans 3.6 (0.942394)

performance(prediction(predtrainCART[,2], train$spam), "auc")@y.values #Ans 3.7 (0.9696044)


predtrainRF <- predict(spamRF, type = "prob")

table(train$spam, predtrainRF[,2] >= 0.5)

(3001 + 916)/nrow(train) #Ans 3.8 (0.9793017)

performance(prediction(predtrainRF[,2], train$spam), "auc")@y.values #Ans 3.9 (0.9979116)

predtestLog <- predict(spamLog, newdata = test)

table(test$spam, predtestLog >= 0.5)

(1245 + 376)/nrow(test) #Ans 4.1 (0.9505239)

performance(prediction(predtestLog, test$spam), "auc")@y.values #Ans 4.2 (0.9627517)

## CART model testing accuracy

predtestCART <- predict(spamCART, newdata = test)

table(test$spam, predtestCART[,2] >= 0.5)
(1243 + 383)/nrow(test) #Ans 4.3 (0.9394645)

performance(prediction(predtestCART[,2], test$spam), "auc")@y.values #Ans 4.4 (0.963176)

predtestRF <- predict(spamRF, newdata = test, type = "prob")

table(test$spam, predtestRF[,2] >= 0.5)
(1302 + 394)/nrow(test) #Ans 4.5 (0.975553)

performance(prediction(predtestRF[,2], test$spam), "auc")@y.values #Ans 4.6 (0.9975656)

#ASSIGNMENT 2 - SPAM emails (Continued)

wordCount = rowSums(as.matrix(dtm))

hist(wordCount) #Ans 6.2

hist(log(wordCount)) #Ans 6.3

emailsSparse$logWordCount <- log(wordCount)

boxplot(emailsSparse$logWordCount~emailsSparse$spam) #Ans 6.4

## Splitting data

library(caTools)
set.seed(123)
split <- sample.split(emailsSparse, SplitRatio = 0.7)

train2 <- subset(emailsSparse, split == T)
test2 <- subset(emailsSparse, split == F)

spam2CART <- rpart(spam ~., data = train2, method = "class")
prp(spam2CART) #Ans 6.5

set.seed(123)
spam2RF <- randomForest(spam ~., data = train2)

predtestCART2 <- predict(spam2CART, newdata = test2)
predtestRF2 <- predict(spam2RF, newdata = test2, type = "prob")

table(test2$spam, predtestCART2[,2] >= 0.5)
(1217 + 392)/nrow(test2) #Ans 6.6 (0.9301513)

performance(prediction(predtestCART2[,2], test2$spam), "auc")@y.values #Ans 6.7 (0.9582438)

table(test2$spam, predtestRF2[,2] >= 0.5)
(1298 + 388)/nrow(test2) #Ans 6.8 (0.9772992)

performance(prediction(predtestRF2[,2], test2$spam), "auc")@y.values #Ans 6.9 (0.9980905)

### Can learn more about n-grams