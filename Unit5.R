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

