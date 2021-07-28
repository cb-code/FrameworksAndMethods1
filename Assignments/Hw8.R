[Workspace loaded from ~/.RData]

install.packages("gbm")
install.packages("randomForest")

library(gbm)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(caTools)
library(ISLR)
library(e1071)
library(randomForest)

data(OJ)

set.seed(1984)

split = sample.split(OJ$Purchase, SplitRatio = 0.7)

train = OJ[split,]
test = OJ[!split,]

nrow(train)
nrow(test)

str(train)

MM <- train[train$Purchase == "MM",]

mean(train$PriceMM)
mean(train$DiscMM)

train$WeekofPurchase
train$WeekofPurchase == 275

which(train$WeekofPurchase == 275)

week <- c(27,  66,  69, 147, 263, 315, 322, 332, 342, 379, 401 ,514 ,518 ,530, 563 ,601, 615, 639, 649, 671, 683)

which
week

week[train$Purchase=="MM"]
train$Purchase[week]

sum(train$Purchase[week]=="MM")
sum(train$Purchase[week]=="CH")

classTree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, method = 'class')
summary(classTree1)

pred = predict(classTree1, newdata = test)

ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)
as.numeric(performance(ROCRpred, "auc")@y.values)

printcp(classTree1)

trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(.cp = seq(0.001, 0.1, 0.001))

cvModel = train(Purchase~., data = train, method = "rpart", trControl=trControl,tuneGrid = tuneGrid)
cvModel$bestTune

treeCV = rpart(Purchase~., data = train, control = rpart.control(cp = cvModel$bestTune))

rmseCV = sqrt(mean((predTreeCV - test$Purchase)^2))
rmseCV

predTreeCV = predict(treeCV, newdata = test, type = "class")
ct = table(test$Purchase, predTreeCV)
ct

ct[1,1]+ct[2,2]/nrow(test)
(ct[1,1]+ct[2,2])/nrow(test)

treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, method = "class", control = rpart.control(cp = trainCV$bestTune))

trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(.cp = seq(0,0.1,0.001))

trainCV = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, method = "rpart", trControl = trControl, tuneGrid = tuneGrid)

head(trainCV$results)
plot(trainCV)

trainCV$bestTune

treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, method = "class", control = rpart.control(cp = trainCV$bestTune))

predCV = predict(treeCV, newdata=test, type = "class")

ct = table(test$Purchase, predCV)
ct

(ct[1,1]+ct[2,2])/nrow(test)

predCV = predict(treeCV, newdata = test)

ROCRpredCV = prediction(predCV[,2], test$Purchase)
ROCRpredCV

ROCRperfCV = performance(ROCRpredCV, "tpr", "fpr")
plot(ROCRperfCV)
as.numeric(performance(ROCRpredCV, "auc")@y.values)

treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, method = "class", control = rpart.control(cp = trainCV$bestTune))

predCV = predict(treeCV, newdata=test, type = "class")

ROCRperfCV = performance(ROCRpredCV, "tpr", "fpr")
trainCV$bestTune

treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, method = 'class', control = rpart.control(cp = trainCV$bestTune))

predCV = predict(treeCV, newdata=test, type = "class")

classTree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, method = 'class', control = rpart.control(cp = trainCV$bestTune))

pred = predict(classTree1, newdata = test)

ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)

as.numeric(performance(ROCRpred, "auc")@y.values)

classTree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, method = 'class', control = rpart.control(cp = 0.005))

pred = predict(classTree1, newdata = test)
ROCRpred = prediction(pred[,2],test$Purchase)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
 
plot(ROCRperf)

as.numeric(performance(ROCRpred, "auc")@y.values)

bag = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, mtry = ncol(train)-1, ntree = 1000)

predBag = predict(bag, newdata = test, type = "prob")
 
treetest = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, cp = 0.005)

rpart.plot(treetest)

predtest = predict(treetest, newdata = test, type = 'prob')

ROCRpredtest = prediction(predtest[,2], test$Purchase)
ROCRperftest = performance(ROCRpredtest, "tpr","fpr")

plot(ROCRperftest)
as.numeric(performance(ROCRpredtest,"auc")@y.values)

set.seed(617)

plot(bag)

ROCRpredbag = prediction(predBag[,2], test$Purchase)
ROCRperfbag = performance(ROCRpredbag, "tpr", "fpr")

plot(ROCRperfbag)

as.numeric(performance(ROCRpredbag, "auc")@y.values)
round(as.numeric(performance(ROCRpredbag, "auc")@y.values), 2)

forest = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, ntree = 1000)

predForest = predict(forest, newdata = test, type = "prob")

ROCRpredFST = prediction(predForest[,2], test$Purchase)
ROCRperfFST = performance(ROCRpredFST, "tpr", "fpr")

plot(ROCRperfFST)

as.numeric(performance(ROCRpredFST, "auc")@y.values)
round(as.numeric(performance(ROCRpredFST, "auc")@y.values), 2)

train$Purchase2 = as.numeric(train$Purchase)-1
test$Purchase2 = as.numeric(test$Purchase)-1

set.seed(617)

update.packages("gbm")

boost = gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH, data = train, distribution  = "bernoulli", n.trees = 1000, interaction.depth = 1, shrinkage = 0.04)

predBoostTrain = predict(boost, type = "response", n.trees = 100)

predBoostTrain = predict(boost, newdata = test, type = "response", n.trees = 100)
predBoostTrain = predict(boost,type = "response", n.trees = 100)

predBoostTest = predict(boost,newdata = test, type = "response", n.trees = 100)

ROCRPredBT = prediction(predBoostTest, test$Purchase)
ROCRPerfBT = performance(ROCRPredBT, "tpr", "fpr")

as.numeric(performance(ROCRPredBT, "auc")@y.values)
round(as.numeric(performance(ROCRPredBT, "auc")@y.values), 2)
