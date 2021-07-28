setwd("~/Documents/Rcode")

eBay <- read.csv('eBayAssignment.csv')

install.packages('ROCR')   # if you have not installed ROCR, be sure to install it first.

library(ROCR)
library(caTools)

set.seed(196)

split = sample.split(eBay$sold, SplitRatio = 0.8)

train = eBay[split,]
test = eBay[!split,]

nrow(train)
head(train, 3)

soldTrain <- train[train$sold == 1,]

head(soldTrain, 3)
median(soldTrain$startprice)
levels(train$productline)

unsoldTrain <- train[train$sold == 0,]

head(unsoldTrain, 3)
median(unsoldTrain$startprice)

model1 = glm(sold~biddable+startprice+condition+cellular+carrier+color+storage+productline+noDescription+charCountDescription+upperCaseDescription+startprice_99end, family = "binomial", data = train)

summary(model1)
summary(model1)$aic

model2 <- glm(sold ~ biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end, family = 'binomial', data = train)

summary(model2)
summary(model2)$aic

levels(train$productline)

predict(model2, newdata = data.frame(productline = 1), type = 'response')

levels(train$biddable)
levels(train$condition)
levels(train$storage)
levels(train$upperCaseDescription)

mean(train$upperCaseDescription)
mean(train$startprice)

levels(train$startprice_99end)
mean(train$startprice_99end)
average(train$startprice_99end)
mean(levels(train$startprice_99end))

predict(model2, newdata = data.frame(biddable = 1, condition = 1, storage = 1, upperCaseDescription = 2.532572, startprice = 209.2366, startprice_99end = 1, productline = 1), type = 'response')
predict(model2, newdata = data.frame(biddable = 1, condition = 1, storage = 1, upperCaseDescription == 2.532572, startprice == 209.2366, startprice_99end = 1, productline = 1), type = 'response')

predict(model2, newdata = data.frame(biddable = 1, condition = 1, storage = 1, startprice == 209.2366, startprice_99end = 1, productline = 1), type = 'response')
predict(model2, newdata = data.frame(biddable = 1, condition = 1, storage = 1, upperCaseDescription = mean(train$upperCaseDescription), startprice = mean(train$startprice), startprice_99end = 1, productline = 1), type = 'response')

str(train)

levels(train$biddable)
train$biddable(level = 1)

levels.default(train$biddable)
levels.default(train)
levels.default(train$condition)

summary(model2)
coefficients(model2)

levels(train$productline)
coef(train$productline)
summary(train$productline)

summary(model2)$coef[8]
summary(model2)$coef[7]
summary(model2)$coef[9]

pred = predict(model2, type = 'response')

exp(summary(model2)$coef[1])
exp(summary(model2)$coef[2])
exp(summary(model2)$coef[3])
exp(summary(model2)$coef[4])
exp(summary(model2)$coef[5])
exp(summary(model2)$coef[6])
exp(summary(model2)$coef[7])
exp(summary(model2)$coef[8])
exp(summary(model2)$coef[9])
exp(summary(model2)$coef[10])
exp(summary(model2)$coef[11])
exp(summary(model2)$coef[12])
exp(summary(model2)$coef[13])

summary(model2)$coef[10]
100*(exp(summary(model2)$coef[10])-1)

model3 = glm(sold~productline, family = 'binomial', data = train)

summary(model3)
summary(model3)$coef[3]
exp(summary(model3)$coef[3])
100*(exp(summary(model3)$coef[3])-1)

model4 = glm(sold~startprice, family = 'binomial', data = train)

summary(model4)
100*(exp(summary(model4)$coef[2])-1)

summary(model3)

exp(summary(model3)$coef[5])
summary(model3)$coef[5]
100*(exp(summary(model3)$coef[5])-1)

coef(model3)
coef(model2)

summary(model2)$coef[12]
exp(summary(model2)$coef[12])
100*(exp(summary(model2)$coef[12])-1)

summary(model2)$coef[10]
exp(summary(model2)$coef[10])
100*(exp(summary(model2)$coef[10])-1)

100*(exp(summary(model2)$coef[3])-1)

model_productline <- model3
coef(model3)

AICmodel = glm(sold ~biddable+startprice+condition+cellular+carrier+color+storage+productline+noDescription+charCountDescription+upperCaseDescription+startprice_99end, family = "binomial", data = train)

summary(AICmodel)
summary(AICmodel)$aic

pred <- test
pred[test$UniqueID==10940]
pred <- pred[test$UniqueID==10940]

View(pred)

str(test)
which(test$UniqueID==10940)

predict(model2, newdata = test[184,], type = 'response')

test[184,]

pred = predict(model2, newdata = test[184,], type = 'response')
pred = predict(model2, type = 'response')

data.frame(sold = test$sold[1:10], predicted_probability = pred[1:10])
data.frame(sold = test$sold[1:10], predicted_probability = pred[1:10], prediction_binary = as.integer(pred[1:10]>0.5))

ct = table(sold = test$sold, predictions = as.numeric(pred>0.5))

pred = predict(model2, newdata =test, type = 'response')

ct = table(sold = test$sold, predictions = as.numeric(pred>0.5))
ct

ct = table(sold = test$sold, predictions = as.integer(pred>0.5))
ct

accuracy = sum(ct[1,1], ct[2,2])/nrow(test)
accuracy

ct2 = table(sold = test$sold, predictions = as.integer(pred<0.5))
ct2

accuracy2 = sum(ct2[1,1], ct2[2,2])/nrow(test)
accuracy2

ROCRpred = prediction(pred,test$sold)

as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot

ROCRperf = performance(ROCRpred,"tpr","fpr")

plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve

ROCRpred = prediction(pred,test$sold)

as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

View(model4)
View(ROCRperf)

sum(test$sold==1)
sum(test$sold==0)
