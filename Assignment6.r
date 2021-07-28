setwd("~/Documents/Rcode")

house <- read.csv('houses.csv')

install.packages("car")
install.packages("leaps")
install.packages("leaps")
install.packages("lasso2")
install.packages("glmnet")
install.packages("corrplot")

library(car)
library(caret)
library(tidyr)
library(dplyr)
library(leaps)
library(lasso2)
library(glmnet)
library(ggplot2)
library(corrplot)

set.seed(1031)

split <- createDataPartition(houses$price, p = .7, groups = 100, list = F)
split <- createDataPartition(house$price, p = .7, groups = 100, list = F)

train <- house[split,]
test <- house[-split,]

mean(train$price)
str(train)

cor(train[,-16])
round(cor(train[,-16]), 2)*100

corMatrix = as.data.frame(cor(train[,-16]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>% gather(key = var2, value = r, 1:15) %>% arrange(var1, desc(var2)) %>%
  ggplot(aes(x=var1, y = reorder(var2, order(var2, decreasing = F)), fill = r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours=c('#d7191c', '#fdae61', '#ffffbf', '#a6d96a', '#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust=1))+xlab('')+ylab('')

corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

cor(train$sqft_living,(train$sqft_above+train$sqft_basement))
cor(sqft_living,sqft_above+sqft_basement)

model = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)

vif(model)

subsets = regsubsets(price~., data = train, nvmax = 10)

summary(subsets)

str(train)

newtrain <- train[,-(12:15)]
str(newtrain)

newtrain <- train[,-1]
str(newtrain)

newtrain <- train[,-(12:15)]
newtrain <- newtrain[,-1]
str(newtrain)

subsets = regsubsets(price~., data = newtrain, nvmax = 10)
summary(subsets)

subsets = regsubsets(price~., data = newtrain, nvmax = 6)
summary(subsets)

names(summary(subsets))
which.min(summary(subsets)$cp)
coef(subsets, which.min(summary(subsets)$cp))

summary(subsets$cp[6,])
summary(subsets$cp[,5])
summary(subsets)$cp[,6]
summary(subsets)$cp[6,]
summary(subsets)$cp[6]
summary(subsets)$cp

rsq.min(summary(subsets)$cp)
summary(subsets)$rsq

start_mod = lm(price~1, data = newtrain)
empty_mod = lm(price~1, data = newtrain)
full_mod = lm(price~., data = newtrain)

forwardStepwise = step(start_mod, scope=list(upper=full_mod,lower=empty_mod), direction='forward')

start_mod = lm(price~., data = newtrain)
empty_mod = lm(price~1, data = newtrain)
full_mod = lm(price~., data = newtrain)

backwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='backward')

start_mod = lm(price~1, data = newtrain)
empty_mod = lm(price~1, data = newtrain)
full_mod = lm(price~., data = newtrain)

hybridStepwise = step(start_mod, scope=list(upper=full_mod,lower=empty_mod),direction='both')

summary(forwardStepwise)
summary(hybridStepwise)

x = model.matrix(price~.-1,data=newtrain)
y = newtrain$price

ridgeModel = glmnet(x,y,alpha = 0)

plot(ridgeModel, xvar="lambda", label = T)

cv.ridge = cv.glmnet(x,y,alpha = 0)

plot(cv.ridge)
coef(cv.ridge)

lassoModel = glmnet(x,y,alpha=1)

plot(lassoModel,xvar = 'lambda',label =T)
plot(lassoModel,xvar='dev',label=T)

cv.lasso = cv.glmnet(x,y,alpha=1)

plot(cv.lasso)
coef(cv.lasso)

str(lassoModel)
names(lassoModel)

str(cv.lasso)

head(cv.lasso, 3)
names(cv.lasso)
coef(cv.lasso)

cv.lasso[-828548.0282]

cv.lasso$cvsd
cv.lasso

summary(cv.lasso)

r2_test <- fit$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.1se)]

rsq = 1 - cv.lasso$cvm/var(y)
rsq

coef(cv.lasso)

which(coef(cv.lasso)== -828548.0282)
which[max(cv.lasso$glmnet.fit)]

max(cv.lasso$glmnet.fit)

cv.lasso$glmnet.fit
cv.lasso$name

tail(cv.lasso, 5)
tail(cv.lasso, 1)

head(cv.lasso, 1)

dim(cv.lasso)
shape(cv.lasso)
names(cv.lasso)

min(cv.lasso$cvm)

rsq_calc <- cv.lasso
rsq_calc$rsq <- 1 - rsq_calc$cvm/var(y)

min(rsq_calc$rsq)

bestlam = cv.lasso$lambda.min
bestlam

lambda.id <- which(cv.lasso$lambda == cv.lasso$lambda.min)
lambda.id

mse.2 = cv.lasso$cvm[cv.lasso$lambda == bestlam]

cat("MSE (method 2): ", mse.2, "\n")

mse = function(x,y) { mean((x-y)^2)}

mse.1 = mse(cv.lasso$fit[,lambda.id], y)
mse.1

mse.1 = mse(cv.lasso$glmnet.fit[,lambda.id], y)

pr.lasso = cv.glmnet(x,y,type.measure='mse', keep=TRUE, alpha=1)

lambda.lasso = pr.lasso$lambda.min;
lambda.id <- which(pr.lasso$lambda == pr.lasso$lambda.min)

mse.1 = mse(pr.lasso$glmnet.fit[,lambda.id], y)
mse.1 = mse(pr.lasso$fit.preval[,lambda.id], y)
mse.1

coef(cv.lasso)

min(cv.lasso)
min(cv.lasso$lambda)

which(cv.lasso$lambda==558.8669)

str(cv.lasso)

cv.lasso$glmnet.fit
cv.lasso$glmnet.fit$beta

cvtest <- cv.lasso[,lamba=min(cv.lasso$lambda)]

cfr2 <- coef(cv.lasso, s = "lambda.1se")

i <- which(cv.lasso$lambda == cv.lasso$lambda.1se)
e <- cv.lasso$cvm[i]

r2 <- 1-e/var(fundm)
r2 <- 1-e/var(y)
r2

test <- test[,-1]
test <- test[,-(11:14)]
str(test)

houses2 <- read.csv('houses.csv')

split2 <- createDataPartition(houses2$price, p = .7, groups = 100, list = F)

train2 = houses2[split2,]
test2 = houses2[-split2,]

trainPredictors = train2[,c(3:11,16)];
testPredictors = test2[,c(3:11,16)]

x2 = preProcess(x2 = trainPredictors,method = 'pca',thresh = 0.9)
x2 = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)

trainComponents = predict(x2,newdata=trainPredictors)
trainComponents$price = train2$price

str(trainComponents)
summary(trainComponents)
head(trainComponents)

train_model = lm(price~., trainComponents)
summary(train_model)

pred2 = predict(train_model, newdata=trainComponents)

sse2 = sum((pred2-trainComponents$price)^2)
sst2 = sum((mean(trainComponents$price)-trainComponents$price)^2)

r2_train2 = 1-sse2/sst2
r2_train2

testComponents = predict(x2, newdata = testPredictors)
testComponents$price = test2$price

str(testComponents)

pred2 = predict(train_model, newdata=testComponents)
pred3 = predict(train_model, newdata=testComponents)

sse3 = sum((pred3-testComponents$price)^2)
sst3 = sum((mean(trainComponents$price)-testComponents$price)^2)

r2_test3 = 1-sse3/sst3
r2_test3

coef(cv.lasso)
cv.lasso$glmnet.fit

train = newtrain

xvars = x

y_var = y
y_var = train$price

cv_output <- cv.glmnet(xvars,y_var,alpha=1)

idealamda <- cv_output$lambda.min
idealamda

lasso_best <- glmnet(xvars,y_var,alpha = 1, lambda = idealamda)
pred_best <- predict(lasso_best, s = idealamda, newx = xvars)

final <- cbind(y_var, pred_best)

head(final)

fit.lasso = glmnet(x,y,alpha = 1)

plot(fit.lasso, xvar = "lambda", label = T)

cv.lasso = cv.glmnet(x,y, alpha = 1)

plot(cv.lasso)
coef(cv.lasso)

cv.lasso$lambda.min
coef(cv.lasso, s =  558.8669)

lasso.tr = glmnet(xvars[train,], y_var[train])
plot(fit.lasso,xvar="dev",label=TRUE)

coef(cv.lasso)

xvar

xvars
str(xvars)
head(xvars, 3)

summary(cv.lasso[lambda==lambda.min])
which(cv.lasso[lambda==lambda.min])

bestlam

cvfit = cv.glmnet(x,y,family = "binomial", type.measure = "class")

names(cv.lasso)
names(cv.glmnet)

cv.lasso$glmnet.fit$dev.ratio

str(cv.lasso$glmnet.fit$dev.ratio)

which(min(cv.lasso$lambda))
which(cv.lasso$lambda==bestlam)

cv.lasso$glmnet.fit$dev.ratio[67]
