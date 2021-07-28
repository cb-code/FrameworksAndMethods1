[Workspace loaded from ~/.RData]

install.packages("prp")
library(rpart)
library(rpart.plot)

setwd("~/Documents/Spring Semester/5200 R/Rcode/csv")

set.seed(1731)

wages <- read.csv("assignment7_wages.csv")
wages = wages[wages$earn>0,]

split = sample(1:nrow(wages), nrow(wages)*0.75)

train = wages[split,]
test = wages[-split,]

model1 = lm(earn~., data = train)
summary(model1)

model1 = lm(earn ~ height + sex + race + ed + age, data = train)
summary(model1)

pred1 = predict(model1)

rmse1 = sqrt(mean((pred1-train$earn)^2))
rmse1

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)

predsx = predict(model_sex_ed)

rmsesx = sqrt(mean((predsx - train$earn)^2))
rmsesx

model2 = lm(earn~height+sex+race+ed+age+sex*ed, data = train)
pred2 = predict(model2)

rmse2 = sqrt(mean((pred2-train$earn)^2))
rmse2

model3 = lm(earn~height+sex+race+ed+age+sex*ed+sex*age, data = train)
pred3 = predict(model3)

rmse3 = sqrt(mean((pred3-train$earn)^2))
rmse3

model4 = lm(earn~height+sex+race+ed+age+sex*ed+sex*age+age*ed, data = train)
pred4 = predict(model4)

rmse4 = sqrt(mean((pred4-train$earn)^2))
rmse4

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5 = predict(model5)

rmse5 = sqrt(mean((pred5-train$earn)^2))
rmse5

pred5_test = predict(model5, newdata= test)

rmse5_test = sqrt(mean((pred5_test- test$earn)^2))
rmse5_test

tree1 = rpart(earn~height+sex+race+ed+age, data = train)

rpart.plot(tree1, digits = 5)

predTree1 = predict(tree1)

rmseTree1 = sqrt(mean((predTree1 - train$earn)^2))
rmseTree1

treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))

predSimp1 = predict(treeSimp1)

rmseSimp1 = sqrt(mean((predSimp1 - train$earn)^2))
rmseSimp1

treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))

predSimp2 = predict(treeSimp2)

rmseSimp2 = sqrt(mean((predSimp2 - train$earn)^2))
rmseSimp2

treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))

predComp1 = predict(treeComplex1)

rmseComp1 = sqrt(mean((predComp1 - train$earn)^2))
rmseComp1

treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))

predComp2 = predict(treeComplex2)

rmseComp2 = sqrt(mean((predComp2 - train$earn)^2))
rmseComp2
