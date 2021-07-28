[Workspace loaded from ~/.RData]

setwd("~/Documents/Spring Semester/5200 R/Rcode/csv")

wages <- read.csv("assignment7_wages.csv")
wages = wages[wages$earn>0,]

set.seed(1731)

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

rmse5_test = sqrt(mean((pred5-test$earn)^2))
rmse5_test
