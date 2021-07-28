library(tidyverse)
library(ggplot2)
library(dplyr)
library(rlang)
library(devtools)
library(mapdata)
library(gapminder)
library(maps)
library(caret)
library(ISLR)
library(caTools)
library(tidyr)

install.packages(c("ggthemes", "latex2exp", "odds.converter", "pinnacle.data", "rstanarm"))

set.seed(1731)
setwd("~/Documents/Spring Semester/5200 R/csv")

wages <- read.csv('assignment7_wages.csv')

str(wages)
wages = wages[wages$earn>0,]  # remove rows with negative earning
nrow(wages)

levels(wages$sex)
names(wages$sex)
level(wages$sex = 2)
level(wages$sex[1])
level(wages[,wages$sex == "female"])

head(wages, 3)
level(wages[,2])

level(wages$sex[2])
level(wages$sex[2])[2]
wages$sex[2]
wages$sex[1]
nrow(wages$sex == "female")

nrow(wages[,wages$sex==1])
sum(wages[,wages$sex==1])

female <- wages[,wages$sex == "female"]
female <- wages[,wages$sex == 1]
male <- wages[,wages$sex != 1]

names(wages)

sum(wages$sex == 1)
sum(wages$sex == "female")

# 859/1368

levels(wages$race)
black <- wages[,wages$race == "black"]
black <- wages[wages$race == "black",]

female <- wages[wages$sex == "female",]
male <- wages[wages$sex == "male",]

hispanic <- wages[wages$race == "hispanic",]
other <- wages[wages$race == "other",]
white <- wages[wages$race == "white",]

avg(black$earn)
mean(black$earn)
mean(hispanic$earn)
mean(other$earn)
mean(white$earn)


split = sample(1:nrow(wages), nrow(wages)*0.75)

train = wages[split,]
test = wages[-split,]

# 1026/1368

model1 = lm(formula = earn ~ height + sex + race + ed + age, data = train)
summary(model1)

pred = predict(model1)

sse = sum((pred-wages$earn)^2)
sst = sum((mean(wages$earn)-wages$earn)^2)

model1_r2 = 1 - sse/sst
model1_r2

rmse = sqrt(mean((pred-wages$earn)^2))
rmse

sse = sum((pred-train$earn)^2)
sst = sum((mean(train$earn)-train$earn)^2)

model1_r2 = 1 - sse/sst
model1_r2

rmse = sqrt(mean((pred-train$earn)^2))
rmse

ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+
geom_bar(stat="summary",fun.y="mean",position="dodge")
ggplot(data=train,aes(y=earn,x=ed,color=sex))+
geom_smooth(method="lm",se=F,size=1.2)+
scale_x_continuous(breaks=c(seq(2,20,2)))+
scale_y_continuous(breaks=c(seq(0,100000,10000)))

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)
summary(model_sex_ed)

model2 = lm(formula = earn ~ height + sex + race + ed + age + sex * ed, data = train)
summary(model2)

pred = predict(model2)
sse = sum((pred-train$earn)^2)

pred = predict(model1)
sse = sum((pred-train$earn)^2)

pred = predict(model1)
pred2 = predict(model2)

sse2 = sum((pred2-train$earn)^2)
sst2 = sum((mean(train$earn)-train$earn)^2)

model2_r2 = 1 - sse2/sst2
model2_r2

rmse = sqrt(mean((pred-train$earn)^2))
rmse2 = sqrt(mean((pred2-train$earn)^2))

model3 = lm(formula = earn ~ height + sex + race + ed + age + sex * ed + sex * age, data = train)
summary(model3)
pred3 = predict(model3)

sse3 = sum((pred3-train$earn)^2)
sst3 = sum((mean(train$earn)-train$earn)^2)

model3_r2 = 1 - sse3/sst3
model3_r2

rmse3 = sqrt(mean((pred3-train$earn)^2))
rmse3

model4 = lm(formula = earn ~ height + sex + race + ed + age + sex * ed +
sex * age + age * ed, data = train)
pred4 = predict(model4)
summary(model4)

sse4 = sum((pred4-train$earn)^2)
sst4 = sum((mean(train$earn)-train$earn)^2)

model4_r2 = 1 - sse4/sst4
model4_r2

rmse4 = sqrt(mean((pred4-train$earn)^2))
rmse4

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
summary(model5)

pred5 = prediction(model5)
pred5 = predict(model5)

rmse5 = sqrt(mean((pred5-train$earn)^2))
rmse5

sse5 = sum((pred5-train$earn)^2)
sst5 = sum((mean(train$earn)-train$earn)^2)

model5_r2 = 1 - sse5/sst5
model5_r2
