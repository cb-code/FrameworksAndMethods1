#________________________________________________________________________________________
#                                                                                        #
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    | Columbia University: APAN S5200, Spring 2020 |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [1] Feature Selection              |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#


#loading in required packages

library(beepr)
library(tidyr)
library(dplyr)

library(gbm)
library(Hmisc)
library(xgboost)

library(caret)
library(caTools)
library(corrplot)

library(rpart)
library(regclass)
library(rpart.plot)
library(randomForest)

#reading in Kaggle AirBnb Data CSV Files: Analysis Data and Scoring Data
data <- read.csv('analysisData.csv')
scoringData <- read.csv('scoringData.csv')

#identifying which columns have 60% or more NA values
index_na_keep = colSums(is.na(data)) <= nrow(data) * 0.6
cdata = data[, index_na_keep]

#attempts to coerce all data frame columns with NA values
index_na = colSums(is.na(cdata))
index_na = as.logical(index_na)
index_na = cdata[, index_na]

#deciding which columns to use: did not implement the following line [58]
index_na_use = index_na[, -c(1:4, 7)]

#creating train and test data with seed value 1984 for reproducibility
set.seed(1984)
split = sample.split(Y = cdata$price,
                     SplitRatio = 0.7,
                     group = NULL)
train = cdata[split, ]
test = cdata[!split, ]

#attempting to fill in NA values by way of calculating na.omit means and imputing those for each column
means = lapply(cdata, mean, na.rm = TRUE)

NA_means = c(
     means$bathrooms,
     means$bedrooms,
     means$review_scores_accuracy,
     means$review_scores_cleanliness,
     means$review_scores_checkin,
     means$review_scores_communication,
     means$review_scores_location,
     means$review_scores_value
)

cdata$bathrooms[is.na(cdata$bathrooms)] <- means$bathrooms
cdata$bedrooms[is.na(cdata$bedrooms)]  <- means$bedrooms
cdata$review_scores_accuracy[is.na(cdata$review_scores_accuracy)] <-
     means$review_scores_accuracy
cdata$review_scores_cleanliness[is.na(cdata$review_scores_cleanliness)] <-
     means$review_scores_cleanliness
cdata$review_scores_checkin[is.na(cdata$review_scores_checkin)] <-
     means$review_scores_checkin
cdata$review_scores_communication[is.na(cdata$review_scores_communication)] <-
     means$review_scores_communication
cdata$review_scores_location[is.na(cdata$review_scores_location)] <-
     means$review_scores_location
cdata$review_scores_value[is.na(cdata$review_scores_value)] <-
     means$review_scores_value

#checking some data values in which the data appears almost homogenous, with little to no variance
hostverificationtrue = subset(cdata, cdata$host_verifications == "true")
hostpictrue = subset(cdata, cdata$host_has_profile_pic == "true")
hostidtrue = subset(cdata, cdata$host_id == "true")
guestpictrue = subset(cdata, cdata$require_guest_profile_picture == "true")
guestphonetrue = subset(cdata, cdata$require_guest_phone_verification == "true")
licensetrue = subset(cdata, cdata$requires_license == "true")

#checking for empty security deposit entries, which can be surmised would also mean no deposit/deposit = $0.00
levels(cdata$security_deposit)[levels(cdata$security_deposit) == ""] <-
     "$0.00"

#setting the empty factor level for cleaning_fee var to the mean value for cleaning_fee var
round(mean(as.numeric(cdata$cleaning_fee)), 2)
levels(cdata$cleaning_fee)[levels(cdata$cleaning_fee) == ""] <-
     "$75.63"

#using class pdf code to build a heatmap for variable correlation/importance
spectrum = colorRampPalette(c("blue", "white", "red")) (20)
heatmap(x = ndata.cor, col = spectrum, symm = TRUE)

#different method of splitting data and creating test/train datasets, for plot/mapping of corr.
numSplit = createDataPartition(
     y = numData$price,
     p = 0.7,
     list = F,
     groups = 100
)
numTrain = numData[split, ]
numTest = numData[-split, ]

cor(numTrain[, -24])
col2 = colorRampPalette(c(
     '#d73027',
     '#fc8d59',
     '#fee08b',
     '#ffffbf',
     '#d9ef8b',
     '#91cf60',
     '#1a9850'
))
corrplot(
     cor(numTrain[, -24]),
     method = 'square',
     type = 'lower',
     diag = F,
     col = col2(7)
)

#cleaning up the data we will use -- possible that removed vars here will be restored later, so we create copies of df
cdata_copy = cdata[, -c(1,
                        3:6,
                        8:20,
                        24:32,
                        34:43,
                        49,
                        52:56,
                        58,
                        61:62,
                        64:70,
                        72:73,
                        75:77,
                        79:82,
                        84:92,
                        94:97)]
cdata_copy = cdata_copy[, -(3:4)]
cdata_copy = cdata_copy[, -11]

#converting description data into numeric data given coursework on
#eBay listings and personal experience with AirBnb rentals regarding numeric "size",
#or length of descriptions as meaningful in terms of their content volume itself
cdata_copy$description = as.numeric(cdata_copy$description)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [2] Stepwise Analysis              |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#creating train and test data from copy of subsetted data with seed value 1984 for reproducibility
set.seed(1984)
cSplit_copy = sample.split(Y = cdata_copy, SplitRatio = 0.7)
cTrain_copy = cdata_copy[cSplit_copy, ]
cTest_copy = cdata_copy[!cSplit_copy, ]

#feature selection method with particular variables: Stepwise analysis part 1 [Forward]
start_modf = lm(price ~ 1, data = cTrain)
empty_modf = lm(price ~ 1, data = cTrain)
full_modf = lm(price ~ ., data = cTrain)

forwardStepwise = step(
     start_modf,
     scope = list(upper = full_modf, lower = empty_modf),
     direction = 'forward'
)

summary(forwardStepwise)

#feature selection method with particular variables: Stepwise analysis part 2 [Backward]
start_modb = lm(price ~ ., data = cTrain)
empty_modb = lm(price ~ 1, data = cTrain)
full_modb = lm(price ~ ., data = cTrain)

backwardStepwise = step(
     start_modb,
     scope = list(upper = full_modb, lower = empty_modb),
     direction = 'backward'
)

summary(backwardStepwise)

#feature selection method with particular variables: Stepwise analysis part 3 [Hybrid]
start_modv = lm(price ~ 1, data = cTrain)
empty_modv = lm(price ~ 1, data = cTrain)
full_modv = lm(price ~ ., data = cTrain)

hybridStepwise = step(
     start_modv,
     scope = list(upper = full_modv, lower = empty_modv),
     direction = 'both'
)

summary(hybridStepwise)

#feature selection method with particular variables: Stepwise analysis part 4 [Hybrid - cleaned/modified data]
start_modv2 = lm(price ~ 1, data = cdata_copy)
empty_modv2 = lm(price ~ 1, data = cdata_copy)
full_modv2 = lm(price ~ ., data = cdata_copy)

hybridStepwisev2 = step(
     start_modv2,
     scope = list(upper = full_modv2, lower = empty_modv2),
     direction = 'both'
)

summary(hybridStepwisev2)

#feature selection method with particular variables: Stepwise analysis part 5 [Hybrid - cleaned/modified data]
start_modv3 = lm(price ~ 1, data = cdata_copy)
empty_modv3 = lm(price ~ 1, data = cdata_copy)
full_modv3 = lm(price ~ ., data = cdata_copy)

hybridStepwisev3 = step(
     start_modv3,
     scope = list(upper = full_modv3, lower = empty_modv3),
     direction = 'both'
)

summary(hybridStepwisev3)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [3]  Linear  Modeling              |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#


#prediction method using particular variables: linear modeling part 1
modelv = lm(
     formula = price ~ accommodates + minimum_maximum_nights++room_type + neighbourhood_group_cleansed + maximum_nights++bathrooms + bedrooms + calculated_host_listings_count++availability_365 + review_scores_location + review_scores_communication++host_response_time + review_scores_cleanliness + review_scores_accuracy++review_scores_value + number_of_reviews + minimum_nights++maximum_minimum_nights,
     data = cdata_copy
)

predv = predict(modelv, newdata = scoringData)

predt = predict(modelv, newdata = cTrain_copy)

sse_predt = sum((predt - cTrain_copy$price) ^ 2)
sst_predt = sum((mean(cTrain_copy$price) - cTrain_copy$price) ^ 2)

modelt_r2 = 1 - sse_predt / sst_predt
rmset = sqrt(mean((predt - cTrain_copy$price) ^ 2))

#prediction method using particular variables: linear modeling part 2
modelv2 = lm(
     formula = price ~
          host_is_superhost + as.numeric(description)
     + room_type + bathrooms + bedrooms + bed_type
     + accommodates + guests_included++minimum_nights + minimum_maximum_nights + availability_90++review_scores_location + review_scores_value + review_scores_rating
     + number_of_reviews + calculated_host_listings_count + neighbourhood_group_cleansed,
     data = cdata_copy
)

predv2 = predict(modelv2, newdata = scoringData)

#trying modelv2 on copy_data (cleaned/modified) for train data - prediction method: linear modeling part 2.5
predt2 = predict(modelv2, newdata = cTrain_copy)

sse_predt2 = sum((predt2 - cTrain_copy$price) ^ 2)
sst_predt2 = sum((mean(cTrain_copy$price) - cTrain_copy$price) ^ 2)

modelt2_r2 = 1 - sse_predt2 / sst_predt2
rmset2 = sqrt(mean((predt2 - cTrain_copy$price) ^ 2))

#To Fix:
#    Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) :
#         Factor extra_people has new levels
#         $102.00, $109.00, $167.00, $176.00, $180.00, $195.00, $42.00, $46.00, $63.00, $79.00, $82.00, $94.00

#feature selection method with particular variables: linear modeling part 3
modelv3 = lm(
     formula = price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location + review_scores_value +
          bedrooms + review_scores_rating + minimum_nights,
     data = cdata_copy
)

predv3 = predict(modelv3, newdata = scoringData)

#trying modelv3 on copy_data (cleaned/modified) for train data - prediction method: linear modeling part 3.5
predt3 = predict(modelv3, newdata = cTrain_copy)

sse_predt3 = sum((predt3 - cTrain_copy$price) ^ 2)
sst_predt3 = sum((mean(cTrain_copy$price) - cTrain_copy$price) ^ 2)

modelt3_r2 = 1 - sse_predt3 / sst_predt3
rmset3 = sqrt(mean((predt3 - cTrain_copy$price) ^ 2))

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [4]  Tree Modeling                 |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#prediction method using particular variables: tree modeling part 1
modeltree = rpart(
     price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location + review_scores_value +
          bedrooms + review_scores_rating + minimum_nights,
     data = cdata_copy
)

predTree = predict(modeltree, newdata = scoringData)

#prediction method using particular variables for train data: tree modeling part 1.5
trainTree = rpart(
     price ~ accommodates + minimum_maximum_nights++neighbourhood_group_cleansed + bathrooms++calculated_host_listings_count + availability_90 + room_type++number_of_reviews + review_scores_location + review_scores_value++bedrooms + review_scores_rating + minimum_nights,
     data = cTrain_copy
)

predtrainTree = predict(tree, newdata = cTrain_copy)
predtestTree = predict(tree, newdata = cTest_copy)

rmsetrainTree = sqrt(mean((predtrainTree - cTrain_copy$price) ^ 2))
rmsetestTree = sqrt(mean((predtestTree - cTest_copy$price) ^ 2))

#prediction method using particular variables: attempted cv modeling part 1 *Edit Notation for this*
trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(.cp = seq(0, 0.1, 0.001))

cvModel = train(
     price ~ .,
     data = cTrain_copy,
     method = "rpart",
     trControl = trControl,
     tuneGrid = tuneGrid
)
cvModel$bestTune

treeCV = rpart(price ~ .,
               data = cTrain_copy,
               control = rpart.control(cp = cvModel$bestTune))

predTreeCV = predict(treeCV, newdata = cTest_copy)

rmseCV = sqrt(mean((predTreeCV - cTest_copy$price) ^ 2))

#prediction method using particular variables: rpart/tree modeling part 1
modelPtree = rpart(
     price ~ accommodates + minimum_maximum_nights++neighbourhood_group_cleansed + bathrooms++calculated_host_listings_count + availability_90 + room_type++number_of_reviews + review_scores_location + review_scores_value++bedrooms + review_scores_rating + minimum_nights,
     data = cdata_copy
)

#applying to scoringData - predictive model for rpart generated tree 1
predPTree = predict(modelPtree, newdata = scoringData)

#writing csv file for Kaggle submission - predictive model for rpart generated tree 1
submission_d4s1_modelt = data.frame(id = scoringData$id, price = predTree)
write.csv(submission_d4s1_modelt,
          'submission_d4s1_modelt.csv',
          row.names = F)

#prediction method using particular variables: Random Forest modeling part 1
forest1 = randomForest(
     price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location +
          bedrooms + review_scores_rating + minimum_nights,
     data = cdata_copy,
     ntree = 100,
     na.action = na.exclude
)

#applying to scoringData - predictive model for Random Forest modeling 1
predForest1 = predict(forest1, newdata = scoringData)
rmseForest1 = sqrt(mean((predForest1 - scoringData$price) ^ 2))

#writing csv file for Kaggle submission - predictive model for Random Forest modeling 1
submission_d3s2 = data.frame(id = scoringData$id, price = predForest1)
write.csv(submission_d3s2, 'submission_d3s2.csv', row.names = F)

#prediction method using particular variables: Random Forest modeling part 2
forest2 = randomForest(
     price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location +
          bedrooms + review_scores_rating + minimum_nights,
     data = cdata_copy,
     ntree = 750,
     na.action = na.exclude
)

#applying to scoringData - predictive model for Random Forest modeling 2
predForest2 = predict(forest2, newdata = scoringData)
rmseForest2 = sqrt(mean((predForest2 - scoringData$price) ^ 2))

#writing csv file for Kaggle submission - predictive model for Random Forest modeling 2
submission_d3s3 = data.frame(id = scoringData$id, price = predForest2)
write.csv(submission_d3s3, 'submission_d3s3.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [5] Boosted Modeling               |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#setting seed to 1984 for reproducibility
set.seed(1984)

boost_data <- cdata_copy

boost_split = sample.split(Y = boost_data, SplitRatio = 0.7)
boost_train = boost_data[boost_split, ]
boost_test = boost_data[!boost_split, ]

#prediction method using cdata_copy variables: boosted modeling part 1
boost_copy = gbm(
     formula = formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 3,
     shrinkage = 0.001
)

#applying to scoringData - predictive model for boosted modeling part 1
predBoostTrain_copy = predict(boost_train, n.trees = 100000)
rmseBoostTrain_copy = sqrt(mean((predBoostTrain_copy - boost_train$price) ^
                                     2))
summary(boost_copy)

predboost_copy = predict(boost_copy, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for Boosted Modeling 1
submission_d4s2 = data.frame(id = scoringData$id, price = predboost_copy)
write.csv(submission_d4s2, 'submission_d4s2.csv', row.names = F)

#could not complete after running for over 6 hours on laptop...sad
trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(
     n.trees = 1000,
     interaction.depth = c(1, 2),
     shrinkage = (1:100) * 0.001,
     n.minobsinnode = 5
)
garbage = capture.output(
     cvBoost <-
          train(
               formula = price ~ .,
               data = boost_data,
               method = "gbm",
               trControl = trControl,
               tuneGrid = tuneGrid,
               na.action = na.exclude
          )
)

boostCV = gbm(
     formula = price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location +
          bedrooms + review_scores_rating + minimum_nights,
     data = boost_data,
     distribution = "gaussian",
     n.trees = cvBoost$bestTune$n.trees,
     interaction.depth = cvBoost$bestTune$interaction_depth,
     shrinkage = cvBoost$bestTune$shrinkage,
     n.minobsinnode = cvBoost$bestTune$n.minobsinnode
)

predBoostCV = predict(boostCV, scoringData, n.trees = 1000)
rmseBoostCV = sqrt(mean((predBoostCV - scoringData$price) ^ 2, na.action = na.exclude))

#trying greater interaction depth
#prediction method on boost_data: boosted modeling part 2
boost_copy_v1 = gbm(
     formula = price ~ accommodates + minimum_maximum_nights +
          neighbourhood_group_cleansed + bathrooms +
          calculated_host_listings_count + availability_90 + room_type +
          number_of_reviews + review_scores_location +
          bedrooms + review_scores_rating + minimum_nights,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 5,
     shrinkage = 0.001
)

predBoostTrain_copy_v1 = predict(boost_copy_v1, n.trees = 100000)
rmseBoostTrain_copy_v1 = sqrt(mean((
     predBoostTrain_copy_v1 - boost_train$price
) ^ 2))
summary(boost_copy_v1)

#applying to scoringData - predictive model for boosted modeling part 2
predboost_copy_v1 = predict(boost_copy_v1, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 2
submission_d4s3 = data.frame(id = scoringData$id, price = predboost_copy_v1)
write.csv(submission_d4s3, 'submission_d4s3.csv', row.names = F)

#instead of specifying particular variables within the subsetted/cleaned boost_data,
#model will be run on the entirety of the boost_data subset variant (gbm(formula = price~.)...)
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 3
boost_copy_v2 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 5,
     shrinkage = 0.001
)

predBoostTrain_copy_v2 = predict(boost_copy_v2, n.trees = 100000)
rmseBoostTrain_copy_v2 = sqrt(mean((
     predBoostTrain_copy_v2 - boost_train$price
) ^ 2))
summary(boost_copy_v2)

#applying to scoringData - predictive model for boosted modeling part 3
predboost_copy_v2 = predict(boost_copy_v2, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 3
submission_d5s1 = data.frame(id = scoringData$id, price = predboost_copy_v2)
write.csv(submission_d5s1, 'submission_d5s1.csv', row.names = F)

#trying greater n.trees, lowering interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 4
boost_copy_v3 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 150000,
     interaction.depth = 3,
     shrinkage = 0.001
)

predBoostTrain_copy_v3 = predict(boost_copy_v3, n.trees = 150000)
rmseBoostTrain_copy_v3 = sqrt(mean((
     predBoostTrain_copy_v3 - boost_train$price
) ^ 2))
summary(boost_copy_v3)

#applying to scoringData - predictive model for boosted modeling part 4
predboost_copy_v3 = predict(boost_copy_v3, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 4
submission_d5s2 = data.frame(id = scoringData$id, price = predboost_copy_v3)
write.csv(submission_d5s2, 'submission_d5s2.csv', row.names = F)

#trying greater n.trees, increased interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 5
boost_copy_v4_s2 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 150000,
     interaction.depth = 5,
     shrinkage = 0.001
)

predBoostTrain_copy_v4_s2 = predict(boost_copy_v4_s2, n.trees = 150000)
rmseBoostTrain_copy_v4_s2 = sqrt(mean((
     predBoostTrain_copy_v4_s2 - boost_train_s2$price
) ^ 2))
summary(boost_copy_v4_s2)

#applying to scoringData - predictive model for boosted modeling part 5
predboost_copy_v4_s2 = predict(boost_copy_v4_s2, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 5
submission_d5s3 = data.frame(id = scoringData$id, price = predboost_copy_v4_s2)
write.csv(submission_d5s3, 'submission_d5s3.csv', row.names = F)

#trying a new seed value and splitting data given time interval between boosting models
set.seed(617)
cSplit_copy_s2 = sample.split(Y = cdata_copy, SplitRatio = 0.7)
boost_train_s2 = cdata_copy[cSplit_copy_s2, ]
cTest_copy_s2 = cdata_copy[!cSplit_copy_s2, ]

#using different data for testing/training, set seed value to 617
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 6
boost_copy_v4 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 150000,
     interaction.depth = 5,
     shrinkage = 0.001
)

predBoostTrain_copy_v4 = predict(boost_copy_v4, n.trees = 150000)
rmseBoostTrain_copy_v4 = sqrt(mean((
     predBoostTrain_copy_v4 - boost_train$price
) ^ 2))
summary(boost_copy_v4)

#applying to scoringData - predictive model for boosted modeling part 6
predboost_copy_v4 = predict(boost_copy_v4, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 6
submission_d6s1 = data.frame(id = scoringData$id, price = predboost_copy_v4)
write.csv(submission_d6s1, 'submission_d6s1.csv', row.names = F)

#re-setting seed and splitting data in case of any changes from prior seed choices, set seed to 1984
set.seed(1984)
cSplit_copy = sample.split(Y = cdata_copy, SplitRatio = 0.7)
boost_train = cdata_copy[cSplit_copy, ]
cTest_copy = cdata_copy[!cSplit_copy, ]

#trying reduced n.trees, increased interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 7
boost_copy_v5 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 7,
     shrinkage = 0.001
)

predBoostTrain_copy_v5 = predict(boost_copy_v5, n.trees = 100000)
rmseBoostTrain_copy_v5 = sqrt(mean((
     predBoostTrain_copy_v5 - boost_train$price
) ^ 2))
summary(boost_copy_v5)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#applying to scoringData - predictive model for boosted modeling part 7
predboost_copy_v5 = predict(boost_copy_v5, newdata = scoringData, n.trees = 100000)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#writing csv file for Kaggle submission - predictive model for boosted modeling part 7
submission_d6s2 = data.frame(id = scoringData$id, price = predboost_copy_v5)
write.csv(submission_d6s2, 'submission_d6s2.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying increased interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 7
boost_copy_v6 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 10,
     shrinkage = 0.001
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

predBoostTrain_copy_v6 = predict(boost_copy_v6, n.trees = 100000)
rmseBoostTrain_copy_v6 = sqrt(mean((
     predBoostTrain_copy_v6 - boost_train$price
) ^ 2))
summary(boost_copy_v6)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#applying to scoringData - predictive model for boosted modeling part 8
predboost_copy_v6 = predict(boost_copy_v6, newdata = scoringData, n.trees = 100000)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#writing csv file for Kaggle submission - predictive model for boosted modeling part 8
submission_d6s3 = data.frame(id = scoringData$id, price = predboost_copy_v6)
write.csv(submission_d6s3, 'submission_d6s3.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying increased interaction.depth + check gbm.perf to attempt boosting iteration optimization
#prediction method on boost_data with gbm.perf post-test analysis - boosting iteration optimization part 1
boost_copy_v7 = gbm(
     formula = price ~ .,
     data = boost_data,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 12,
     shrinkage = 0.001
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

predBoostTrain_copy_v7 = predict(boost_copy_v7, n.trees = 100000)
rmseBoostTrain_copy_v7 = sqrt(mean((
     predBoostTrain_copy_v7 - boost_train$price
) ^ 2))
summary(boost_copy_v7)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#applying to scoringData - boosting iteration optimization part 1
predboost_copy_v7 = predict(boost_copy_v7, newdata = scoringData, n.trees = 100000)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#writing csv file for Kaggle submission - boosting iteration optimization part 1
T_Minus3_Sub1 = data.frame(id = scoringData$id, price = predboost_copy_v7)
write.csv(T_Minus3_Sub1, 'T_Minus3_Sub1.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying reduced interaction.depth( = 10)
#prediction method on boost_data with gbm.perf post-test analysis - boosting iteration optimization part 2
ogbm_v1 = gbm(
     formula = price ~ .,
     data = boost_data,
     cv.folds = 10,
     verbose = TRUE,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 10,
     shrinkage = 0.001
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

train_ogbm_v1 = predict(ogbm_v1, n.trees = 100000)
rmse_train_ogbm_v1 = sqrt(mean((train_ogbm_v1 - boost_train$price) ^ 2))
summary(ogbm_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#applying to scoringData - boosting iteration optimization part 2
pred_ogbm_v1 = predict(ogbm_v1, newdata = scoringData, n.trees = 100000)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#writing csv file for Kaggle submission - boosting iteration optimization part 2
T_Minus2_Sub1 = data.frame(id = scoringData$id, price = pred_ogbm_v1)
write.csv(T_Minus2_Sub1, 'T_Minus2_Sub1.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying reduced interaction.depth( = 10)
#prediction method on boost_data with gbm.perf post-test analysis - boosting iteration optimization part 2
ogbm_v1 = gbm(
     formula = price ~ .,
     data = boost_data,
     cv.folds = 10,
     verbose = TRUE,
     distribution = "gaussian",
     n.trees = 100000,
     interaction.depth = 10,
     shrinkage = 0.001
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

train_ogbm_v1 = predict(ogbm_v1, n.trees = 100000)
rmse_train_ogbm_v1 = sqrt(mean((train_ogbm_v1 - boost_train$price) ^ 2))
summary(ogbm_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#applying to scoringData - boosting iteration optimization part 2
pred_t1_v1 = predict(ogbm_v1, newdata = scoringData, n.trees = 100000)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#writing csv file for Kaggle submission - boosting iteration optimization part 2
T_Minus1_Sub1 = data.frame(id = scoringData$id, price = pred_t1_v1)
write.csv(T_Minus1_Sub1, 'T_Minus1_Sub1.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |           [6] Training                       |                    #
#                    |               Boosted Modeling               |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#ideally we will be able to implement CV and derive our model parameters through the CV model process
#might need to work on the hardware limitations of this computational resource need first, however

library(gbm)

#creating copy of analysis data csv for gbm fit purposes
cdata_gbmfit <- cdata

#converting description data into numeric data given coursework on eBay listings and personal experience with AirBnb rentals
cdata_gbmfit$description = as.numeric(cdata_gbmfit$description)

#inspecting factors for levels over 1024 or 1, removing these columns/vars to clean data for gbm fit
fcdata <- cdata_gbmfit[, sapply(cdata_gbmfit, is.factor)]
fcvect <-
     sapply(fcdata, nlevels) > 1 & sapply(fcdata, nlevels) < 1025

fcfit <- fcdata[, fcvect]
f2remove <- fcdata[, !fcvect]

names2remove <- names(f2remove)

cdata_gbmfit_clean <- cdata_gbmfit
cdata_gbmfit_clean <-
     cdata_gbmfit_clean[, !(names(cdata_gbmfit_clean) %in% names2remove)]

#reducing n.trees and interaction.depth, including cv.folds and n.cores parameters explicitly
#establishing a GBM model for training purposes, trying to determine ideal GBM model conditions
#using all cores by default: n.cores = NULL,
#using cross-validation folds to detect and ideally reduce bias/overfitting

#re-setting seed and splitting data with seed set to 1984 for reproducibility/testing
set.seed(1984)

gbm_split = sample.split(Y = cdata_gbmfit_clean, SplitRatio = 0.7)
gbm_train = cdata_gbmfit_clean[gbm_split, ]
gbm_test = cdata_gbmfit_clean[!gbm_split, ]

gbm_fit_v1 <-
     gbm(
          formula = price ~ .,
          data = cdata_gbmfit_clean,
          distribution = "gaussian",
          n.trees = 10000,
          interaction.depth = 1,
          shrinkage = 0.001,
          cv.folds = 5,
          n.cores = NULL,
          verbose = FALSE
     )

#viewing gbm fit results
print(gbm_fit_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_gbm_v1 = predict(gbm_fit_v1, n.trees = 10000)
rmse_gbm_v1 = sqrt(mean((pred_gbm_v1 - gbm_train$price) ^ 2))

print(gbm_fit_v1)
summary(gbm_fit_v1)

#The best cross-validation iteration was 3161.
#There were 75 predictors of which 14 had non-zero influence.

#determining variable importance (more feature selection!) - gbm fitting model part 1
#syntax error
gbm_fit_labels_v1 = colnames(pred_gbm_v1)[apply(pred_gbm_fit_v1, 1, which.max)]
gbm_fit_result_v1 = data.frame(gbm_test$price, gbm_fit_labels_v1)

#applying to scoringData - gbm fitting model part 1
pred_gbm_fit_v1 = predict(gbm_fit_v1, newdata = scoringData, n.trees = 3161)

#writing csv file for Kaggle submission - predictive model for gbm fitted modeling part 1
submission_d7s1 = data.frame(id = scoringData$id, price = pred_gbm_fit_v1)
write.csv(submission_d7s1, 'submission_d7s1.cs1', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |                                              |                    #
#                    |           [[ATTEMPT/APPROACH #2]]            |                    #
#                    |                                              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#


#reducing n.trees and interaction.depth, including cv.folds and n.cores parameters explicitly
#establishing a GBM model for training purposes, trying to determine ideal GBM model conditions
#using all cores by default: n.cores = NULL,
#using cross-validation folds to detect and ideally reduce bias/overfitting

set.seed(1984)

copy_split = sample.split(Y = cdata_copy, SplitRatio = 0.7)
copy_train = cdata_copy[copy_split, ]
copy_test = cdata_copy[!copy_split, ]

gbm_copy_v1 <-
     gbm(
          formula = price ~ .,
          data = cdata_copy,
          distribution = "gaussian",
          n.trees = 10000,
          interaction.depth = 1,
          shrinkage = 0.001,
          cv.folds = 5,
          n.cores = NULL,
          verbose = FALSE
     )

#viewing gbm fit results
print(gbm_copy_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_copy_v1 = predict(gbm_copy_v1, n.trees = 10000)
rmse_copy_v1 = sqrt(mean((pred_copy_v1 - copy_train$price) ^ 2))

print(gbm_copy_v1)
summary(gbm_copy_v1)

#applying to scoringData - gbm fitting model part 1
pred_gbm_copy_v1 = predict(gbm_copy_v1, newdata = scoringData, n.trees = 10000)

#writing csv file for Kaggle submission - predictive model for gbm fitted modeling part 1
submission_d7s2 = data.frame(id = scoringData$id, price = pred_gbm_copy_v1)
write.csv(submission_d7s2, 'submission_d7s2.cs1', row.names = F)

#trying parameters from prior boosting model variant testing with cv folds, etc.
gbm_copy_v2 <-
     gbm(
          formula = price ~ .,
          data = cdata_copy,
          distribution = "gaussian",
          n.trees = 100000,
          interaction.depth = 10,
          shrinkage = 0.001,
          cv.folds = 5,
          n.cores = NULL,
          verbose = FALSE
     )

#viewing gbm fit results
print(gbm_copy_v2)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_copy_v2 = predict(gbm_copy_v2, n.trees = 100000)
rmse_copy_v2 = sqrt(mean((pred_copy_v2 - copy_train$price) ^ 2))

print(gbm_copy_v2)
summary(gbm_copy_v2)

#applying to scoringData - gbm fitting model part 1
pred_gbm_copy_v2 = predict(gbm_copy_v2, newdata = scoringData, n.trees = 'TBD')

#writing csv file for Kaggle submission - predictive model for gbm fitted modeling part 1
submission_d7s3 = data.frame(id = scoringData$id, price = pred_gbm_copy_v2)
write.csv(submission_d7s3, 'submission_d7s3.cs1', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |          [7] XGBoosted Modeling*             |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
# *Note: this section script + xgboosted modeling attempt unsuccessful. Is innoperative. #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#

library(xgboost)
require(xgboost)
require(methods)
library(magrittr)

#re-setting seed and splitting data with seed set to 1984 for reproducibility/testing
set.seed(1984)

#creating data frame for xgboost modeling
xgb_data <- cdata_copy

#converting all integer data types to numeric (apparently xgboosted models require data as all numeric+sparse matrix)
int_data <- lapply(xgb_data, function(x) {
     if (is.integer(x))
          x
     else
          (
               x <- NULL)
})

int_data <- int_data[, -(is.na(int_data))]

str(xgb_data)
str(int_data)

#converting all factor data types to numeric (xgboost req. numeric+sparse matrix--see prior comment)
xgb_data %>% mutate_if(xgb_data, is.factor(x), as.numeric(levels(x))[x])

str(xgb_data)

#clean_data_bst <- cdata_gbmfit_clean

#trying xgboost using clean_xgbst data with verbose 0
clean_xgbst_v1 <-
     xgboost(
          data = clean_data_bst,
          max_depth = 2,
          eta = 1,
          nrounds = 2,
          nthread = 2,
          objective = "binary:logistic",
          verbose = 0
     )

pred_xgbst_v1 <- predict(clean_xgbst_v1, scoringData)

#trying xgboost using clean_xgbst data with verbose 1
clean_xgbst_v2 <-
     xgboost(
          data = clean_data_bst,
          max_depth = 2,
          eta = 1,
          nrounds = 2,
          nthread = 2,
          objective = "binary:logistic",
          verbose = 1
     )

pred_xgbst_v2 <- predict(clean_xgbst_v2, scoringData)

#trying xgboost using clean_xgbst data with verbose 2
clean_xgbst_v3 <-
     xgboost(
          data = clean_data_bst,
          max_depth = 2,
          eta = 1,
          nrounds = 2,
          nthread = 2,
          objective = "binary:logistic",
          verbose = 2
     )

pred_xgbst_v3 <- predict(clean_xgbst_v3, scoringData)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |                                              |                    #
#                    |            [[ USING ADVANCED ]]              |                    #
#                    |           [[ XGBOOST FEATURES ]]             |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#setting seed to 1984 for reproducibility!
set.seed(1984)

split = sample.split(Y = clean_data_bst$price,
                     SplitRatio = 0.7,
                     group = NULL)
train_xgb = clean_data_bst[split, ]
test_xgb = clean_data_bst[!split, ]

#data must be formatted in xgb.DMatrix for adv. feature use
train_xgb_dM <-
     xgb.DMatrix(data = train_xgb$data, label = train_xgb$label)
test_xgb_dM <-
     xgb.DMatrix(data = test_xgb$data, label = test_xgb$label)

clean_xgb_dM <-
     xgb.DMatrix(data = clean_data_bst, label = clean_data_bst$label)
new_xgb_dM <-
     xgb.DMatrix(data = scoringData, label = scoringData$label)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |                                              |                    #
#                    |             [[ USING XGBOOST ]]              |                    #
#                    |           [[ WATCHLIST FEATURE ]]            |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#

#watchlist implemented to monitor resulting evaluation for all data
watchlist_tt <- list(train = train_xgb_dM, test = test_xgb_dM)

xgbst_wl_tt_v1 <-
     xgb.train(
          data = train_xgb_dM,
          max_depth = 2,
          eta = 1,
          nrounds = 2,
          watchlist = watchlist_tt,
          nthread = 2,
          objective = "binary:logistic"
     )

label_wl_tt_v1 = getinfo(test_xgb_dM, "label")

pred_wl_tt_v1 <- predict(xgbst_wl_tt_v1, test_xgb_dM)

imp_matrix_wl_tt_v1 <-
     xgb.importance(feature_names = colnames(train_xgb_dM$data),
                    model = bst)

print(imp_matrix)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#changing and adding multiple watchlist evaluation metrics in xgb training
xgbst_wl_tt_v2 <-
     xgb.train(
          data = train_xgb_dM,
          max_depth = 2,
          eta = 1,
          nrounds = 2,
          watchlist = watchlist_tt,
          eval_metric = "error",
          eval_metric = "logloss",
          nthread = 2,
          objective = "binary:logistic"
     )

label_wl_tt_v2 = getinfo(test_xgb_dM, "label")

pred_wl_tt_v2 <- predict(xgbst_wl_tt_v2, test_xgb_dM)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#                                                                                        #
#                     ______________________________________________                     #
#                    |                                              |                    #
#                    |           ---------------------              |                    #
#                    |             [8] h2o  +  flow                 |                    #
#                    |           ---------------------              |                    #
#                    |                                              |                    #
#                    | Kaggle Contest: Predicting NYC AirBnb Prices |                    #
#                    |                                              |                    #
#                    | Code written by C. Blanchard -- UNI: chb2132 |                    #
#                    |______________________________________________|                    #
#                                                                                        #
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#
#________________________________________________________________________________________#


#loading in required packages, not sure if we need rsparkling for now, so it has been omitted
library(h2o)
library(dplyr)
library(sparklyr)

#setting seed to 1984 for reproducibility
set.seed(1984)

#allocating memory for this instanced initiation of h2o to avoid out of memory errors
h2o.init(max_mem_size = "400g")

#clearing existing/prior h2o workspaces for a clean slate!
h2o.removeAll()

#importing the local analysis and scoring data files with h2o
train1o <-
     h2o.importFile(path = normalizePath("Kaggle Files/analysisData.csv"))
test1o <-
     h2o.importFile(path = normalizePath("Kaggle Files/scoringData.csv"))

#creating h2o objects for each dataset imported with names set to train & test
train <- h2o.assign(train1o, "train.hex")
test <- h2o.assign(test1o, "test.hex")

#creating temp df to remove extra rows created by h2o import of files--these rows lack proper id column values
train2o <- train[!is.na(as.numeric(as.character(train$id))), ]
test2o <- test[!is.na(as.numeric(as.character(test$id))), ]

#creating h2o objects for each corrected dataset, imported in h2o with names set to train_h2o & test_h2o
train_h2o <- h2o.assign(train2o, "train_h2o.hex")
test_h2o <- h2o.assign(test2o, "test_h2o.hex")

#coercing the host_acceptance_rate column in the test df to factor so model will match & run to create prediction
test_h2o$host_acceptance_rate <-
     h2o.asfactor(test_h2o$host_acceptance_rate)

#creating h2o object for the modified training dataset, imported in h2o with name set to test_h2o for use in flow
test_h2o <- h2o.assign(test_h1o, "test_h2o.hex")

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#Note: most modeling was run in the h2o flow through localhost browser interface via Rstudio spark connection,
#meaning the modeling itself does not output anything to the console in RStudio or the history in RStudio
#these are examples as to how modeling can be run via the console/RStudio scripts instead, and show some of the
#parameters that the interface provides when one builds a model, or initiates modeling (ex. Auto_ML)

#trying a first random forest model on training data with h2o via the console interface (RStudio) with params set
rf1 <- h2o.randomForest(
     training_frame = train,
     
     #y indicates the index for our prediction column (here price is column 2, id is 1)
     y = 2,
     
     #normally blank but we can name our model if we want a particular one
     model_id = "rf_covType_v1",
     
     #setting seed (default was -1) for reproducibility, just in case!
     seed = 1984,
     
     #default n.trees in h2o is 50 apparently. Using 100,000 did not work. Lol.
     ntrees = 500,
     
     #stops when avg for 2-trees is within 0.001 of prior averages for 2-trees (sets our convergence)
     stopping_rounds = 2,
     
     #scoring each iteration might give me more information
     score_each_iteration = T
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying a first gradient boosting model on training data with h2o via the console interface (RStudio) with params set
gbm1 <- h2o.gbm(
     training_frame = train,
     
     #y indicates the index for our prediction column (here price is column 2, id is 1)
     y = 2,
     
     #normally blank but we can name our model if we want a particular one
     model_id = "gbm_covType1",
     
     #setting seed (default was -1) for reproducibility, just in case!
     seed = 1984
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying a second gradient boosting model on training data with h2o via the console interface (RStudio) with params set
gbm2 <- h2o.gbm(
     training_frame = train,
     
     #y indicates the index for our prediction column (here price is column 2, id is 1)
     y = 2,
     
     #normally blank but we can name our model if we want a particular one
     model_id = "gbm_covType2",
     
     #setting seed (default was -1) for reproducibility, just in case!
     seed = 1984,
     
     #default n.trees in h2o is 50 apparently. Using 100,000 did not work. Lol.
     ntrees = 50,
     
     #increasing the learning rate
     learn_rate = 0.3,
     
     #setting max_depth to 10 based on the boosting modeling that I was running earlier
     max_depth = 10,
     
     #use a random 70% of the rows to fit each tree
     sample_rate = 0.7,
     
     #use 70% of the columns to fit each tree
     col_sample_rate = 0.7,
     
     #stops when avg for 2-trees is within 0.001 of prior averages for 2-trees (sets our convergence)
     stopping_rounds = 2,
     
     #setting stopping tolerance, might be the default value
     stopping_tolerance = 0.01,
     
     #scoring each iteration might give me more information
     score_each_iteration = T
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#trying a second random forest model on the training data with h2o via the console interface (RStudio) with params set
rf2 <- h2o.randomForest(
     training_frame = train,
     #y indicates the index for our prediction column (here price is column 2, id is 1)
     y = 2,
     
     #normally blank but we can name our model if we want a particular one
     model_id = "rf_covType2",
     
     #setting seed (default was -1) for reproducibility, just in case!
     seed = 1984,
     
     #increasing the number of trees used, default is around 50 I believe
     ntrees = 200,
     
     #our param for rf2 will increase the depth from 20 in earlier rf1 modeling
     max_depth = 30,
     
     #setting tolerance value for stopping/"thresholds for breaks"
     stopping_tolerance = 1e-2,
     
     #stops when avg for 2-trees is within 0.001 of prior averages for 2-trees (sets our convergence)
     stopping_rounds = 2,
     
     #scoring each iteration might give me more information
     score_each_iteration = T,
)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#calculating the prediction values to be used in the submission with the scoringData in h2o
finalRf_predictions <- h2o.predict(object = rf2, newdata = test)
finalgbm_predictions <- h2o.predict(object = gbm2, newdata = test)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

#read in the prediction data from downloaded models after they've been run with predict on the cleaned test dataset
gbm1s0 <- read.csv('gbm1s0.csv')
xgb3 <- read.csv('predxgboost3.csv')
xgb1 <- read.csv('xgb1s0.csv')
xgb_grid_1 <- read.csv('xgb_grid_1.csv')

#set column names to price, as h2o outputs column name as "performance" (I believe) which must be corrected to "price"
colnames(xgb3) <- c("price")
colnames(xgb1) <- c("price")
colnames(gbm1s0) <- c("price")
colnames(xgb_grid_1) <- c("price")

#create data frame for submission with these price predictions and the scoringData id index, write csv file to disk
gbm1s0_submission = data.frame(id = scoringData$id, price = gbm1s0)

write.csv(gbm1s0_submission, 'gbm1s0_submission.csv', row.names = F)

xgb3_submission = data.frame(id = scoringData$id, price = xgb3)

write.csv(xgb3_submission, 'xgb3_submission.csv', row.names = F)

xgb1_submission = data.frame(id = scoringData$id, price = xgb1)

write.csv(xgb1_submission, 'xgb1_submission.csv', row.names = F)

xgb_grid_1_submission = data.frame(id = scoringData$id, price = xgb_grid_1)

write.csv(xgb_grid_1_submission,
          'xgb_grid_1_submission.csv',
          row.names = F)

h2o.shutdown()

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
