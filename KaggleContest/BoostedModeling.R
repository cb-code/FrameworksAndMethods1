# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Boosted Modeling |

################## Boosted Modeling ##################

set.seed(1984)

boost_data <- cdata_copy

boost_split = sample.split(Y = boost_data, SplitRatio = 0.7)
boost_train = boost_data[boost_split,]
boost_test = boost_data[!boost_split,]

#prediction method using cdata_copy variables: boosted modeling part 1
boost_copy = gbm(formula = formula = price ~., data = boost_data, distribution = "gaussian", 
                 n.trees = 100000, interaction.depth = 3, shrinkage = 0.001)

#applying to scoringData - predictive model for boosted modeling part 1
predBoostTrain_copy = predict(boost_train, n.trees = 100000)
rmseBoostTrain_copy = sqrt(mean((predBoostTrain_copy - boost_train$price)^2))
summary(boost_copy)

predboost_copy = predict(boost_copy, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for Boosted Modeling 1
submission_d4s2 = data.frame(id = scoringData$id, price = predboost_copy)
write.csv(submission_d4s2, 'submission_d4s2.csv', row.names = F)

#could not complete after running for over 6 hours on laptop...sad
trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(n.trees = 1000, interaction.depth = c(1,2), 
                       shrinkage = (1:100)*0.001, n.minobsinnode = 5)
garbage = capture.output(cvBoost <- train(formula = price ~., data = boost_data, method = "gbm", 
                                          trControl = trControl, tuneGrid = tuneGrid, na.action=na.exclude))

boostCV = gbm(formula = price ~ accommodates + minimum_maximum_nights + 
                   neighbourhood_group_cleansed + bathrooms +
                   calculated_host_listings_count + availability_90 + room_type + 
                   number_of_reviews + review_scores_location +
                   bedrooms + review_scores_rating + minimum_nights, 
              data = boost_data, distribution = "gaussian", n.trees = cvBoost$bestTune$n.trees,
              interaction.depth = cvBoost$bestTune$interaction_depth,
              shrinkage = cvBoost$bestTune$shrinkage,
              n.minobsinnode = cvBoost$bestTune$n.minobsinnode)

predBoostCV = predict(boostCV, scoringData, n.trees = 1000)
rmseBoostCV = sqrt(mean((predBoostCV - scoringData$price)^2, na.action = na.exclude))

#trying greater interaction depth
#prediction method on boost_data: boosted modeling part 2
boost_copy_v1 = gbm(formula = price ~ accommodates + minimum_maximum_nights + 
                         neighbourhood_group_cleansed + bathrooms +
                         calculated_host_listings_count + availability_90 + room_type + 
                         number_of_reviews + review_scores_location +
                         bedrooms + review_scores_rating + minimum_nights, 
                    data = boost_data, distribution = "gaussian",
                    n.trees = 100000, interaction.depth = 5, shrinkage = 0.001)

predBoostTrain_copy_v1 = predict(boost_copy_v1, n.trees = 100000)
rmseBoostTrain_copy_v1 = sqrt(mean((predBoostTrain_copy_v1 - boost_train$price)^2))
summary(boost_copy_v1)

#applying to scoringData - predictive model for boosted modeling part 2
predboost_copy_v1 = predict(boost_copy_v1, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 2
submission_d4s3 = data.frame(id = scoringData$id, price = predboost_copy_v1)
write.csv(submission_d4s3, 'submission_d4s3.csv', row.names = F)

#instead of specifying particular variables within the subsetted/cleaned boost_data,
#model will be run on the entirety of the boost_data subset variant (gbm(formula = price~.)...)
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 3
boost_copy_v2 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 100000, interaction.depth = 5, shrinkage = 0.001)

predBoostTrain_copy_v2 = predict(boost_copy_v2, n.trees = 100000)
rmseBoostTrain_copy_v2 = sqrt(mean((predBoostTrain_copy_v2 - boost_train$price)^2))
summary(boost_copy_v2)

#applying to scoringData - predictive model for boosted modeling part 3
predboost_copy_v2 = predict(boost_copy_v2, newdata = scoringData, n.trees = 100000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 3
submission_d5s1 = data.frame(id = scoringData$id, price = predboost_copy_v2)
write.csv(submission_d5s1, 'submission_d5s1.csv', row.names = F)

#trying greater n.trees, lowering interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 4
boost_copy_v3 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 150000, interaction.depth = 3, shrinkage = 0.001)

predBoostTrain_copy_v3 = predict(boost_copy_v3, n.trees = 150000)
rmseBoostTrain_copy_v3 = sqrt(mean((predBoostTrain_copy_v3 - boost_train$price)^2))
summary(boost_copy_v3)

#applying to scoringData - predictive model for boosted modeling part 4
predboost_copy_v3 = predict(boost_copy_v3, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 4
submission_d5s2 = data.frame(id = scoringData$id, price = predboost_copy_v3)
write.csv(submission_d5s2, 'submission_d5s2.csv', row.names = F)

#trying greater n.trees, increased interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 5
boost_copy_v4_s2 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                       n.trees = 150000, interaction.depth = 5, shrinkage = 0.001)

predBoostTrain_copy_v4_s2 = predict(boost_copy_v4_s2, n.trees = 150000)
rmseBoostTrain_copy_v4_s2 = sqrt(mean((predBoostTrain_copy_v4_s2 - boost_train_s2$price)^2))
summary(boost_copy_v4_s2)

#applying to scoringData - predictive model for boosted modeling part 5
predboost_copy_v4_s2 = predict(boost_copy_v4_s2, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 5
submission_d5s3 = data.frame(id = scoringData$id, price = predboost_copy_v4_s2)
write.csv(submission_d5s3, 'submission_d5s3.csv', row.names = F)

#trying a new seed value and splitting data given time interval between boosting models
set.seed(617)
cSplit_copy_s2 = sample.split(Y = cdata_copy, SplitRatio = 0.7)
boost_train_s2 = cdata_copy[cSplit_copy_s2,]
cTest_copy_s2 = cdata_copy[!cSplit_copy_s2,]

#using different data for testing/training, set seed value to 617
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 6
boost_copy_v4 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 150000, interaction.depth = 5, shrinkage = 0.001)

predBoostTrain_copy_v4 = predict(boost_copy_v4, n.trees = 150000)
rmseBoostTrain_copy_v4 = sqrt(mean((predBoostTrain_copy_v4 - boost_train$price)^2))
summary(boost_copy_v4)

#applying to scoringData - predictive model for boosted modeling part 6
predboost_copy_v4 = predict(boost_copy_v4, newdata = scoringData, n.trees = 150000)

#writing csv file for Kaggle submission - predictive model for boosted modeling part 6
submission_d6s1 = data.frame(id = scoringData$id, price = predboost_copy_v4)
write.csv(submission_d6s1, 'submission_d6s1.csv', row.names = F)

#re-setting seed and splitting data in case of any changes from prior seed choices, set seed to 1984
set.seed(1984)
cSplit_copy = sample.split(Y = cdata_copy, SplitRatio = 0.7)
boost_train = cdata_copy[cSplit_copy,]
cTest_copy = cdata_copy[!cSplit_copy,]

#trying reduced n.trees, increased interaction.depth
#estimating price with all variables in boost_data subset - predictive model for boosted modeling part 7
boost_copy_v5 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 100000, interaction.depth = 7, shrinkage = 0.001)

predBoostTrain_copy_v5 = predict(boost_copy_v5, n.trees = 100000)
rmseBoostTrain_copy_v5 = sqrt(mean((predBoostTrain_copy_v5 - boost_train$price)^2))
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
boost_copy_v6 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 100000, interaction.depth = 10, shrinkage = 0.001)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

predBoostTrain_copy_v6 = predict(boost_copy_v6, n.trees = 100000)
rmseBoostTrain_copy_v6 = sqrt(mean((predBoostTrain_copy_v6 - boost_train$price)^2))
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
