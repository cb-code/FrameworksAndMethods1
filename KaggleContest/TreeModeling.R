# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Tree Modeling |

################## Tree Modeling ##################

#prediction method using particular variables: tree modeling part 1
modeltree = rpart(price ~ accommodates + minimum_maximum_nights + 
                       neighbourhood_group_cleansed + bathrooms + 
                       calculated_host_listings_count + availability_90 + room_type + 
                       number_of_reviews + review_scores_location + review_scores_value + 
                       bedrooms + review_scores_rating + minimum_nights, data = cdata_copy)

predTree = predict(modeltree, newdata = scoringData)

#prediction method using particular variables for train data: tree modeling part 1.5
trainTree = rpart(price ~ accommodates + minimum_maximum_nights + 
                       +                  neighbourhood_group_cleansed + bathrooms + 
                       +                  calculated_host_listings_count + availability_90 + room_type + 
                       +                  number_of_reviews + review_scores_location + review_scores_value + 
                       +                  bedrooms + review_scores_rating + minimum_nights, data = cTrain_copy)

predtrainTree = predict(tree, newdata = cTrain_copy)
predtestTree = predict(tree, newdata = cTest_copy)

rmsetrainTree = sqrt(mean((predtrainTree - cTrain_copy$price)^2))
rmsetestTree = sqrt(mean((predtestTree - cTest_copy$price)^2))

#prediction method using particular variables: attempted cv modeling part 1 *Edit Notation for this*
trControl = trainControl(method="cv", number = 10)
tuneGrid = expand.grid(.cp = seq(0,0.1,0.001))

cvModel = train(price~., data = cTrain_copy, method = "rpart", trControl = trControl, tuneGrid = tuneGrid)
cvModel$bestTune

treeCV = rpart(price~., data = cTrain_copy, control = rpart.control(cp = cvModel$bestTune))

predTreeCV = predict(treeCV, newdata = cTest_copy)

rmseCV = sqrt(mean((predTreeCV - cTest_copy$price)^2))

#prediction method using particular variables: rpart/tree modeling part 1
modelPtree = rpart(price ~ accommodates + minimum_maximum_nights + 
                       +                       neighbourhood_group_cleansed + bathrooms + 
                       +                       calculated_host_listings_count + availability_90 + room_type + 
                       +                       number_of_reviews + review_scores_location + review_scores_value + 
                       +                       bedrooms + review_scores_rating + minimum_nights, data = cdata_copy)

#applying to scoringData - predictive model for rpart generated tree 1
predPTree = predict(modelPtree, newdata = scoringData)

#writing csv file for Kaggle submission - predictive model for rpart generated tree 1
submission_d4s1_modelt = data.frame(id = scoringData$id, price = predTree)
write.csv(submission_d4s1_modelt, 'submission_d4s1_modelt.csv', row.names = F)

#prediction method using particular variables: Random Forest modeling part 1
forest1 = randomForest(price ~ accommodates + minimum_maximum_nights + 
                            neighbourhood_group_cleansed + bathrooms +
                            calculated_host_listings_count + availability_90 + room_type + 
                            number_of_reviews + review_scores_location +
                            bedrooms + review_scores_rating + minimum_nights, data = cdata_copy, ntree = 100, na.action = na.exclude)

#applying to scoringData - predictive model for Random Forest modeling 1
predForest1 = predict(forest1, newdata = scoringData)
rmseForest1 = sqrt(mean((predForest1 - scoringData$price)^2))

#writing csv file for Kaggle submission - predictive model for Random Forest modeling 1
submission_d3s2 = data.frame(id = scoringData$id, price = predForest1)
write.csv(submission_d3s2, 'submission_d3s2.csv', row.names = F)

#prediction method using particular variables: Random Forest modeling part 2
forest2 = randomForest(price ~ accommodates + minimum_maximum_nights + 
                            neighbourhood_group_cleansed + bathrooms +
                            calculated_host_listings_count + availability_90 + room_type + 
                            number_of_reviews + review_scores_location +
                            bedrooms + review_scores_rating + minimum_nights, 
                       data = cdata_copy, ntree = 750, na.action = na.exclude)

#applying to scoringData - predictive model for Random Forest modeling 2
predForest2 = predict(forest2, newdata = scoringData)
rmseForest2 = sqrt(mean((predForest2 - scoringData$price)^2))

#writing csv file for Kaggle submission - predictive model for Random Forest modeling 2
submission_d3s3 = data.frame(id = scoringData$id, price = predForest2)
write.csv(submission_d3s3, 'submission_d3s3.csv', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
