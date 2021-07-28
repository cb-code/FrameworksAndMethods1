#trying increased interaction.depth + check gbm.perf to attempt boosting iteration optimization
#prediction method on boost_data with gbm.perf post-test analysis - boosting iteration optimization part 1
boost_copy_v7 = gbm(formula = price ~., data = boost_data, distribution = "gaussian", 
                    n.trees = 100000, interaction.depth = 12, shrinkage = 0.001)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

predBoostTrain_copy_v7 = predict(boost_copy_v7, n.trees = 100000)
rmseBoostTrain_copy_v7 = sqrt(mean((predBoostTrain_copy_v7 - boost_train$price)^2))
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
ogbm_v1 = gbm(formula = price ~., data = boost_data, cv.folds = 10, verbose = TRUE,  distribution = "gaussian", 
                    n.trees = 100000, interaction.depth = 10, shrinkage = 0.001)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

train_ogbm_v1 = predict(ogbm_v1, n.trees = 100000)
rmse_train_ogbm_v1 = sqrt(mean((train_ogbm_v1 - boost_train$price)^2))
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
ogbm_v1 = gbm(formula = price ~., data = boost_data, cv.folds = 10, verbose = TRUE,  distribution = "gaussian", 
              n.trees = 100000, interaction.depth = 10, shrinkage = 0.001)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

train_ogbm_v1 = predict(ogbm_v1, n.trees = 100000)
rmse_train_ogbm_v1 = sqrt(mean((train_ogbm_v1 - boost_train$price)^2))
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
