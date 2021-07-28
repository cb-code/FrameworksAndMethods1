# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Training Boosted Modeling |

################## Training Boosted Modeling ##################

#ideally we will be able to implement CV and derive our model parameters through the CV model process
#might need to work on the hardware limitations of this computational resource need first, however

library(gbm)

#creating copy of analysis data csv for gbm fit purposes
cdata_gbmfit <- cdata

#converting description data into numeric data given coursework on eBay listings and personal experience with AirBnb rentals
cdata_gbmfit$description = as.numeric(cdata_gbmfit$description)

#inspecting factors for levels over 1024 or 1, removing these columns/vars to clean data for gbm fit
fcdata <- cdata_gbmfit[, sapply(cdata_gbmfit, is.factor)]
fcvect <- sapply(fcdata, nlevels) > 1 & sapply(fcdata, nlevels) < 1025

fcfit <- fcdata[,fcvect]
f2remove <- fcdata[,!fcvect]

names2remove <- names(f2remove)

cdata_gbmfit_clean <- cdata_gbmfit
cdata_gbmfit_clean <- cdata_gbmfit_clean[ , !(names(cdata_gbmfit_clean) %in% names2remove)]

#reducing n.trees and interaction.depth, including cv.folds and n.cores parameters explicitly
#establishing a GBM model for training purposes, trying to determine ideal GBM model conditions
#using all cores by default: n.cores = NULL, 
#using cross-validation folds to detect and ideally reduce bias/overfitting

#re-setting seed and splitting data with seed set to 1984 for reproducibility/testing
set.seed(1984)

gbm_split = sample.split(Y = cdata_gbmfit_clean, SplitRatio = 0.7)
gbm_train = cdata_gbmfit_clean[gbm_split,]
gbm_test = cdata_gbmfit_clean[!gbm_split,]

gbm_fit_v1 <- gbm(formula = price~., data = cdata_gbmfit_clean, distribution = "gaussian",
               n.trees = 10000, interaction.depth = 1, shrinkage = 0.001,
               cv.folds = 5, n.cores = NULL, verbose = FALSE)

#viewing gbm fit results
print(gbm_fit_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_gbm_v1 = predict(gbm_fit_v1, n.trees = 10000)
rmse_gbm_v1 = sqrt(mean((pred_gbm_v1 - gbm_train$price)^2))

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

#----------------SECOND ATTEMPT/APPROACH----------------#

#reducing n.trees and interaction.depth, including cv.folds and n.cores parameters explicitly
#establishing a GBM model for training purposes, trying to determine ideal GBM model conditions
#using all cores by default: n.cores = NULL, 
#using cross-validation folds to detect and ideally reduce bias/overfitting

set.seed(1984)

copy_split = sample.split(Y = cdata_copy, SplitRatio = 0.7)
copy_train = cdata_copy[copy_split,]
copy_test = cdata_copy[!copy_split,]

gbm_copy_v1 <- gbm(formula = price~., data = cdata_copy, distribution = "gaussian",
                  n.trees = 10000, interaction.depth = 1, shrinkage = 0.001,
                  cv.folds = 5, n.cores = NULL, verbose = FALSE)

#viewing gbm fit results
print(gbm_copy_v1)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_copy_v1 = predict(gbm_copy_v1, n.trees = 10000)
rmse_copy_v1 = sqrt(mean((pred_copy_v1 - copy_train$price)^2))

print(gbm_copy_v1)
summary(gbm_copy_v1)

#applying to scoringData - gbm fitting model part 1
pred_gbm_copy_v1 = predict(gbm_copy_v1, newdata = scoringData, n.trees = 10000)

#writing csv file for Kaggle submission - predictive model for gbm fitted modeling part 1
submission_d7s2 = data.frame(id = scoringData$id, price = pred_gbm_copy_v1)
write.csv(submission_d7s2, 'submission_d7s2.cs1', row.names = F)

#trying parameters from prior boosting model variant testing with cv folds, etc.
gbm_copy_v2 <- gbm(formula = price~., data = cdata_copy, distribution = "gaussian",
                   n.trees = 100000, interaction.depth = 10, shrinkage = 0.001,
                   cv.folds = 5, n.cores = NULL, verbose = FALSE)

#viewing gbm fit results
print(gbm_copy_v2)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")

pred_copy_v2 = predict(gbm_copy_v2, n.trees = 100000)
rmse_copy_v2 = sqrt(mean((pred_copy_v2 - copy_train$price)^2))

print(gbm_copy_v2)
summary(gbm_copy_v2)

#applying to scoringData - gbm fitting model part 1
pred_gbm_copy_v2 = predict(gbm_copy_v2, newdata = scoringData, n.trees = 'TBD')

#writing csv file for Kaggle submission - predictive model for gbm fitted modeling part 1
submission_d7s3 = data.frame(id = scoringData$id, price = pred_gbm_copy_v2)
write.csv(submission_d7s3, 'submission_d7s3.cs1', row.names = F)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
