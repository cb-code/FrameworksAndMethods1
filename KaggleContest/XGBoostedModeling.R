#Note: this attempted script and xgboosted modeling approach was not ultimately successful#

# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | XGBoosted Modeling |

################## XGBoosted Modeling ##################

library(xgboost)
require(xgboost)
require(methods)
library(magrittr)

#re-setting seed and splitting data with seed set to 1984 for reproducibility/testing
set.seed(1984)

#creating data frame for xgboost modeling
xgb_data <- cdata_copy

#converting all integer data types to numeric (apparently xgboosted models require data as all numeric+sparse matrix)
int_data <- lapply(xgb_data, function(x){
     if(is.integer(x)) x else(
          x <- NULL)
     })

int_data <- int_data[,-(is.na(int_data))]

str(xgb_data)
str(int_data)

#converting all factor data types to numeric (xgboost req. numeric+sparse matrix--see prior comment)
xgb_data %>% mutate_if(xgb_data, is.factor(x), as.numeric(levels(x))[x])

str(xgb_data)

#clean_data_bst <- cdata_gbmfit_clean

#trying xgboost using clean_xgbst data with verbose 0
clean_xgbst_v1 <- xgboost(data = clean_data_bst, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 0)

pred_xgbst_v1 <- predict(clean_xgbst_v1, scoringData)

#trying xgboost using clean_xgbst data with verbose 1
clean_xgbst_v2 <- xgboost(data = clean_data_bst, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 1)

pred_xgbst_v2 <- predict(clean_xgbst_v2, scoringData)

#trying xgboost using clean_xgbst data with verbose 2
clean_xgbst_v3 <- xgboost(data = clean_data_bst, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 2)

pred_xgbst_v3 <- predict(clean_xgbst_v3, scoringData)

#--------------USING ADVANCED XGBOOST FEATURES--------------#

#for reproducibility!
set.seed(1984)

split = sample.split(Y = clean_data_bst$price, SplitRatio = 0.7, group = NULL)
train_xgb = clean_data_bst[split,]
test_xgb = clean_data_bst[!split,]

#data must be formatted in xgb.DMatrix for adv. feature use
train_xgb_dM <- xgb.DMatrix(data = train_xgb$data, label=train_xgb$label)
test_xgb_dM <- xgb.DMatrix(data = test_xgb$data, label=test_xgb$label)

clean_xgb_dM <- xgb.DMatrix(data = clean_data_bst, label = clean_data_bst$label)
new_xgb_dM <- xgb.DMatrix(data = scoringData, label = scoringData$label)

#--------------USING WATCHLIST FEATURE IN XGBOOST--------------#

#watchlist implemented to monitor resulting evaluation for all data
watchlist_tt <- list(train=train_xgb_dM, test=test_xgb_dM)

xgbst_wl_tt_v1 <- xgb.train(data=train_xgb_dM, max_depth=2, eta=1, nrounds=2, watchlist=watchlist_tt,
                 nthread = 2, objective = "binary:logistic")

label_wl_tt_v1 = getinfo(test_xgb_dM, "label")

pred_wl_tt_v1 <- predict(xgbst_wl_tt_v1, test_xgb_dM)

imp_matrix_wl_tt_v1 <- xgb.importance(feature_names = colnames(train_xgb_dM$data), model = bst)

print(imp_matrix)

#changing and adding multiple watchlist evaluation metrics in xgb training
xgbst_wl_tt_v2 <- xgb.train(data=train_xgb_dM, max_depth=2, eta=1, nrounds=2, watchlist=watchlist_tt,
                 eval_metric = "error", eval_metric = "logloss",
                 nthread = 2, objective = "binary:logistic")

label_wl_tt_v2 = getinfo(test_xgb_dM, "label")

pred_wl_tt_v2 <- predict(xgbst_wl_tt_v2, test_xgb_dM)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
