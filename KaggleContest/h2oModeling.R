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
train2o <- train[!is.na(as.numeric(as.character(train$id))),]
test2o <- test[!is.na(as.numeric(as.character(test$id))),]

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
