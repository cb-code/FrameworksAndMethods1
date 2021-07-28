# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Linear Modeling |

################## Linear Modeling ##################

#prediction method using particular variables: linear modeling part 1
modelv = lm(formula = price ~ accommodates + minimum_maximum_nights + 
                 +                 room_type + neighbourhood_group_cleansed + maximum_nights + 
                 +                 bathrooms + bedrooms + calculated_host_listings_count + 
                 +                 availability_365 + review_scores_location + review_scores_communication + 
                 +                 host_response_time + review_scores_cleanliness + review_scores_accuracy + 
                 +                 review_scores_value + number_of_reviews + minimum_nights + 
                 +                 maximum_minimum_nights, data = cdata_copy)

predv = predict(modelv, newdata = scoringData)

predt = predict(modelv, newdata = cTrain_copy)

sse_predt = sum((predt - cTrain_copy$price)^2)
sst_predt = sum((mean(cTrain_copy$price)-cTrain_copy$price)^2)

modelt_r2 = 1 - sse_predt/sst_predt
rmset = sqrt(mean((predt-cTrain_copy$price)^2))

#prediction method using particular variables: linear modeling part 2
modelv2 = lm(formula = price ~ 
                  host_is_superhost + as.numeric(description)
             + room_type + bathrooms + bedrooms + bed_type
             + accommodates + guests_included + 
                  + minimum_nights + minimum_maximum_nights + availability_90 +
                  + review_scores_location + review_scores_value + review_scores_rating 
             + number_of_reviews + calculated_host_listings_count + neighbourhood_group_cleansed,
             data = cdata_copy)

predv2 = predict(modelv2, newdata = scoringData)

#trying modelv2 on copy_data (cleaned/modified) for train data - prediction method: linear modeling part 2.5
predt2 = predict(modelv2, newdata = cTrain_copy)

sse_predt2 = sum((predt2 - cTrain_copy$price)^2)
sst_predt2 = sum((mean(cTrain_copy$price)-cTrain_copy$price)^2)

modelt2_r2 = 1 - sse_predt2/sst_predt2
rmset2 = sqrt(mean((predt2-cTrain_copy$price)^2))

#To Fix:
#    Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#         Factor extra_people has new levels 
#         $102.00, $109.00, $167.00, $176.00, $180.00, $195.00, $42.00, $46.00, $63.00, $79.00, $82.00, $94.00

#feature selection method with particular variables: linear modeling part 3
modelv3 = lm(formula = price ~ accommodates + minimum_maximum_nights + 
                  neighbourhood_group_cleansed + bathrooms +
                  calculated_host_listings_count + availability_90 + room_type + 
                  number_of_reviews + review_scores_location + review_scores_value + 
                  bedrooms + review_scores_rating + minimum_nights, data = cdata_copy)

predv3 = predict(modelv3, newdata = scoringData)

#trying modelv3 on copy_data (cleaned/modified) for train data - prediction method: linear modeling part 3.5
predt3 = predict(modelv3, newdata = cTrain_copy)

sse_predt3 = sum((predt3 - cTrain_copy$price)^2)
sst_predt3 = sum((mean(cTrain_copy$price)-cTrain_copy$price)^2)

modelt3_r2 = 1 - sse_predt3/sst_predt3
rmset3 = sqrt(mean((predt3-cTrain_copy$price)^2))

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
