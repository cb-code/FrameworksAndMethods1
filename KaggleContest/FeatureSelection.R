# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Feature Selection |

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

index_na_keep = colSums(is.na(data)) <= nrow(data)*0.6
cdata = data[,index_na_keep]

index_na = colSums(is.na(cdata))
index_na = as.logical(index_na)
index_na = cdata[,index_na]

index_na_use = index_na[,-c(1:4, 7)]

#creating train and test data with seed value 1984
set.seed(1984)
split = sample.split(Y = cdata$price, SplitRatio = 0.7, group = NULL)
train = cdata[split,]
test = cdata[!split,]

means = lapply(cdata, mean, na.rm = TRUE)

NA_means = c(means$bathrooms, means$bedrooms, means$review_scores_accuracy, 
             means$review_scores_cleanliness, means$review_scores_checkin, 
             means$review_scores_communication, means$review_scores_location, 
             means$review_scores_value)

cdata$bathrooms[is.na(cdata$bathrooms)] <- means$bathrooms
cdata$bedrooms[is.na(cdata$bedrooms)]  <- means$bedrooms
cdata$review_scores_accuracy[is.na(cdata$review_scores_accuracy)] <- means$review_scores_accuracy
cdata$review_scores_cleanliness[is.na(cdata$review_scores_cleanliness)] <- means$review_scores_cleanliness
cdata$review_scores_checkin[is.na(cdata$review_scores_checkin)] <- means$review_scores_checkin
cdata$review_scores_communication[is.na(cdata$review_scores_communication)] <- means$review_scores_communication
cdata$review_scores_location[is.na(cdata$review_scores_location)] <- means$review_scores_location
cdata$review_scores_value[is.na(cdata$review_scores_value)] <- means$review_scores_value

hostverificationtrue = subset(cdata, cdata$host_verifications == "true")
hostpictrue = subset(cdata, cdata$host_has_profile_pic == "true")
hostidtrue = subset(cdata, cdata$host_id == "true")

guestpictrue = subset(cdata, cdata$require_guest_profile_picture == "true")
guestphonetrue = subset(cdata, cdata$require_guest_phone_verification == "true")

licensetrue = subset(cdata, cdata$requires_license == "true")

levels(cdata$security_deposit)[levels(cdata$security_deposit)==""] <- "$0.00"

round(mean(as.numeric(cdata$cleaning_fee)),2)
levels(cdata$cleaning_fee)[levels(cdata$cleaning_fee)==""] <- "$75.63"

spectrum = colorRampPalette(c("blue", "white", "red")) (20)
heatmap(x = ndata.cor, col = spectrum, symm = TRUE)

numSplit = createDataPartition(y = numData$price, p = 0.7, list = F, groups = 100)
numTrain = numData[split,]
numTest = numData[-split,]

cor(numTrain[,-24])
col2 = colorRampPalette(c('#d73027', '#fc8d59', '#fee08b', '#ffffbf', '#d9ef8b', '#91cf60', '#1a9850'))
corrplot(cor(numTrain[,-24]), method = 'square', type = 'lower', diag = F, col = col2(7))

#cleaning up the data we will use -- possible that removed vars here will be restored later
cdata_copy = cdata[,-c(1, 3:6, 8:20, 24:32, 34:43, 49, 52:56, 58, 61:62, 64:70, 72:73, 75:77, 79:82, 84:92, 94:97)]

cdata_copy = cdata_copy[,-(3:4)]
cdata_copy = cdata_copy[,-11]

#converting description data into numeric data given coursework on eBay listings and personal experience with AirBnb rentals
cdata_copy$description = as.numeric(cdata_copy$description)

#creating train and test data from copy of subsetted data with seed value 1984
set.seed(1984)
cSplit_copy = sample.split(Y = cdata_copy, SplitRatio = 0.7)
cTrain_copy = cdata_copy[cSplit_copy,]
cTest_copy = cdata_copy[!cSplit_copy,]

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
