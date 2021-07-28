model1 = lm(price~sqft_living, data = train)
summary(model1)
pred = predict(model1)
data.frame(price = train$price[100:109], prediction = pred[100:109])
sse1 = sum((pred-train$price)^2)
sst1 = sum((mean(train$price)-train$price)^2)
model1_r2 = 1-sse1/sst1
model1_r2
rmse1 = sqrt(mean((pred-train$price)^2))

model2 = lm(price~waterfront, data = train)
summary(model2)
pred = predict(model2)
data.frame(price = train$price[100:109], prediction = pred[100:109])
sse2 = sum((pred-train$price)^2)
sst2 = sum((mean(train$price)-train$price)^2)
model2_r2 = 1 - sse2/sst2
model2_r2
rmse2 = sqrt(mean((pred-train$price)^2))
model3 = lm(price~sqft_living+waterfront, data = train)
summary(model3)
pred = predict(model3)
sse3 = sum((pred-train$price)^2)
sst3 = sum((mean(train$price)-train$price)^2)
model3_r2 = 1 - sse3/sst3
model3_r2
rmse3 = sqrt(mean((pred-train$price)^2))
predict(model2, newdata = data.frame(sqft_living = mean(train$sqft_living))

model4 =
lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,
data = train)
summary(model4)
pred = predict(model4)
sse4 = sum((pred-train$price)^2)
sst4 = sum((mean(train$price)-train$price)^2)
model4_r2 = 1 - sse4/sst4
model4_r2
rmse4 = sqrt(mean((pred-train$price)^2))

model5 =
lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,
data = test)
summary(model5)
pred = predict(model5)

sse5 = sum((pred-test$price)^2)
sst5 = sum((mean(test$price)-test$price)^2)
model5_r2 = 1 - sse5/sst5
rmse5 = sqrt(mean((pred-test$price)^2))
model5_r2
