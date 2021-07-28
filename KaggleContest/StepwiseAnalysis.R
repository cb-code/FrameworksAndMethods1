# | APAN S5200 | Frameworks & Methods I | Kaggle Contest |
# | Columbia University SPS | Spring 2020 | chb2132 |
# | Predicting NYC AirBnb Prices | Stepwise Analysis |

################## Stepwise Analysis ##################

#feature selection method with particular variables: Stepwise analysis part 1 [Forward]
start_modf = lm(price~1, data = cTrain)
empty_modf = lm(price~1, data = cTrain)
full_modf = lm(price~., data = cTrain)

forwardStepwise = step(start_modf,
                       scope = list(upper = full_modf, lower = empty_modf),
                       direction = 'forward')

summary(forwardStepwise)

#feature selection method with particular variables: Stepwise analysis part 2 [Backward]
start_modb = lm(price~., data = cTrain)
empty_modb = lm(price~1, data = cTrain)
full_modb = lm(price~., data = cTrain)

backwardStepwise = step(start_modb,
                        scope = list(upper = full_modb, lower = empty_modb),
                        direction = 'backward')

summary(backwardStepwise)

#feature selection method with particular variables: Stepwise analysis part 3 [Hybrid]
start_modv = lm(price~1, data = cTrain)
empty_modv = lm(price~1, data = cTrain)
full_modv = lm(price~., data = cTrain)

hybridStepwise = step(start_modv,
                      scope = list(upper = full_modv, lower = empty_modv),
                      direction = 'both')

summary(hybridStepwise)

#feature selection method with particular variables: Stepwise analysis part 4 [Hybrid - cleaned/modified data]
start_modv2 = lm(price~1, data = cdata_copy)
empty_modv2 = lm(price~1, data = cdata_copy)
full_modv2 = lm(price~., data = cdata_copy)

hybridStepwisev2 = step(start_modv2,
                        scope = list(upper = full_modv2, lower = empty_modv2),
                        direction = 'both')

summary(hybridStepwisev2)

#feature selection method with particular variables: Stepwise analysis part 5 [Hybrid - cleaned/modified data]
start_modv3 = lm(price~1, data = cdata_copy)
empty_modv3 = lm(price~1, data = cdata_copy)
full_modv3 = lm(price~., data = cdata_copy)

hybridStepwisev3 = step(start_modv3,
                        scope = list(upper = full_modv3, lower = empty_modv3),
                        direction = 'both')

summary(hybridStepwisev3)

#play a user interface alert when R code completes
system("say -v Magnus Code Complete!")
