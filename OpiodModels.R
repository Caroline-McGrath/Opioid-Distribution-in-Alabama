


## save data frame 
nodrugs 
write.xlsx(nodrugs,"C:\\Users\\Pat\\Documents\\nodrugs.xlsx")


library(readxl)
nodrugs <- read_xlsx("C:\\Users\\Pat\\Documents\\nodrugs.xlsx")
modelData <- nodrugs


#append  mme and total doses from original data



final <- read_xlsx("C:\\Users\\Pat\\Documents\\final(6.0).xlsx")
modelData$totalDoses <- final$totalDoses
modelData <- modelData[,2:37]



## linear model

linear <- lm(totalDoses ~. - PSZ -ACS_PCT_POP_SAME_SEX_UNMRD_P, data = modelData)
summary(linear)


## Stepwise regression 


train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.step.model <- train(totalDoses ~. -PSZ, data = modelData, method = "lmStepAIC",
                            trControl = train.control, trace = FALSE)

summary(trained.step.model$finalModel)

par(mfrow=c(2,2))
plot(trained.step.model$finalModel)


## stepwise regression on MME

modelData2 <- nodrugs
modelData2$MME <- final$MME

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.step.model <- train(MME ~. -PSZ -ACS_PCT_POP_SAME_SEX_UNMRD_P, data = modelData2, method = "lmStepAIC",
                            trControl = train.control, trace = FALSE)

summary(trained.step.model$finalModel)

par(mfrow=c(2,2))
plot(trained.step.model$finalModel)


## Stepwise regression on MOUD

modelData3 <- nodrugs
modelData3$MOUD <- final$PctAddiction
modelData3 <- na.omit(modelData3)

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.step.model <- train(MOUD ~. -PSZ -ACS_PCT_POP_SAME_SEX_UNMRD_P, data = modelData3, method = "lmStepAIC",
                            trControl = train.control, trace = FALSE)

summary(trained.step.model$finalModel)

par(mfrow=c(2,2))
plot(trained.step.model$finalModel)



## Stepwise regression on nonMOUD

modelData4 <- nodrugs
modelData4$nonMOUD <- final$PctPain
modelData4 <- na.omit(modelData4)

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.step.model <- train(nonMOUD ~. -PSZ -ACS_PCT_POP_SAME_SEX_UNMRD_P, data = modelData4, method = "lmStepAIC",
                            trControl = train.control, trace = FALSE)

summary(trained.step.model$finalModel)

par(mfrow=c(2,2))
plot(trained.step.model$finalModel)



## Education Model

library(caret)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.model <- train(totalDoses ~ ACS_PCT_BACHELOR_DGR + ACS_PCT_HS_GRADUATE + ACS_PCT_LT_HS , data = modelData, method = "lm",
                            trControl = train.control, trace = FALSE)

summary(trained.model$finalModel)
AIC(trained.model$finalModel)




## Neighborhood

trained.model <- train(totalDoses ~ ACS_PCT_10UNITS + ACS_PCT_HU_MOBILE_HOME + ACS_MEDIAN_HOME_VALUE + ACS_MEDIAN_YEAR_BUILT + ACS_PCT_HU_NO_VEH +
                         ACS_PCT_PUBL_TRANSIT + ACS_PCT_WALK_2WORK, data = modelData, method = "lm",
                       trControl = train.control, trace = FALSE)

summary(trained.model$finalModel)
AIC(trained.model$finalModel)




## Social factors

trained.model <- train(totalDoses ~ ACS_PCT_POP_SAME_SEX_SPOUSE + ACS_PCT_DISABLE + ACS_PCT_DIVORCED + ACS_PCT_HH_LIMIT_ENGLISH, 
                       data = modelData, method = "lm",
                       trControl = train.control, trace = FALSE)

summary(trained.model$finalModel)
AIC(trained.model$finalModel)



## Economic factors

trained.model <- train(totalDoses ~ ACS_PCT_EMPLOYED + ACS_GINI_INDEX + ACS_PER_CAPITA_INC + ACS_PCT_HH_FOOD_STMP + 
                         ACS_MEDIAN_HH_INC, 
                       data = modelData, method = "lm",
                       trControl = train.control, trace = FALSE)

summary(trained.model$finalModel)
AIC(trained.model$finalModel)

##



train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trained.step.model <- train(totalDoses ~., data = modelData, method = "lmStepAIC",
                            trControl = train.control, trace = FALSE)








