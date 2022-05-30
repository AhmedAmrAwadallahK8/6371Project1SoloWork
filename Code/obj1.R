#Objective 1: MLR Model
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(car)
library(glmnet)
########Use averaging
get_train_test_list = function(df, splitPercent){
  dfRowIndices = 1:dim(df)[1]
  dfRowSize = dim(df)[1]
  
  sampleSize = round(splitPercent * dfRowSize)
  trainIndices = sample(dfRowIndices, sampleSize)
  testIndices = -trainIndices
  
  train = df[trainIndices,]
  test = df[testIndices,]
  
  return(list(train, test))
}

#Variables with more than 5% NA Values
  #Total.expenditure
  #Schooling
  #Population
  #Income.composition.of.resources
  #Hepatitis.B
  #GDP
  #Alcohol

#Variables with less than 5% NA Values
  #thinness.5.9.years
  #thinness..1.19.years
  #Polio
  #Life.expectancy
  #Diphtheria
  #BMI
  #Adult.Mortality
#Extract Data
LifeExpecFilePath = "../Datasets/LifeExpectancyData.csv"
LifeExpecRaw<-read.csv(LifeExpecFilePath)

#Remove all features with NA values
variablesWithNa = c("Total.expenditure", "Schooling", "Population", "Income.composition.of.resources",
                    "Hepatitis.B", "GDP", "Alcohol", "thinness.5.9.years", "thinness..1.19.years", 
                    "Polio", "Diphtheria", "Adult.Mortality","Country", "BMI")
LifeExpecObj1Clean1 = LifeExpecRaw %>% select(-variablesWithNa)
LifeExpecObj1Clean1$Status = as.factor(LifeExpecObj1Clean1$Status)

#Out target value has some NAs, remove rows where this is NA
LifeExpecObj1Clean1 = LifeExpecObj1Clean1 %>% filter(!is.na(Life.expectancy))

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecObj1Clean1, splitPercent)
trainIndex = 1
testIndex = 2
obj1Train1 = trainTestList[[trainIndex]]
obj1Test1 = trainTestList[[testIndex]]

linearModel1 = lm(Life.expectancy ~., data = obj1Train1)

#Model Stats
summary(linearModel1)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel1) 
#ggpairs(obj1Train)
par(mfrow=c(1,1))
vif(linearModel1)^2 #infant.deaths and under.five.deaths highly correlated. Next model remove one
#Choose under.five.deaths as it also encompasses infants info


#Model Performance Stats: RMSE
obj1Test1$Predictions = predict(linearModel1, obj1Test1)
obj1Test1$Residuals = obj1Test1$Predictions - obj1Test1$Life.expectancy
obj1Test1$SquaredResiduals = obj1Test1$Residuals^2
mse = mean(obj1Test1$SquaredResiduals)
rmse1 = sqrt(mse)
rmse1
AIC(linearModel1)

#Second Model
secondModelVars = c("Year", "Status", "under.five.deaths", "percentage.expenditure",
              "HIV.AIDS", "Life.expectancy", "infant.deaths")
obj1Train2 = obj1Train1 %>% select(secondModelVars)
obj1Test2 = obj1Test1 %>% select(secondModelVars)

linearModel2 = lm(Life.expectancy ~., data = obj1Train2)

#Model Stats
summary(linearModel2)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel2) 
#ggpairs(obj1Train)
par(mfrow=c(1,1))
vif(linearModel2)^2 

#Model Performance Stats: RMSE
obj1Test2$Predictions = predict(linearModel2, obj1Test2)
obj1Test2$Residuals = obj1Test2$Predictions - obj1Test2$Life.expectancy
obj1Test2$SquaredResiduals = obj1Test2$Residuals^2
mse = mean(obj1Test2$SquaredResiduals)
rmse2 = sqrt(mse)
rmse2
AIC(linearModel2)

modelIterations = 5000
rmseModel1 = 0
rmseModel2 = 0
aicModel1 = 0
aicModel2 = 0
for(i in 1:modelIterations){
  #Train Test Setup
  splitPercent = 0.85
  trainTestList = get_train_test_list(LifeExpecObj1Clean1, splitPercent)
  trainIndex = 1
  testIndex = 2
  obj1Train1 = trainTestList[[trainIndex]]
  obj1Test1 = trainTestList[[testIndex]]
  
  secondModelVars = c("Year", "Status", "under.five.deaths", "percentage.expenditure",
                      "HIV.AIDS", "Life.expectancy", "infant.deaths")
  obj1Train2 = obj1Train1 %>% select(secondModelVars)
  obj1Test2 = obj1Test1 %>% select(secondModelVars)
  
  #Models
  linearModel1 = lm(Life.expectancy ~., data = obj1Train1)
  linearModel2 = lm(Life.expectancy ~., data = obj1Train2)
  
  #Get RMSE
  obj1Test1$Predictions = predict(linearModel1, obj1Test1)
  obj1Test1$Residuals = obj1Test1$Predictions - obj1Test1$Life.expectancy
  obj1Test1$SquaredResiduals = obj1Test1$Residuals^2
  mse = mean(obj1Test1$SquaredResiduals)
  rmse1 = sqrt(mse)
  rmseModel1 = rmseModel1+rmse1
  
  obj1Test2$Predictions = predict(linearModel2, obj1Test2)
  obj1Test2$Residuals = obj1Test2$Predictions - obj1Test2$Life.expectancy
  obj1Test2$SquaredResiduals = obj1Test2$Residuals^2
  mse = mean(obj1Test2$SquaredResiduals)
  rmse2 = sqrt(mse)
  rmseModel2 = rmseModel2+rmse2
  
  #Get AIC
  aicModel1 = aicModel1 + AIC(linearModel1)
  aicModel2 = aicModel2 + AIC(linearModel2)
}

#Average Everything
rmseModel1 = rmseModel1/modelIterations
rmseModel2 = rmseModel2/modelIterations
aicModel1 = aicModel1/modelIterations
aicModel2 = aicModel2/modelIterations

#Output
rmseModel1
rmseModel2
aicModel1
aicModel2

#Basically the same, choose the simpler model (Model 2)