library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(car)
library(glmnet)

#ToDo
  #Train Val Test
  #Redo analysis standardized

#Change working directory to this source file directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Load my custom functions
source("personal_funcs.r")

#Extract
LifeExpecFilePath = "../Datasets/LifeExpectancyData.csv"
LifeExpecRaw<-read.csv(LifeExpecFilePath)

#Gain some perspective on the data we are removing
variablesToExclude = c("Total.expenditure", "Schooling", "Population", "Income.composition.of.resources",
                       "Hepatitis.B", "GDP", "Alcohol")

LifeExpecNA = LifeExpecRaw %>% select(-variablesToExclude)

#Check the countrys that experienced the most NA rows
LifeExpecNA = get_na_df(LifeExpecNA)
str(LifeExpecNA)

#Specifically South Sudan and Sudan have the most missing values
LifeExpecNA %>% ggplot(aes(y = Country)) + geom_bar()

#Lose Some Perspective on Developing Nations
LifeExpecNA %>% ggplot(aes(y = Status)) + geom_bar()

#Remove High NA Features and Clean Data
variablesWithHighNa = c("Total.expenditure", "Schooling", "Population", "Income.composition.of.resources",
                        "Hepatitis.B", "GDP", "Alcohol", "Country") #High NA vars have More than 5% 
LifeExpecClean = LifeExpecRaw %>% select(-variablesWithHighNa)

#RemoveNAs
LifeExpecClean$Status = as.factor(LifeExpecClean$Status)
LifeExpecClean = LifeExpecClean %>% filter(!is.na(Life.expectancy))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(BMI))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(Adult.Mortality))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(Diphtheria))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(Polio))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(thinness..1.19.years))
LifeExpecClean = LifeExpecClean %>% filter(!is.na(thinness.5.9.years))

#How much of the original dataset did we remove: around 1%
PercentDataRemoved = (dim(LifeExpecRaw)[1] - dim(LifeExpecClean)[1])/dim(LifeExpecRaw)[1]*100
PercentDataRemoved

LifeExpecClean$Year = as.numeric(LifeExpecClean$Year)
LifeExpecClean$Adult.Mortality = as.numeric(LifeExpecClean$Adult.Mortality)
LifeExpecClean$infant.deaths = as.numeric(LifeExpecClean$infant.deaths)
LifeExpecClean$Measles = as.numeric(LifeExpecClean$Measles)
LifeExpecClean$under.five.deaths = as.numeric(LifeExpecClean$under.five.deaths)
LifeExpecClean$Polio = as.numeric(LifeExpecClean$Polio)
LifeExpecClean$Diphtheria = as.numeric(LifeExpecClean$Diphtheria)

#Verify no NAs
LifeExpecClean %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))/2938*100) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")



#Transform a feature prior to standardization as it will report NA values if we dont do it now
LifeExpecClean$LogOneOverHIV.AIDS = log(1/LifeExpecClean$HIV.AIDS)

#Standardize
vars = c("Year", "Life.expectancy", "Adult.Mortality", "infant.deaths", 
         "percentage.expenditure", "Measles", "BMI", "under.five.deaths",
         "Polio", "Diphtheria", "HIV.AIDS", "thinness..1.19.year", "thinness.5.9.years",
         "LogOneOverHIV.AIDS")
LifeExpecClean = get_standardized_df(LifeExpecClean, vars)

##Model 1: Everything that doesnt have excessive NA values is in the model

#Var Selection
variablesToRemove = c("LogOneOverHIV.AIDS")
LifeExpecClean1 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean1, splitPercent)

trainIndex = 1
testIndex = 2
Train1 = trainTestList[[trainIndex]]
Test1 = trainTestList[[testIndex]]

#LM Model
linearModel1 = lm(Life.expectancy ~., data = Train1)

#Model Stats
summary(linearModel1)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel1)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel1)^2

#Model Performance Stats: RMSE
Predictions = predict(linearModel1, Test1)
Residuals = Predictions - Test1$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse1 = sqrt(mse)
rmse1
AIC(linearModel1)

#Lasso

Train1Features = Train1
Train1Features = model.matrix(Life.expectancy~.,Train1Features)[,-1]
Train1Target = Train1$Life.expectancy

Test1Features = Test1
Test1Features = model.matrix(Life.expectancy~.,Test1Features)[,-1]
Test1Target = Test1$Life.expectancy

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(Train1Features,Train1Target,alpha=1, lambda =grid)
cv.out=cv.glmnet(Train1Features,Train1Target,alpha=1) #alpha=1 performs LASSO
plot(cv.out)

bestlambda<-cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=Test1Features)

testMSE_LASSO<-mean((Test1Target-lasso.pred)^2)
testMSE_LASSO

coef(lasso.mod,s=bestlambda)

##Model 2: Remove features lasso found useless

#Use coef to remove terms, increase complexity of remaining terms
#Remove:
#infant.deaths: 0
#under.five.deaths: e-04
#thinness.5.9.years: 0
#Most Impactful Term:
#Status e0

#Post Lasso Var Selection
variablesToRemove = c("infant.deaths", "under.five.deaths", "thinness.5.9.years", "LogOneOverHIV.AIDS")
LifeExpecClean2 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean2, splitPercent)

trainIndex = 1
testIndex = 2
Train2 = trainTestList[[trainIndex]]
Test2 = trainTestList[[testIndex]]

#LM Model
linearModel2 = lm(Life.expectancy ~., data = Train2)

#Model Stats
summary(linearModel2)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel2)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel2)^2 

#Metrics
Predictions = predict(linearModel2, Test2)
Residuals = Predictions - Test2$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse2 = sqrt(mse)
rmse2
AIC(linearModel2)

##Model 3: Remove variables with high collinearity(not all just enough to remove the high VIF problem)
#After Standardizing this is no longer an issue
#under.five.deaths and infant.deaths high VIF. Remove infant.deaths and Retrain

variablesToRemove = c("infant.deaths", "under.five.deaths", "thinness.5.9.years", "LogOneOverHIV.AIDS")
LifeExpecClean3 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean3, splitPercent)

trainIndex = 1
testIndex = 2
Train3 = trainTestList[[trainIndex]]
Test3 = trainTestList[[testIndex]]

#LM Model
linearModel3 = lm(Life.expectancy ~., data = Train3)

#Model Stats
summary(linearModel3)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel3)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel3)^2 #looks good now

#Metrics
Predictions = predict(linearModel3, Test3)
Residuals = Predictions - Test3$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse3 = sqrt(mse)
rmse3
AIC(linearModel3)


##Model 4: Create a complex model to help explain the data more

#Now deal with polynomial behavior
#Variables with nonlinear behavior
  #Adult.Mortality: Nonlinear 1 vertex even
  #BMI: Nonlinear 3 vertices even
  #under.five.deaths: Nonlinear 3 vertices even
  #Polio: Nonlinear 2 Vertices odd
  #Diphtheria: Nonlinear 2 vertices odd
  #HIV.AIDS: Nonlinear 1/x
  #thinness..1.19.years: Nonlinear 3 vertices even
variablesToRemove = c("infant.deaths", "under.five.deaths", "thinness.5.9.years", "HIV.AIDS")
LifeExpecClean4 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean4, splitPercent)

trainIndex = 1
testIndex = 2
Train4 = trainTestList[[trainIndex]]
Test4 = trainTestList[[testIndex]]



linearModel4 = lm(Life.expectancy ~ Year + Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    percentage.expenditure + percentage.expenditure^2 +
                    Measles + Measles^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 + 
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    LogOneOverHIV.AIDS + 
                    thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                  data = Train4)

#Model Stats
summary(linearModel4) #Now under.five.deaths no longer significant. Remove


#Assumption Check
par(mfrow=c(2,2))
plot(linearModel4)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel4)^2

#Metrics
Predictions = predict(linearModel4, Test4)
Residuals = Predictions - Test4$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse4 = sqrt(mse)
rmse4
AIC(linearModel4)


##Model 5: Remove variable the no longer explains as much
#This model no longer serves a purpose
#under.five.deaths no longer a significant term. Remove
variablesToRemove = c("infant.deaths", "under.five.deaths", "thinness.5.9.years", "HIV.AIDS")
LifeExpecClean5 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean5, splitPercent)

trainIndex = 1
testIndex = 2
Train5 = trainTestList[[trainIndex]]
Test5 = trainTestList[[testIndex]]

linearModel5 = lm(Life.expectancy ~ Year + Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 + 
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    log(1/HIV.AIDS) + 
                    thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                  data = Train5)

#Model Stats
summary(linearModel5)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel5)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel5)^2

#Metrics
Predictions = predict(linearModel5, Test5)
Residuals = Predictions - Test5$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse5 = sqrt(mse)
rmse5
AIC(linearModel5)

##Model 6: Remove a serial feature
#Years is technically time dependent but our analysis assumes no time dependence. Try a model without years

variablesToRemove = c("infant.deaths", "under.five.deaths", "thinness.5.9.years", "HIV.AIDS", "Year")
LifeExpecClean6 = LifeExpecClean %>% select(-variablesToRemove)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean6, splitPercent)

trainIndex = 1
testIndex = 2
Train6 = trainTestList[[trainIndex]]
Test6 = trainTestList[[testIndex]]

linearModel6 = lm(Life.expectancy ~ Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    percentage.expenditure + percentage.expenditure^2 +
                    Measles + Measles^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 + 
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    LogOneOverHIV.AIDS + 
                    thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                  data = Train4)

#Model Stats
summary(linearModel6) #Now under.five.deaths no longer significant. Remove

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel6)
#ggpairs(Train1)
par(mfrow=c(1,1))
vif(linearModel6)^2

#Metrics
Predictions = predict(linearModel6, Test6)
Residuals = Predictions - Test6$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse6 = sqrt(mse)
rmse6
AIC(linearModel6)


#Compare All Models Now
modelIterations = 1000
rmseModel1 = 0
rmseModel2 = 0
rmseModel3 = 0
rmseModel4 = 0
rmseModel5 = 0
rmseModel6 = 0
aicModel1 = 0
aicModel2 = 0
aicModel3 = 0
aicModel4 = 0
aicModel5 = 0
aicModel6 = 0

#Loop creates a train test split then evaluates the RMSe and AIC. Do this 
#modelIterations amount of times and average result.
for(i in 1:modelIterations){
  print(i)
  #Train Test Setup
  splitPercent = 0.85
  trainTestList = get_train_test_list(LifeExpecClean, splitPercent)
  trainIndex = 1
  testIndex = 2
  
  #Train Test dataframes for each model
  Train1 = trainTestList[[trainIndex]]
  Test1 = trainTestList[[testIndex]]
  
  variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years")
  Train2 = Train1 %>% select(-variablesToRemove)
  Test2 = Test1 %>% select(-variablesToRemove)
  
  variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years", "infant.deaths")
  Train3 = Train1 %>% select(-variablesToRemove)
  Test3 = Test1 %>% select(-variablesToRemove)
  
  variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years", "infant.deaths")
  Train4 = Train1 %>% select(-variablesToRemove)
  Test4 = Test1 %>% select(-variablesToRemove)
  
  variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years", "infant.deaths", "under.five.deaths")
  Train5 = Train1 %>% select(-variablesToRemove)
  Test5 = Test1 %>% select(-variablesToRemove)
  
  variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years", "infant.deaths", "under.five.deaths", "Year")
  Train6 = Train1 %>% select(-variablesToRemove)
  Test6 = Test1 %>% select(-variablesToRemove)
  
  #Models
  linearModel1 = lm(Life.expectancy ~., data = Train1)
  linearModel2 = lm(Life.expectancy ~., data = Train2)
  linearModel3 = lm(Life.expectancy ~., data = Train3)
  linearModel4 = lm(Life.expectancy ~ Year + Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 + 
                      under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 +
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      log(1/HIV.AIDS) + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train4)
  linearModel5 = lm(Life.expectancy ~ Year + Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 + 
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      log(1/HIV.AIDS) + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train5)
  linearModel6 = lm(Life.expectancy ~ Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 + 
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      log(1/HIV.AIDS) + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train6)
  
  #Get RMSE for each model
  #M1
  Predictions = predict(linearModel1, Test1)
  Residuals = Predictions - Test1$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel1 = rmseModel1 + rmse
  
  #M2
  Predictions = predict(linearModel2, Test2)
  Residuals = Predictions - Test2$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel2 = rmseModel2 + rmse
  
  #M3
  Predictions = predict(linearModel3, Test3)
  Residuals = Predictions - Test3$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel3 = rmseModel3 + rmse
  
  #M4
  Predictions = predict(linearModel4, Test4)
  Residuals = Predictions - Test4$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel4 = rmseModel4 + rmse
  
  #M5
  Predictions = predict(linearModel5, Test5)
  Residuals = Predictions - Test5$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel5 = rmseModel5 + rmse
  
  #M6
  Predictions = predict(linearModel6, Test6)
  Residuals = Predictions - Test6$Life.expectancy
  SquaredResiduals = Residuals^2
  mse = mean(SquaredResiduals)
  rmse = sqrt(mse)
  rmseModel6 = rmseModel6 + rmse
  
  #Get AIC
  aicModel1 = aicModel1 + AIC(linearModel1)
  aicModel2 = aicModel2 + AIC(linearModel2)
  aicModel3 = aicModel3 + AIC(linearModel3)
  aicModel4 = aicModel4 + AIC(linearModel4)
  aicModel5 = aicModel5 + AIC(linearModel5)
  aicModel6 = aicModel6 + AIC(linearModel6)
}

#Average Everything
rmseModel1 = rmseModel1/modelIterations
rmseModel2 = rmseModel2/modelIterations
rmseModel3 = rmseModel3/modelIterations
rmseModel4 = rmseModel4/modelIterations
rmseModel5 = rmseModel5/modelIterations
rmseModel6 = rmseModel6/modelIterations
aicModel1 = aicModel1/modelIterations
aicModel2 = aicModel2/modelIterations
aicModel3 = aicModel3/modelIterations
aicModel4 = aicModel4/modelIterations
aicModel5 = aicModel5/modelIterations
aicModel6 = aicModel6/modelIterations

#Output
rmseModel1
rmseModel2
rmseModel3
rmseModel4
rmseModel5
rmseModel6
aicModel1
aicModel2
aicModel3
aicModel4
aicModel5
aicModel6


#library(MASS)
#library(ISLR)
library(FNN)

#KNN Regression Model
par(mfrow=c(1,1))
#Use all base features before selection in my previous model
LifeExpecCleanKnn = LifeExpecClean

#Small Data Setup here, Status needs to be a dummy variable
LifeExpecCleanKnn$Developed =ifelse(LifeExpecCleanKnn$Status == "Developed", 1 , 0)
LifeExpecCleanKnn$Developing =ifelse(LifeExpecCleanKnn$Status == "Developing", 1 , 0)
variablesToRemove = c("Status")
LifeExpecCleanKnn = LifeExpecCleanKnn %>% select(-variablesToRemove)

#Now standardize everything that isn't a dummy variable
vars = c("Year", "Life.expectancy", "Adult.Mortality", "infant.deaths", 
                         "percentage.expenditure", "Measles", "BMI", "under.five.deaths",
                         "Polio", "Diphtheria", "HIV.AIDS", "thinness..1.19.year", "thinness.5.9.years")
LifeExpecCleanKnn = get_standardized_df(LifeExpecCleanKnn, vars)

#Train Test Prep
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecCleanKnn, splitPercent)

trainIndex = 1
testIndex = 2
TrainKnn = trainTestList[[trainIndex]]
TestKnn = trainTestList[[testIndex]]

target = c("Life.expectancy")

xTrainKnn = TrainKnn %>% select(-target)
yTrainKnn = TrainKnn %>% select(target)

xTestKnn = TestKnn %>% select(-target)
yTestKnn = TestKnn %>% select(target)

#Model
knnModel = knn.reg(train = xTrainKnn, test = xTestKnn, y = yTrainKnn, k = 1)

knnPredictions = knnModel[[4]]

#RMSE
Residuals = knnPredictions - yTestKnn$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse = sqrt(mse)
rmse

#Find optimal K
modelIterations = 500
kStart = 1
kEnd = 10
minRmse = 1000000
minK = 0
rmseKnnVector = c()

for(k in kStart:kEnd){
  print(k)
  rmseKnn = 0
  for(j in 1:modelIterations){
    if(k!=2){ #for some reason k=2 is not allowed for this k regression function
      #Train Test Prep
      splitPercent = 0.85
      trainTestList = get_train_test_list(LifeExpecCleanKnn, splitPercent)
      
      trainIndex = 1
      testIndex = 2
      TrainKnn = trainTestList[[trainIndex]]
      TestKnn = trainTestList[[testIndex]]
      
      target = c("Life.expectancy")
      
      xTrainKnn = TrainKnn %>% select(-target)
      yTrainKnn = TrainKnn %>% select(target)
      
      xTestKnn = TestKnn %>% select(-target)
      yTestKnn = TestKnn %>% select(target)
      
      #Model
      knnModel = knn.reg(train = xTrainKnn, test = xTestKnn, y = yTrainKnn, k = k)
      knnPredictions = knnModel[[4]]
      
      #UnStandardize
      yTestKnn$Life.expectancy = yTestKnn$Life.expectancy * sd(LifeExpecClean$Life.expectancy) + mean(LifeExpecClean$Life.expectancy)
      knnPredictions = knnPredictions * sd(LifeExpecClean$Life.expectancy) + mean(LifeExpecClean$Life.expectancy)
      
      #RMSE
      Residuals = knnPredictions - yTestKnn$Life.expectancy
      SquaredResiduals = Residuals^2
      mse = mean(SquaredResiduals)
      rmse = sqrt(mse)
      rmseKnn = rmseKnn + rmse
    }
  }
  if(k==2){
    avgRmseKnn = NA
    rmseKnnVector = c(rmseKnnVector, avgRmseKnn)
  }
  else{
    avgRmseKnn = rmseKnn/modelIterations
    rmseKnnVector = c(rmseKnnVector, avgRmseKnn)
    if(avgRmseKnn < minRmse){
      minRmse = avgRmseKnn
      minK = k
    }
  }
}
plot(kStart:kEnd, rmseKnnVector)
abline(v = minK, col="red")
#Report Minimum AVG RMSE and associated K
minRmse
minK

#KNN Regression using caret lib
library(caret)

par(mfrow=c(1,1))
#Use all base features before selection in my previous model
LifeExpecCleanKnn2 = LifeExpecClean

#Small Data Setup here, Status needs to be a dummy variable
LifeExpecCleanKnn2$Developed =ifelse(LifeExpecCleanKnn2$Status == "Developed", 1 , 0)
LifeExpecCleanKnn2$Developing =ifelse(LifeExpecCleanKnn2$Status == "Developing", 1 , 0)
variablesToRemove = c("Status")
LifeExpecCleanKnn2 = LifeExpecCleanKnn2 %>% select(-variablesToRemove)

#Standardize everything that isn't a dummy variable
vars = c("Year", "Life.expectancy", "Adult.Mortality", "infant.deaths", 
                         "percentage.expenditure", "Measles", "BMI", "under.five.deaths",
                         "Polio", "Diphtheria", "HIV.AIDS", "thinness..1.19.year", "thinness.5.9.years")
LifeExpecCleanKnn2 = get_standardized_df(LifeExpecCleanKnn2, vars)

#Train Test Prep
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecCleanKnn2, splitPercent)

trainIndex = 1
testIndex = 2
TrainKnn = trainTestList[[trainIndex]]
TestKnn = trainTestList[[testIndex]]

target = c("Life.expectancy")

xTrainKnn = TrainKnn %>% select(-target)
yTrainKnn = TrainKnn[, target]

xTestKnn = TestKnn %>% select(-target)
yTestKnn = TestKnn[, target]

#Model
knnModel = knnreg(x= xTrainKnn,y = yTrainKnn, k = 2)
knnPredictions = predict(knnModel, xTestKnn)

#RMSE
Residuals = knnPredictions - yTestKnn
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse = sqrt(mse)
rmse

#Find optimal K
modelIterations = 500
kStart = 1
kEnd = 10
minRmse = 1000000
minK = 0
rmseKnnVector = c()

for(k in kStart:kEnd){
  print(k)
  rmseKnn = 0
  for(j in 1:modelIterations){
    #Train Test Prep
    splitPercent = 0.85
    trainTestList = get_train_test_list(LifeExpecCleanKnn2, splitPercent)
    
    trainIndex = 1
    testIndex = 2
    TrainKnn = trainTestList[[trainIndex]]
    TestKnn = trainTestList[[testIndex]]
    
    target = c("Life.expectancy")
    
    xTrainKnn = TrainKnn %>% select(-target)
    yTrainKnn = TrainKnn[, target]
    
    xTestKnn = TestKnn %>% select(-target)
    yTestKnn = TestKnn[, target]
    
    #Model
    knnModel = knnreg(x= xTrainKnn,y = yTrainKnn, k = k)
    knnPredictions = predict(knnModel, xTestKnn)
    
    #Unstandardize so we can compare with previous models
    #yTestKnn = yTestKnn * sd(LifeExpecClean$Life.expectancy) + mean(LifeExpecClean$Life.expectancy)
    #knnPredictions = knnPredictions * sd(LifeExpecClean$Life.expectancy) + mean(LifeExpecClean$Life.expectancy)
    
    #RMSE
    Residuals = knnPredictions - yTestKnn
    SquaredResiduals = Residuals^2
    mse = mean(SquaredResiduals)
    rmse = sqrt(mse)
    rmse
    rmseKnn = rmseKnn + rmse
    
  }
  avgRmseKnn = rmseKnn/modelIterations
  rmseKnnVector = c(rmseKnnVector, avgRmseKnn)
  if(avgRmseKnn < minRmse){
    minRmse = avgRmseKnn
    minK = k
  }
}
plot(kStart:kEnd, rmseKnnVector)
abline(v = minK, col="red")
#Report Minimum AVG RMSE and associated K
minRmse
minK

#Seems like both models think k = 3 is the best value
