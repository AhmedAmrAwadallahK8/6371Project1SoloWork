library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(car)
library(glmnet)
library(reshape2)

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

#NA Count by Country plot
LifeExpecNA %>% ggplot(aes(y = Country)) + geom_bar()

NaCountries = c("Tuvalu", "Timor-Leste", "Sudan", "South Sudan", "San Marino", "Saint Kitts and Nevis", 
                "Palau", "Niue", "Nauru", "Montenegro", "Monaco", "Marshall Islands", "Dominica", "Cook Islands")
#Country count if it had NA values removed
LifeExpecRaw[LifeExpecRaw$Country %in% NaCountries,] %>% ggplot(aes(y = Country)) + geom_bar()

#Country data remaining after removing NAs plot setup
NaCountryCount = as.data.frame(table(LifeExpecNA$Country))
CountryCount = as.data.frame(table(LifeExpecRaw[LifeExpecRaw$Country %in% NaCountries,]$Country))
CountryCount$RowsRemaining = CountryCount$Freq - NaCountryCount$Freq 

#Remaining Data for Country plot
CountryCount %>% ggplot(aes(y = Var1, x = RowsRemaining)) + geom_col()


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
LifeExpecCleanS = LifeExpecClean
#Standardize
vars = c("Year", "Life.expectancy", "Adult.Mortality", "infant.deaths", 
         "percentage.expenditure", "Measles", "BMI", "under.five.deaths",
         "Polio", "Diphtheria", "HIV.AIDS", "thinness..1.19.year", "thinness.5.9.years",
         "LogOneOverHIV.AIDS")
LifeExpecClean = get_standardized_df(LifeExpecClean, vars)

#Train Validation Test Setup
set.seed(1)
testSplitPercent = 0.9
trainTestList = get_train_test_list(LifeExpecClean, testSplitPercent)

trainValIndex = 1
testIndex = 2
TrainVal = trainTestList[[trainValIndex]]
Test = trainTestList[[testIndex]]

testRows = dim(Test)[1]
trainValRows = dim(TrainVal)[1]
testRows
valSplitPercent = 1 - (testRows/trainValRows)
valSplitPercent
trainValList = get_train_test_list(TrainVal, valSplitPercent)

trainIndex = 1
valIndex = 2
Train = trainValList[[trainIndex]]
Val = trainValList[[valIndex]]

valRows = dim(Val)[1]
valRows

##Model 1: Everything that doesnt have excessive NA values is in the model

#Var Selection
variablesToRemove = c("LogOneOverHIV.AIDS")
Train1 = Train %>% select(-variablesToRemove)
Val1 = Val %>% select(-variablesToRemove)

#LM Model
linearModel1 = lm(Life.expectancy ~., data = Train1)

#Model Stats
summary(linearModel1)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel1)
par(mfrow=c(1,1))
#ggpairs(Train1)
vif(linearModel1)^2

#Model Performance Stats: RMSE
Predictions = predict(linearModel1, Val1)
rmse1 = get_rmse(Predictions, Val1$Life.expectancy)
rmse1
AIC(linearModel1)

#Lasso

Train1Features = Train1
Train1Features = model.matrix(Life.expectancy~.,Train1Features)[,-1]
Train1Target = Train1$Life.expectancy

Val1Features = Val1
Val1Features = model.matrix(Life.expectancy~.,Val1Features)[,-1]
Val1Target = Val1$Life.expectancy

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(Train1Features,Train1Target,alpha=1, lambda =grid)
cv.out=cv.glmnet(Train1Features,Train1Target,alpha=1) #alpha=1 performs LASSO
plot(cv.out)

bestlambda<-cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=Val1Features)

testMSE_LASSO<-mean((Val1Target-lasso.pred)^2)
testMSE_LASSO

coef(lasso.mod,s=bestlambda)

##Model 2: Remove features lasso found useless
#Use coef to remove terms, increase complexity of remaining terms
#Remove:
#infant.deaths: 0
#thinness.5.9.years: 0

#Post Lasso Var Selection
variablesToRemove = c("infant.deaths", "thinness.5.9.years", "LogOneOverHIV.AIDS")
Train2 = Train %>% select(-variablesToRemove)
Val2 = Val %>% select(-variablesToRemove)

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
Predictions = predict(linearModel2, Val2)
rmse2 = get_rmse(Predictions, Val2$Life.expectancy)
rmse2
AIC(linearModel2)

##Model 3: Create a complex model to help explain the data more

#Now deal with polynomial behavior
#Variables with nonlinear behavior
  #Adult.Mortality: Nonlinear 1 vertex even
  #BMI: Nonlinear 3 vertices even
  #under.five.deaths: Nonlinear 3 vertices even
  #Polio: Nonlinear 2 Vertices odd
  #Diphtheria: Nonlinear 2 vertices odd
  #HIV.AIDS: Nonlinear 1/x
  #thinness..1.19.years: Nonlinear 3 vertices even
variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS")
Train3 = Train %>% select(-variablesToRemove)
Val3 = Val %>% select(-variablesToRemove)

linearModel3 = lm(Life.expectancy ~ Year + Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    percentage.expenditure + percentage.expenditure^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 +
                    Measles + Measles^2 +
                    under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 + 
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    LogOneOverHIV.AIDS + 
                    thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                  data = Train3)

#Model Stats
summary(linearModel3) #Now under.five.deaths no longer significant. Remove


#Assumption Check
par(mfrow=c(2,2))
plot(linearModel3)
par(mfrow=c(1,1))
vif(linearModel3)^2

#Metrics
Predictions = predict(linearModel3, Val3)
rmse3 = get_rmse(Predictions, Val3$Life.expectancy)
rmse3
AIC(linearModel3)

##Model 4: Remove a serial feature
#Years is technically time dependent but our analysis assumes no time dependence. Try a model without years

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year")
Train4 = Train %>% select(-variablesToRemove)
Val4 = Val %>% select(-variablesToRemove)

linearModel4 = lm(Life.expectancy ~ Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    percentage.expenditure + percentage.expenditure^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 +
                    Measles + Measles^2 +
                    under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 + 
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
par(mfrow=c(1,1))
vif(linearModel4)^2

#Metrics
Predictions = predict(linearModel4, Val4)
rmse4 = get_rmse(Predictions, Val4$Life.expectancy)
rmse4
AIC(linearModel4)

#Model 5. under.five.deaths removed

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year", "under.five.deaths")
Train5 = Train %>% select(-variablesToRemove)
Val5 = Val %>% select(-variablesToRemove)

linearModel5 = lm(Life.expectancy ~ Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    percentage.expenditure + percentage.expenditure^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 +
                    Measles + Measles^2 +
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    LogOneOverHIV.AIDS + 
                    thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                  data = Train5)

#Model Stats
summary(linearModel5) #Now under.five.deaths no longer significant. Remove

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel5)
par(mfrow=c(1,1))
vif(linearModel5)^2

#Metrics
Predictions = predict(linearModel5, Val5)
rmse5 = get_rmse(Predictions, Val5$Life.expectancy)
rmse5
AIC(linearModel5)

#Compare All Models Now
modelIterations = 700
rmseModel1 = 0
rmseModel2 = 0
rmseModel3 = 0
rmseModel4 = 0
rmseModel5 = 0
aicModel1 = 0
aicModel2 = 0
aicModel3 = 0
aicModel4 = 0
aicModel5 = 0

#Loop creates a train test split then evaluates the RMSe and AIC. Do this 
#modelIterations amount of times and average result.
for(i in 1:modelIterations){
  print(i)
  #Train Test Setup
  trainValList = get_train_test_list(TrainVal, valSplitPercent)
  trainIndex = 1
  valIndex = 2
  
  Train = trainValList[[trainIndex]]
  Val = trainValList[[valIndex]]
  
  #Train Test dataframes for each model
  variablesToRemove = c("LogOneOverHIV.AIDS")
  Train1 = Train %>% select(-variablesToRemove)
  Val1 = Val %>% select(-variablesToRemove)
  
  variablesToRemove = c("infant.deaths", "thinness.5.9.years", "LogOneOverHIV.AIDS")
  Train2 = Train %>% select(-variablesToRemove)
  Val2 = Val %>% select(-variablesToRemove)
  
  variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS")
  Train3 = Train %>% select(-variablesToRemove)
  Val3 = Val %>% select(-variablesToRemove)
  
  variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year")
  Train4 = Train %>% select(-variablesToRemove)
  Val4 = Val %>% select(-variablesToRemove)
  
  variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year", "under.five.deaths")
  Train5 = Train %>% select(-variablesToRemove)
  Val5 = Val %>% select(-variablesToRemove)
  
  
  
  #Models
  linearModel1 = lm(Life.expectancy ~., data = Train1)
  linearModel2 = lm(Life.expectancy ~., data = Train2)

  linearModel3 = lm(Life.expectancy ~ Year + Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      percentage.expenditure + percentage.expenditure^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 + 
                      Measles + Measles^2 +
                      under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 +
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      LogOneOverHIV.AIDS + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train3)
  linearModel4 = lm(Life.expectancy ~ Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      percentage.expenditure + percentage.expenditure^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 +
                      Measles + Measles^2 +
                      under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 +
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      LogOneOverHIV.AIDS + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train4)
  
  linearModel5 = lm(Life.expectancy ~ Status + 
                      Adult.Mortality + Adult.Mortality^2 +
                      percentage.expenditure + percentage.expenditure^2 +
                      BMI + BMI^2 + BMI^3 + BMI^4 +
                      Measles + Measles^2 +
                      Polio + Polio^2 + Polio^3 +
                      Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                      LogOneOverHIV.AIDS + 
                      thinness..1.19.years + thinness..1.19.years^2 + thinness..1.19.years^3 + thinness..1.19.years^4,
                    data = Train5)
  
  #Get RMSE for each model
  #M1
  Predictions = predict(linearModel1, Val1)
  rmse = get_rmse(Predictions, Val1$Life.expectancy)
  rmseModel1 = rmseModel1 + rmse
  
  #M2
  Predictions = predict(linearModel2, Val2)
  rmse = get_rmse(Predictions, Val2$Life.expectancy)
  rmseModel2 = rmseModel2 + rmse
  
  #M3
  Predictions = predict(linearModel3, Val3)
  rmse = get_rmse(Predictions, Val3$Life.expectancy)
  rmseModel3 = rmseModel3 + rmse
  
  #M4
  Predictions = predict(linearModel4, Val4)
  rmse = get_rmse(Predictions, Val4$Life.expectancy)
  rmseModel4 = rmseModel4 + rmse
  
  #M5
  Predictions = predict(linearModel5, Val5)
  rmse = get_rmse(Predictions, Val5$Life.expectancy)
  rmseModel5 = rmseModel5 + rmse
  
  #Get AIC
  aicModel1 = aicModel1 + AIC(linearModel1)
  aicModel2 = aicModel2 + AIC(linearModel2)
  aicModel3 = aicModel3 + AIC(linearModel3)
  aicModel4 = aicModel4 + AIC(linearModel4)
  aicModel5 = aicModel5 + AIC(linearModel5)
}

#Average Everything
rmseModel1 = rmseModel1/modelIterations
rmseModel2 = rmseModel2/modelIterations
rmseModel3 = rmseModel3/modelIterations
rmseModel4 = rmseModel4/modelIterations
rmseModel5 = rmseModel5/modelIterations
aicModel1 = aicModel1/modelIterations
aicModel2 = aicModel2/modelIterations
aicModel3 = aicModel3/modelIterations
aicModel4 = aicModel4/modelIterations
aicModel5 = aicModel5/modelIterations

#Train Test dataframes for each model
variablesToRemove = c("LogOneOverHIV.AIDS")
Test1 = Test %>% select(-variablesToRemove)

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "LogOneOverHIV.AIDS")
Test2 = Test %>% select(-variablesToRemove)

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS")
Test3 = Test %>% select(-variablesToRemove)

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year")
Test4 = Test %>% select(-variablesToRemove)

variablesToRemove = c("infant.deaths", "thinness.5.9.years", "HIV.AIDS", "Year", "under.five.deaths")
Test5 = Test %>% select(-variablesToRemove)

#Get Test
print("Test Set RMSE")
Predictions = predict(linearModel1, Test1)
rmse1 = get_rmse(Predictions, Test1$Life.expectancy)

Predictions = predict(linearModel2, Test2)
rmse2 = get_rmse(Predictions, Test2$Life.expectancy)

Predictions = predict(linearModel3, Test3)
rmse3 = get_rmse(Predictions, Test3$Life.expectancy)

Predictions = predict(linearModel4, Test4)
rmse4 = get_rmse(Predictions, Test4$Life.expectancy)

Predictions = predict(linearModel5, Test5)
rmse5 = get_rmse(Predictions, Test5$Life.expectancy)

Predictions = predict(linearModel5, Test5)
UnstandardizedPredictions = Predictions*sd(LifeExpecCleanS$Life.expectancy) + mean(LifeExpecCleanS$Life.expectancy)
UnstandardizedTargets = Test5$Life.expectancy*sd(LifeExpecCleanS$Life.expectancy) + mean(LifeExpecCleanS$Life.expectancy)
rmse5Unstandardized = get_rmse(UnstandardizedPredictions, UnstandardizedTargets)

#Output
print("Validation Metrics")
rmseModel1
rmseModel2
rmseModel3
rmseModel4
rmseModel5
aicModel1
aicModel2
aicModel3
aicModel4
aicModel5

#Test Output
print("Test Metrics")
rmse1
rmse2
rmse3
rmse4
rmse5

#Final Model Information Together
summary(linearModel5)
plot(predict(linearModel5, Test5), Test5$Life.expectancy, 
     xlab = "Predictions", ylab = "Targets", main = "Targets v Predictions")
aicModel5
#Standardized
#Validation Set
rmseModel5
#Test Set
rmse5

#Unstandardized RMSE
#Test Set
rmse5Unstandardized

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

#Train Validation Test Setup
set.seed(1)
testSplitPercent = 0.9
trainTestList = get_train_test_list(LifeExpecCleanKnn, testSplitPercent)

trainValIndex = 1
testIndex = 2
TrainVal = trainTestList[[trainValIndex]]
TestKnn = trainTestList[[testIndex]]

testRows = dim(Test)[1]
trainValRows = dim(TrainVal)[1]
valSplitPercent = 1 - (testRows/trainValRows)

trainValList = get_train_test_list(TrainVal, valSplitPercent)

trainIndex = 1
valIndex = 2
TrainKnn = trainValList[[trainIndex]]
ValKnn = trainValList[[valIndex]]

target = c("Life.expectancy")
xTrainKnn = TrainKnn %>% select(-target)
yTrainKnn = TrainKnn %>% select(target)

xValKnn = ValKnn %>% select(-target)
yValKnn = ValKnn %>% select(target)

xTestKnn = TestKnn %>% select(-target)
yTestKnn = TestKnn %>% select(target)

#Model
knnModel = knn.reg(train = xTrainKnn, test = xValKnn, y = yTrainKnn, k = 1)

knnPredictions = knnModel[[4]]

#RMSE
rmse = get_rmse(knnPredictions, yValKnn$Life.expectancy)
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
      trainValList = get_train_test_list(TrainVal, valSplitPercent)
      trainIndex = 1
      valIndex = 2
      
      TrainKnn = trainValList[[trainIndex]]
      ValKnn = trainValList[[valIndex]]
      
      target = c("Life.expectancy")
      
      xTrainKnn = TrainKnn %>% select(-target)
      yTrainKnn = TrainKnn %>% select(target)
      
      xValKnn = ValKnn %>% select(-target)
      yValKnn = ValKnn %>% select(target)
      
      #Model
      knnModel = knn.reg(train = xTrainKnn, test = xValKnn, y = yTrainKnn, k = k)
      knnPredictions = knnModel[[4]]
      
      #RMSE
      rmse = rmse = get_rmse(knnPredictions, yValKnn$Life.expectancy)
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

#Test set RMSE
#Model
knnModel = knn.reg(train = xTrainKnn, test = xTestKnn, y = yTrainKnn, k = minK)
knnPredictions = knnModel[[4]]

#RMSE
rmseTest = get_rmse(knnPredictions, yTestKnn$Life.expectancy)
rmseTest

plot(knnPredictions, yTestKnn$Life.expectancy)

#KNN Regression using caret lib
library(caret)

par(mfrow=c(1,1))


#Model 2 using Caret
knnModel = knnreg(x= xTrainKnn,y = yTrainKnn$Life.expectancy, k = 2)
knnPredictions = predict(knnModel, xValKnn)

#RMSE
rmse = get_rmse(knnPredictions, yValKnn$Life.expectancy)
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
    trainValList = get_train_test_list(TrainVal, valSplitPercent)
    trainIndex = 1
    valIndex = 2
    
    TrainKnn = trainValList[[trainIndex]]
    ValKnn = trainValList[[valIndex]]
    
    target = c("Life.expectancy")
    
    xTrainKnn = TrainKnn %>% select(-target)
    yTrainKnn = TrainKnn %>% select(target)
    
    xValKnn = ValKnn %>% select(-target)
    yValKnn = ValKnn %>% select(target)
    
    #Model
    knnModel = knnreg(x= xTrainKnn,y = yTrainKnn$Life.expectancy, k = k)
    knnPredictions = predict(knnModel, xValKnn)
    
    #RMSE
    rmse = get_rmse(knnPredictions, yValKnn$Life.expectancy)
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

#Test Set
knnModel = knnreg(x= xTrainKnn,y = yTrainKnn$Life.expectancy, k = minK)
knnPredictions = predict(knnModel, xTestKnn)

#RMSE
rmse = get_rmse(knnPredictions, yTestKnn$Life.expectancy)
rmse

plot(knnPredictions, yTestKnn$Life.expectancy)
