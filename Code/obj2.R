library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(car)
library(glmnet)


#Extract
LifeExpecFilePath = "../Datasets/LifeExpectancyData.csv"
LifeExpecRaw<-read.csv(LifeExpecFilePath)

#Remove High NA Features and Clean Data
variablesWithHighNa = c("Total.expenditure", "Schooling", "Population", "Income.composition.of.resources",
                        "Hepatitis.B", "GDP", "Alcohol", "Country") #High NA vars have More than 5% 
LifeExpecClean1 = LifeExpecRaw %>% select(-variablesWithHighNa)
LifeExpecClean1$Status = as.factor(LifeExpecClean1$Status)
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(Life.expectancy))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(BMI))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(Adult.Mortality))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(Diphtheria))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(Polio))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(thinness..1.19.years))
LifeExpecClean1 = LifeExpecClean1 %>% filter(!is.na(thinness.5.9.years))

LifeExpecClean1$Year = as.numeric(LifeExpecClean1$Year)
LifeExpecClean1$Adult.Mortality = as.numeric(LifeExpecClean1$Adult.Mortality)
LifeExpecClean1$infant.deaths = as.numeric(LifeExpecClean1$infant.deaths)
LifeExpecClean1$Measles = as.numeric(LifeExpecClean1$Measles)
LifeExpecClean1$under.five.deaths = as.numeric(LifeExpecClean1$under.five.deaths)
LifeExpecClean1$Polio = as.numeric(LifeExpecClean1$Polio)
LifeExpecClean1$Diphtheria = as.numeric(LifeExpecClean1$Diphtheria)

#Verify no NAs
LifeExpecClean1 %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))/2938*100) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")

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

#

#Model Performance Stats: RMSE
Predictions = predict(linearModel1, Test1)
Residuals = Predictions - Test1$Life.expectancy
SquaredResiduals = Residuals^2
mse = mean(SquaredResiduals)
rmse1 = sqrt(mse)
rmse1
AIC(linearModel1)

head(Test1)

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
#Use coef to remove terms, increase complexity of remaining terms
#Remove:
  #Measles: e-05
  #percent.expenditure: e-04
  #thinness.5.9.years: e-04
#Most Impactful Term:
  #Status e0

#Post Lasso Var Selection
variablesToRemove = c("Measles", "percentage.expenditure", "thinness.5.9.years")
LifeExpecClean2 = LifeExpecClean1 %>% select(-variablesToRemove)

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
summary(linearModel1)

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

#under.five.deaths and infant.deaths high VIF. Remove infant.deaths and Retrain
variablesToRemove = c("infant.deaths")
LifeExpecClean3 = LifeExpecClean2 %>% select(-variablesToRemove)

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

#Now deal with polynomial behavior
#Variables with nonlinear behavior
  #Adult.Mortality: Nonlinear 1 vertex even
  #BMI: Nonlinear 3 vertices even
  #under.five.deaths: Nonlinear 3 vertices even
  #Polio: Nonlinear 2 Vertices odd
  #Diphtheria: Nonlinear 2 vertices odd
  #HIV.AIDS: Nonlinear 1/x
  #thinness..1.19.years: Nonlinear 3 vertices even

str(LifeExpecClean3)

#Train Test Split
splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean3, splitPercent)

trainIndex = 1
testIndex = 2
Train4 = trainTestList[[trainIndex]]
Test4 = trainTestList[[testIndex]]

linearModel4 = lm(Life.expectancy ~ Year + Status + 
                    Adult.Mortality + Adult.Mortality^2 +
                    BMI + BMI^2 + BMI^3 + BMI^4 + 
                    under.five.deaths + under.five.deaths^2 + under.five.deaths^3 + under.five.deaths^4 +
                    Polio + Polio^2 + Polio^3 +
                    Diphtheria + Diphtheria^2 + Diphtheria^3 + 
                    log(1/HIV.AIDS) + 
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

#Model 5
variablesToRemove = c("under.five.deaths")
LifeExpecClean5 = LifeExpecClean3 %>% select(-variablesToRemove)

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
summary(linearModel5) #Now under.five.deaths no longer significant. Remove

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

#Compare 5 Models Now
modelIterations = 1000
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

for(i in 1:modelIterations){
  print(i)
  #Train Test Setup
  splitPercent = 0.85
  trainTestList = get_train_test_list(LifeExpecClean1, splitPercent)
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

  
  #Get AIC
  aicModel1 = aicModel1 + AIC(linearModel1)
  aicModel2 = aicModel2 + AIC(linearModel2)
  aicModel3 = aicModel3 + AIC(linearModel2)
  aicModel4 = aicModel4 + AIC(linearModel2)
  aicModel5 = aicModel5 + AIC(linearModel2)
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

#Output
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
