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
Test1$Predictions = predict(linearModel1, Test1)
Test1$Residuals = Test1$Predictions - Test1$Life.expectancy
Test1$SquaredResiduals = Test1$Residuals^2
mse = mean(Test1$SquaredResiduals)
rmse = sqrt(mse)
rmse
AIC(linearModel1)

head(Test1)

#Lasso

Train1Features = Train1
Train1Features = model.matrix(Life.expectancy~.,Train1Features)[,-1]
Train1Target = Train1$Life.expectancy

modelPerformanceStats = c("Predictions", "Residuals", "SquaredResiduals")
Test1Features = Test1 %>% select(-modelPerformanceStats)
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

#Now deal with polynomial behavior
#Variables with nonlinear behavior
  #Adult.Mortality: Nonlinear 1 vertex even
  #BMI: Nonlinear 3 vertices even
  #under.five.deaths: Nonlinear 3 vertices even
  #Polio: Nonlinear 2 Vertices odd
  #Diphtheria: Nonlinear 2 vertices odd
  #HIV.AIDS: Nonlinear 1/x
  #thinness..1.19.years: Nonlinear 3 vertices even