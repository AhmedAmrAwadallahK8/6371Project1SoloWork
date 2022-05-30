library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(car)
library(glmnet)


LifeExpecFilePath = "../Datasets/LifeExpectancyData.csv"
LifeExpecRaw<-read.csv(LifeExpecFilePath)

#Analysis Obj2; Varnames incorrect atm
variablesWithHighNa = c("Total.expenditure", "Schooling", "Population", "Income.composition.of.resources",
                        "Hepatitis.B", "GDP", "Alcohol", "Country") #More than 5%
LifeExpecClean1 = LifeExpecRaw %>% select(-variablesWithHighNa)
LifeExpecClean1$Status = as.factor(LifeExpecClean1$Status)
str(LifeExpecClean1)
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

LifeExpecClean1 %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))/2938*100) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")

splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecClean1, splitPercent)

trainIndex = 1
testIndex = 2
Train1 = trainTestList[[trainIndex]]
Test1 = trainTestList[[testIndex]]

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