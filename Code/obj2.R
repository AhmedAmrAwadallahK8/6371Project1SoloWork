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
LifeExpecObj1Clean2 = LifeExpecRaw %>% select(-variablesWithHighNa)
LifeExpecObj1Clean2$Status = as.factor(LifeExpecObj1Clean2$Status)
str(LifeExpecObj1Clean2)
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(Life.expectancy))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(BMI))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(Adult.Mortality))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(Diphtheria))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(Polio))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(thinness..1.19.years))
LifeExpecObj1Clean2 = LifeExpecObj1Clean2 %>% filter(!is.na(thinness.5.9.years))

LifeExpecObj1Clean2$Year = as.numeric(LifeExpecObj1Clean2$Year)
LifeExpecObj1Clean2$Adult.Mortality = as.numeric(LifeExpecObj1Clean2$Adult.Mortality)
LifeExpecObj1Clean2$infant.deaths = as.numeric(LifeExpecObj1Clean2$infant.deaths)
LifeExpecObj1Clean2$Measles = as.numeric(LifeExpecObj1Clean2$Measles)
LifeExpecObj1Clean2$under.five.deaths = as.numeric(LifeExpecObj1Clean2$under.five.deaths)
LifeExpecObj1Clean2$Polio = as.numeric(LifeExpecObj1Clean2$Polio)
LifeExpecObj1Clean2$Diphtheria = as.numeric(LifeExpecObj1Clean2$Diphtheria)

LifeExpecObj1Clean2 %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))/2938*100) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")

splitPercent = 0.85
trainTestList = get_train_test_list(LifeExpecObj1Clean2, splitPercent)

trainIndex = 1
testIndex = 2
obj1Train2 = trainTestList[[trainIndex]]
obj1Test2 = trainTestList[[testIndex]]

linearModel2 = lm(Life.expectancy ~., data = obj1Train2)

#Model Stats
summary(linearModel2)

#Assumption Check
par(mfrow=c(2,2))
plot(linearModel2)
#ggpairs(obj1Train)
par(mfrow=c(1,1))
vif(linearModel2)^2

#

#Model Performance Stats: RMSE
obj1Test2$Predictions = predict(linearModel2, obj1Test2)
obj1Test2$Residuals = obj1Test2$Predictions - obj1Test2$Life.expectancy
obj1Test2$SquaredResiduals = obj1Test2$Residuals^2
mse = mean(obj1Test2$SquaredResiduals)
rmse = sqrt(mse)
rmse
AIC(linearModel2)

head(obj1Test2)

#Lasso

obj1Train2Features = obj1Train2
obj1Train2Features = model.matrix(Life.expectancy~.,obj1Train2Features)[,-1]
obj1Train2Target = obj1Train2$Life.expectancy

modelPerformanceStats = c("Predictions", "Residuals", "SquaredResiduals")
obj1Test2Features = obj1Test2 %>% select(-modelPerformanceStats)
obj1Test2Features = model.matrix(Life.expectancy~.,obj1Test2Features)[,-1]
obj1Test2Target = obj1Test2$Life.expectancy

grid=10^seq(10,-2, length =100)
lasso.mod=glmnet(obj1Train2Features,obj1Train2Target,alpha=1, lambda =grid)
cv.out=cv.glmnet(obj1Train2Features,obj1Train2Target,alpha=1) #alpha=1 performs LASSO
plot(cv.out)

bestlambda<-cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=obj1Test2Features)

testMSE_LASSO<-mean((obj1Test2Target-lasso.pred)^2)
testMSE_LASSO

coef(lasso.mod,s=bestlambda)
#Use coef to remove terms, increase complexity of remaining terms