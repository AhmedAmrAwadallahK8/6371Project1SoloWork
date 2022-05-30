#EDA FILE
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyverse)

#Future thought: Might be possible to automate this whole thing
LifeExpecFilePath = "../Datasets/LifeExpectancyData.csv"
LifeExpecRaw<-read.csv(LifeExpecFilePath)
head(LifeExpecRaw)
str(LifeExpecRaw)

#NA Values
LifeExpecRaw %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))/2938*100) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature") + xlab("Na Value Percent")

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
  

#NA Handling Method:
  #I would prefer to just remove the NAs features from consideration for the main model
  #As they introduce a sampling BIAS. A subsequent model with these features
  #Could be considerd but with the caveat that it has a bias in it

#BMI: Nonlinear
#under.five.deaths: Nonlinear
#Polio: Nonlinear
#Diphtheria: Nonlinear
#HIV.AIDS: Nonlinear
#thinness..1.19.years: Nonlinear

#EDA Visual Summary
  #Strong Relationship
    #Year: Linear Y
    #Status: Catergorical Y
    #Adult.Mortality: Nonlinear
    #Alcohol: Nonlinear
    #percentage.expenditure: Nonlinear Y
    #BMI: Nonlinear
    #Polio: Nonlinear
    #Diphtheria: Nonlinear
    #HIV.AIDS: Nonlinear Y
    #GDP: Nonlinear
    #thinness..1.19.years: Nonlinear
    #thinness.5.9.years: Nonlinear
    #Income.composition.of.resources: Nonlinear
    #Schooling: Nonlinear
  #Moderate Relationship
    #Hepatitis.B: Nonlinear
    #Measles: Nonlinear
    #under.five.deaths: Nonlinear
    #Total.expenditure: Nonlinear
  #Weak Relationship
    #Infant,deaths: Nonlinear
    #Population: Nonlinear

#Everything compared to Life.expectancy
#ED1: Country
LifeExpec_v_Country = LifeExpecRaw %>% select(c("Life.expectancy", "Country"))

#Plot Everything: Looks bad
LifeExpec_v_Country %>% ggplot(aes(x=Life.expectancy, y=reorder(Country, Life.expectancy))) + geom_boxplot()

#Extract Median by Country
MedianLifeExpectancy = 
  LifeExpec_v_Country %>%
  group_by(Country) %>%
  summarise(MedianLife.expectancy=median(Life.expectancy))

#Reorder Country based on Median
MedianLifeExpectancy = MedianLifeExpectancy[order(MedianLifeExpectancy$MedianLife.expectancy, decreasing = TRUE),]
MedianLifeExpectancy  = MedianLifeExpectancy  %>% filter(!is.na(MedianLife.expectancy))
#Pickout x amount of head or tail
entry_count = 20
TopMedianLifeExpec = data.frame(MedianLifeExpectancy %>% head(entry_count))
BotMedianLifeExpec = data.frame(MedianLifeExpectancy %>% tail(entry_count))

#Filter main DF by whoever was in the top or bottom
TopLifeExpec_v_Country = LifeExpec_v_Country %>%
  filter(Country %in% TopMedianLifeExpec$Country)
BotLifeExpec_v_Country = LifeExpec_v_Country %>%
  filter(Country %in% BotMedianLifeExpec$Country)

#Plot
TopLifeExpec_v_Country %>% 
  ggplot(aes(x=Life.expectancy, y=reorder(Country, Life.expectancy))) + geom_boxplot()

#Bot has some values missing distributions?
BotLifeExpec_v_Country  %>% 
  ggplot(aes(x=Life.expectancy, y=reorder(Country, Life.expectancy))) + geom_boxplot()

#ED2: Year
LifeExpecRaw %>% ggplot(aes(x=Year, y=Life.expectancy)) + geom_smooth()
#Trending Up

#ED3: Status: Developed or Developing status
LifeExpecRaw %>% ggplot(aes(x=Life.expectancy, y=Status)) + geom_boxplot()
#Developed median is higher than developing

#ED4: Adult.Mortality: Adult Mortality Rates of both sexes (probability of dying between 15 and 60 years per 1000 population)
LifeExpecRaw %>% ggplot(aes(x=Adult.Mortality, y=Life.expectancy)) + geom_smooth()
#Interesting behavior, goes up then falls off rapidly

#ED5: Infant,deaths: Number of Infant Deaths per 1000 population
LifeExpecRaw %>% ggplot(aes(x=infant.deaths, y=Life.expectancy)) + geom_smooth()
#Odd Behavior

#ED6: Alcohol: Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
LifeExpecRaw %>% ggplot(aes(x=Alcohol, y=Life.expectancy)) + geom_smooth()
#Interesting Behavior

#ED7: percentage.expenditure: Expenditure on health as a percentage of Gross Domestic Product per capita(%)
LifeExpecRaw %>% ggplot(aes(x=percentage.expenditure, y=Life.expectancy)) + geom_smooth()

#ED8: Hepatitis.B: Hepatitis B (HepB) immunization coverage among 1-year-olds (%)
LifeExpecRaw %>% ggplot(aes(x=Hepatitis.B, y=Life.expectancy)) + geom_smooth()

#ED9: Measles: Measles - number of reported cases per 1000 population
LifeExpecRaw %>% ggplot(aes(x=Measles, y=Life.expectancy)) + geom_smooth()

#ED10: BMI: Average Body Mass Index of entire population
LifeExpecRaw %>% ggplot(aes(x=BMI, y=Life.expectancy)) + geom_smooth()
 
#ED11: under.five.deaths: Number of under-five deaths per 1000 population
LifeExpecRaw %>% ggplot(aes(x=under.five.deaths, y=Life.expectancy)) + geom_smooth()

#ED12: Polio: Polio (Pol3) immunization coverage among 1-year-olds (%)
LifeExpecRaw %>% ggplot(aes(x=Polio, y=Life.expectancy)) + geom_smooth()

#ED13: Total.expenditure: General government expenditure on health as a percentage of total government expenditure (%)
LifeExpecRaw %>% ggplot(aes(x=Total.expenditure, y=Life.expectancy)) + geom_smooth()

#ED14: Diphtheria: Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)
LifeExpecRaw %>% ggplot(aes(x=Diphtheria, y=Life.expectancy)) + geom_smooth()

#ED15: HIV.AIDS: Deaths per 1 000 live births HIV/AIDS (0-4 years)
LifeExpecRaw %>% ggplot(aes(x=HIV.AIDS, y=Life.expectancy)) + geom_smooth()

#ED16: GDP: Gross Domestic Product per capita (in USD)
LifeExpecRaw %>% ggplot(aes(x=GDP, y=Life.expectancy)) + geom_smooth()

#ED17: Population
LifeExpecRaw %>% ggplot(aes(x=Population, y=Life.expectancy)) + geom_smooth()

#ED18: thinness..1.19.years: Prevalence of thinness among children and adolescents for Age 10 to 19 (% )
LifeExpecRaw %>% ggplot(aes(x=thinness..1.19.years, y=Life.expectancy)) + geom_smooth()

#ED19: thinness.5.9.years: Prevalence of thinness among children for Age 5 to 9(%)
LifeExpecRaw %>% ggplot(aes(x=thinness.5.9.years, y=Life.expectancy)) + geom_smooth()

#ED20: Income.composition.of.resources: Human Development Index in terms of income composition of resources (index ranging from 0 to 1)
LifeExpecRaw %>% ggplot(aes(x=Income.composition.of.resources, y=Life.expectancy)) + geom_smooth()

#ED21: Schooling: Number of years of Schooling(years)
LifeExpecRaw %>% ggplot(aes(x=Schooling, y=Life.expectancy)) + geom_smooth()


