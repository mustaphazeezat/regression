
## Packages
library(readr)
library(dplyr)
library(car)
library(olsrr)
library(ggcorrplot)
library(ggplot2)
library(leaps)
library(xtable)

path <- "bikedata.csv"
bikeData <- read_csv(file=path)

# check the structure of Data
head(bikeData)
str(bikeData)

## changed type of variable seasons and Holiday  to factor
class(bikeData$Seasons)
bikeData$Seasons <- as.factor(bikeData$Seasons)
unique(bikeData$Hour)

class(bikeData$Holiday)
bikeData$Holiday <- as.factor(bikeData$Holiday)
unique(bikeData$Holiday)

#Check for missing data
sum(is.na(bikeData))

#QUESTION 1
## Relationship between the variables
numericData = bikeData[, 1:9]

corr <- round(cor(numericData), 1)

p.mat <- cor_pmat(numericData)
head(p.mat[, 1:4])
ggcorrplot(corr, type = "full",
           outline.col = "white",  lab = TRUE, p.mat = p.mat)

#Relationship between Seasons,Holiday and bike rental

ggplot(bikeData, aes(x=Seasons, y=log.Rented.Bike.Count, fill=Seasons)) +
  geom_boxplot()+ 
  theme(legend.position ="top")

ggplot(bikeData, aes(x=Holiday, y=log.Rented.Bike.Count, fill=Holiday)) +
  geom_boxplot()+ 
  theme(legend.position ="top")

#Question 2
##regression
fullModel = lm(log.Rented.Bike.Count ~., bikeData)
summary(fullModel)

###confidence interval of the model
confint(fullModel)

## Extract regression coefficient in a table
xtable(fullModel)
xtable(confint(fullModel))

#Question 3
##Regression - Model selection
bestSubsetModels = ols_step_best_subset(fullModel)
bestSubsetModels

### Selecting model 7(Hour Temperature Humidity Wind.speed Rainfall Seasons Holiday) with the lowest Mallows' C_p
bestModel <-
  lm(log.Rented.Bike.Count ~  Hour + Temperature + Humidity + Wind.speed + Rainfall + Seasons + Holiday, data = bikeData)
summary(bestModel)

#How the categorical variables were coded.
contrasts(bikeData$Seasons)
contrasts(bikeData$Holiday)

###confidence interval of the model
confint(bestModel)

## Extract regression coefficient in a table
xtable(summary(bestModel))
xtable(confint(bestModel, level = 0.95))

#Question 4
## Residual plots for model evaluation of best model
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2

plot(bestModel)


## Multicollinearity

car::vif(bestModel)
xtable(car::vif(bestModel))

