#Data management packages
library(ggplot2)
library(coefplot)
library(dplyr)
library(imputeTS)
library(mice)
library(forecast)

#Exercise 3: Predicting household income with logistic regression

#Read the dataset into R library
survey.data <- read.csv("acs_ny.csv")

#examine dataset
head(survey.data)

#summary stat of the dataset
summary(survey.data)

#create a density plot of family income to see distribution
p <- ggplot(survey.data, aes(x=FamilyIncome)) + 
  geom_density(color="darkblue", fill="green")
p

#creating binary variable for family income (1 for income above $150,000 and 0 for income below $150,000)
survey.data$FamilyIncome <- ifelse(survey.data$FamilyIncome >= 150000, 1, 0)
summary(survey.data)

#model building using logistic regression
model <- glm(formula = FamilyIncome~., data = survey.data, family = "binomial")
summary(model)

#select significant varibles to create new model
survey.model <- glm(formula = FamilyIncome~NumBedrooms+NumPeople+NumRooms+NumUnits+NumVehicles
                    +NumWorkers+NumWorkers+OwnRent+Insurance, data = survey.data, family = "binomial")
summary(survey.model)
#create model for family income greater than or equal $150,000
survey.model2 <-  glm(FamilyIncome == 1~NumBedrooms+NumPeople+NumRooms+NumUnits+NumVehicles
                      +NumWorkers+NumWorkers+OwnRent+Insurance, data = survey.data, family = "binomial")

#Create a coefficient plot for logistic regression on family income greater than $150,000 
coefplot(survey.model2,col.pts="red",  intercept=TRUE)

#Exercise 4: Predicting patient count for 12 months for a Dental Clinic

#Read the dataset into R library
dental <- read.csv("BestSmileDental.csv")

#examine dataset
head(dental)
#summary stat of the dataset
summary(dental)

ggplot(dental, aes(Year, Customers)) + geom_line()  + ylab("Customers") +
  xlab("")

#convert non numeric to NA value
dental$Customers <- as.integer(dental$Customers)

#convert negative numbers to NA value
dental$Customers[dental$Customers < 0] <- NA

#imputation for missing/invalid data using ImputeTS
new.dental <- dental %>% na.kalman

# build a time series from data
dentalTS  <- ts(new.dental$Customers, start = c(2001,1), frequency = 12)
plot(dentalTS)

#find the components of the time series
components.ts = decompose(dentalTS)
plot(components.ts)

# Assess the time series using ACF and PACF
acf(dentalTS)
pacf(dentalTS)

# use diffing for data transformation. R can find optimal diffing
ndiffs(x = dentalTS)

# plot to see the effect of diffing
plot(diff(dentalTS, 1))

# fit the ARIMA model
arimaForecast <- auto.arima(x=dentalTS)

# review the ARIMA model
arimaForecast

# check the ACF and PACF of the ARIMA model residuals
acf(arimaForecast$residuals)
pacf(arimaForecast$residuals)
# check the coefficients
coef(arimaForecast)

# try second ARIMA models - this time provide p,d,q values
arimaForecast2  <- arima(dentalTS, order=c(0,1,1))

# review the second ARIMA model
arimaForecast2

# check the ACF and PACF of the ARIMA model residuals
acf(arimaForecast2$residuals)
pacf(arimaForecast2$residuals)
coef(arimaForecast2)

# # predict next five months using the second ARIMA model
nextForecast  <- forecast (arimaForecast2, h=12)
# review the predictions from the second ARIMA model
nextForecast
# plot the predictions from the second ARIMA model
plot(nextForecast)

# forecast using Holt-Winters Exponential Smoothing
holtForecast.mean <- HoltWinters(dentalTS, gamma=FALSE)

# check alpha and beta suggested by R
holtForecast.mean

#Forecast the Customer count for Year 2008 using Holt-Winters model
holtForecast.predict <- predict(holtForecast.mean, n.ahead = 12, prediction.interval = TRUE)
holtForecast.predict

# Plot data
plot.ts(dentalTS)
lines(holtForecast.mean$fitted[,1], col="green")
lines(holtForecast.predict[,1], col="blue")
lines(holtForecast.predict[,2], col="red")
lines(holtForecast.predict[,3], col="red")



