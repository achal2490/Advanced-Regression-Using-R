install.packages("psych")
install.packages("caret")
install.packages("tidyverse")
installed.packages("ggplot2")
install.packages("dplyr")

library(psych)
library(caret)
library(dplyr)
library(tidyverse)
library(ggplot2)


######################### Data Accquisition ##########################
RealEstate <- read.csv("Data/Real estate valuation data set.csv", header = T)

# View the data
head(RealEstate,5)


########################## Data Cleaning ############################
# Renaming Column Names in Data Frame

colnames(RealEstate) <- c('Id','TranDate','Age','MRTDist', 'NoStores'
                     ,'Lat', 'Long', 'Y_HousePrice')

#View the structure of data
str(RealEstate)
  
  
# Converting into date
RealEstate$TranDate <- as.Date(RealEstate$TranDate, origin = "1900-01-01")

#Verify the structure of data
str(RealEstate)


### Descriptive Analysis
#Exploring the data and gather some insights!
#Duplicating data set for exploration analysis, we will create some new columns and don't want to alter original data frame

RealEstate_Explore <- RealEstate


#### Average House price in Euros per square metre
#Original Data is given in 10000 New Taiwan Dollar/Ping. 

#Assuming average conversion rate 1NT$ = 0.028 Euros and 1 ping = 3.30 sq meter.
#Below code will convert House Price as required and store values into a new column. And Later calculate the average of that column

convFact <- ((10000 * 0.028)/3.30)
RealEstate_Explore$PriceEuroPerSqM <- RealEstate_Explore$Y_HousePrice * convFact 
mean_All <- mean(RealEstate_Explore$PriceEuroPerSqM)

##### The average House price in Euros Per Sq Metre is:

mean_All


#### Are houses near(within 1KM radius) MRT station more expensive?
#Note - Original distance is given in meters.
#Below code will first filter out houses which are less than 1 KM away from MRT station and then calculate average of the prices for those houses.

RealEstate_Less1KM <- RealEstate_Explore %>% 
  filter(RealEstate_Explore$MRTDist<1000)
mean_Less1KM <- mean(RealEstate_Less1KM$PriceEuroPerSqM)

#The average House price in Euros Per Sq Meter where distance to nearest MRT is less than 1KM is:

mean_Less1KM

#The difference between both averages in Euros is:

mean_Less1KM - mean_All


#Also, visualizing the House Prices and Distance from MRT station using a scatter plot

ggplot(data = RealEstate_Explore, aes(x = MRTDist, y = PriceEuroPerSqM)) +
  geom_point()  +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Distance from MRT Station Vs House Prices") +
  xlab("Distance from Nearest MRT station(Metres)") + 
  ylab("House Price (Euros Per Sq Metres)")


#Scatter plot clearly shows that houses closer to MRT stations have comparatively higher prices.Hence the houses near MRT station have higher price on an average than overall city average

#### Does age of the house relates to the price of the house?
#visualizing the House Prices and age of the house using a scatter plot

ggplot(data = RealEstate_Explore, aes(x = Age, y = PriceEuroPerSqM)) +
  geom_point()  +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Age of the Vs House Prices") +
  xlab("Age of the House") + 
  ylab("House Price (Euros Per Sq Metres)")

#Scatter plot clearly shows that  age of the house is not related to the price of house. Hence the house price is not affected by the age of the house

#### Age of the top 5 costliest properties which are not newly built
#Below code will filter out houses whose Age > 0 and then sort them in ascending order

Costliest_Estate <- RealEstate_Explore %>% 
  filter(Age > 0) %>% 
  arrange(Y_HousePrice)



barplot(tail(Costliest_Estate$Age,5),
        names.arg=tail(Costliest_Estate$Id,5),
        col = ("gold1"),
        border = ("goldenrod2"),
        main="Age of Top 5 Non-Newly Built Costliest Houses", 
        xlab="Esatate ID",
        ylab="HOuse Price"
        )



### Regression Analysis

####Creating Subset
#Create a subset with only required columns for the regression

RealEstate_Norm_sub <- RealEstate%>% 
  select(Age:Y_HousePrice)


####Analyse Output/Predictor column
#Bar plot of the House Prices


boxplot(RealEstate_Norm_sub$Y_HousePrice)

#Notice there are few outliers, performing further analysis on whether to remove outlier or not.

#Plotting a scatterplot with regression line to visualize position of outlier


scatter.smooth(RealEstate_Norm_sub$Y_HousePrice, ylab= "House Prices")

#Scatterplot clearly shows the outlier will not impact the regression line but will only increase the residuals and can be removed.

#Calculate the Adjusted Rsquare value for the regression model before removing any outlier.

summary(lm(Y_HousePrice ~ ., data =RealEstate_Norm_sub))$adj.r.squared

#adjusted r square is 0.565 which suggest its not a great model

#Removing the house with exceptionally high price and recalculating the r square value.


RealEstate_Norm_sub <- RealEstate_Norm_sub[-which.max(RealEstate_Norm_sub$Y_HousePrice),]
summary(lm(Y_HousePrice ~ ., data =RealEstate_Norm_sub))$adj.r.squared


#Adjusted r square for the model is greatly increased and hence removal of the record was correct decision.

#Also, visualizing the scatterplot again to verify the regression line.

scatter.smooth(RealEstate_Norm_sub$Y_HousePrice, ylab= "House Prices")

#Slope of regression is line not impacted.

####Correlation among various columns
#Graph below will perform three analysis
#1. Distribution of data for each feature or column shown using a histogram.
#2. Correlation coefficients for each combination of feature
#3. Scatterplot with fit line to visualize correlation


pairs.panels(RealEstate_Norm_sub)


# Key Observations 
#1. Age does not a show a strong relation with any of the feature and hence must be included in model.
#2. Distance from nearest MRT station is highly correlated with latitude and longitude.

#### Splitting data into training and test datasets.

set.seed(123)
training.samples <- RealEstate_Norm_sub$Y_HousePrice %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- RealEstate_Norm_sub[training.samples, ]
test.data <- RealEstate_Norm_sub[-training.samples, ]



#### Hypotheis
#NULL Hypothesis H0: There is no relationship between Dependent and Independent variables
#Alternate Hypothesis H1: There is a relationship between Dependent and Independent variables


#### Initial Model
#Creating a regression model first using all columns and view the adjusted r squared statistics for the model.

priceModel <- lm(Y_HousePrice ~ ., data =train.data)
summary(priceModel)$adj.r.squared

#Adjusted r squared is bit low, more the value of adjusted r square better will be the model.

#Optimizing the model and then taking a deeper look into various model statistics.

### MODEL OPTIMIZATION
#### Adding Non Linear Relationships
#By default linear regression assumes linear relationship, which is not the case for each feature. For example age of the house can have a non linear #relationship.

train.data$Age_2 <- train.data$Age^2
test.data$Age_2 <- test.data$Age^2


#### Step Backward Optimization

priceModel_step <- step(priceModel, direction = "backward")
priceModel_step <- lm(formula = Y_HousePrice ~ Age + MRTDist + 
     NoStores + Lat + Age_2, 
   data = train.data)


summary(priceModel_step)$adj.r.squared

#Increase in Adjusted R squared value signifies an increase in accuracy of the model.
#Further improving accuracy by taking into account interactions.

#### Adding Interactions and Non Linear relationship for enhanced model
#There is a possibility of interaction between location of house (latitude and longitude) and distance from MRT station and hence taking interactions into #account.

priceModel_enhanced <- lm(Y_HousePrice ~ Age + Age_2 + MRTDist*Lat*Long + NoStores  , data =train.data)


summary(priceModel_enhanced)

#Increase in Adjusted r squared value indicates the improvement in model performance
#Also, overall p-value (less than 0.05) shows null hypothesis can be rejected and hence house price can be predicted using various columns.
#F-Statistic represent the relation between dependent and independent variable, higher the value stronger the relationship
#Above summary shows the features were significant in predicting House Prices. Three stars
#shows highly significant features while two or less stars shows less significant features.

### Understanding Summary in detail
#Understanding each part of the summary in more details.

#### Is Hypothesis supported?

summary(priceModel_enhanced)$coefficients

#Output shows equation coefficients for each feature 

#First row shows the intercept or constant of the equation
#Next rows represent the slope of equation for each of the feature used in regression.

#Various Columns represent following information.
 #Estimate: Expected value for the slope of feature and intercept of the equation.
 #Std. Error: Average variation from expected value
 #t value: Statistics to show how many SD the estimated coefficient is from 0
 #Pr(>|t|): p-Value, using 95% CI, if p-value is less than 0.05 reject null hypothesis

#It can be observed that p-value (less than 0.05) and all columns are significant in predicting house prices.
  

#### How well model fits the data?

summary(summary(priceModel_enhanced)$residuals)

#Minimum, maximum and other values represent value of error that can occur in the model.
#Residuals explain how well the model is fits the data. Residuals should be symmetrically distributed around 0. Visualizing the residual plot.


ggplot(data=train.data, aes(priceModel_enhanced$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#Histogram shows that residuals are evenly distributed around 0 and hence model is a good fit.

### Comparing three models using RMSE
#The RMSE is the square root of the variance of the residuals.
#Lower values of RMSE indicate better fit. RMSE is a good measure of how accurately the model predicts the response, and it is the most important criterion #for fit if the main purpose of the model is prediction.


sqrt(sum(priceModel$residuals^2) / priceModel$df)
sqrt(sum(priceModel_step$residuals^2) / priceModel_step$df)
sqrt(sum(priceModel_enhanced$residuals^2) / priceModel_enhanced$df)

#Enhanced model has the least RMSE value and is best in predicting House prices.


### Predicting prices


predictPrice <- predict(priceModel_enhanced, test.data)



#### Plot Actual vs Predicted
#Plot of actual vs predicted house prices represent how well the model performed in predicting house prices of test data set.

ggplot(data = test.data, aes(x = predictPrice, y = test.data$Y_HousePrice)) +
  geom_point()  +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data") +
  ylab("Actual Price") + 
  xlab("Predicted  Price")

#Model was able to predict house prices, there were some variations in the predictions but model 

# Evaluting residual plots for the model to analyse the fit
layout(matrix(1:4,2,2)) # to display at once
plot(priceModel_enhanced)

# Residuals Vs Fitted  - Residuals are randomly distributed around horizontal line, no distinct trend in distribution is seen
# Normal QQ - Residuals follow straight line with few exceptions and hence can be considered as normally distributed
# Scale Location - Residuals are equally distributed along range of predictors. Hence homoscedasity is there
# Residuals vs Leverage - No points exist on top right or bottom right corner i.e points with distance greater than 1 and hence model is good fit 