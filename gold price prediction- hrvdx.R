

################################## Gold price Prediction ##################################################


# -A- ------------------------- Data exploratory and Analysis--------------------------
# 1. - ----------------------------------------------Dowloading and creating our dataset
# 2. - ----------------------------------------------Preparing & Exploring the data
# 3. - ----------------------------------------------Creating -Exponential moving averages (EMA)-
# 4. - ----------------------------------------------Feature Engineering
# 5. - ----------------------------------------------Exploring & Visualizing

# -B- ------------------------- Modeling and testing-----------------------------------
# 1.- ----------------------------------------------create train/test datasets
# 2.- ----------------------------------------------Creating function to process the predictions
# 3.- ----------------------------------------------Applying three models:
# a.- ----------------------------------------------Naive bayes Model
# b.- ----------------------------------------------Support vector Model
# c.- ----------------------------------------------Random Forest Model

# -C---------------------------- Overall performance of the models-----------------------------------
# 1.- ----------------------------------------------visualizing in a table the accuracy of our models
# 2.- ----------------------------------------------Plotting the cummlative returns
# 3.- ----------------------------------------------Conclusion




################################ A- Data exploratory and Analysis ###############################

# 1- Dowloading and creating our dataset- ----------------------------------------------
# #Note: you dont need to run this code, it is just to show you how our dataframe that we are using in this project was created"
# Downloading the source file and cleaning the data-
#----the below code will show you how the data was downloaded from finance.yahoo and exported to the WD---
# # downloading the data
# getSymbols("GC=F", src ="yahoo")
# golddataset<- `GC=F` 
# # creating a new DF and slicing the columns that we will use in this project (open, high, low, close)
# golddataset<- golddataset[,1:4]
# # removing the rows with NAs, normally these are the days where there is no trade.
# golddataset<- na.omit(golddataset)
# #exporting the file into the working directory
# library(zoo)
# write.zoo(golddataset, file ='golddataset.csv',sep=",")
#----------------------------------------------------------------------------------


#Preparing & Exploring the data------------------------------------------------------------
#Adding the required packages & libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
library(quantmod) # chartseries
library(dplyr)
library(tidyverse)
library(TTR)  # EMAs
library(ggplot2)
library(randomForest) #RF
library(e1071)  # SVM
library(naivebayes) # NB
library(xts) 

#Downloading our dataset as csv file format and changing the type into xts file
#Dowloading and importing  the data from the github repository 
urlfile="https://raw.githubusercontent.com/rwelkh/price-prediction--hrvdx/master/goldds.csv"
goldds0<-read_csv(url(urlfile))
#Checking how thw data is structured
str(goldds0)
#changing the columns names
names(goldds0)<- c("Date", "Open", "High", "Low", "Close")
#Change the file from CSV to xts format
gold_xts <- xts(goldds0[,-1], order.by = goldds0$Date)
#We will use only the last two years in this project.
#we will train our model based on the previous two years hence, we will slice the data accordingly.
#will slice the data starting from January 1, 2018 till june 1, 2020
gold_xts <- gold_xts["20180101/20200601"]
#Plot the goldprices chart
rm(goldds0, urlfile)
chartSeries(gold_xts, theme= 'white')
#----------------------------------------------------------------------------------

# 3-Creating -Exponential moving averages (EMA)------------------------------------------------
#Creating the EMA for 7 days 
gold_xts$ema7 <- EMA(gold_xts$Close, n = 7)
#Creating the EMA for 20 days
gold_xts$ema20 <- EMA(gold_xts$Close, n = 20)

#Since the EMA is based on the previous data we will notice that the first 20 rows will include NA values in the EMA20 column and 7 NA Values in the EMA7 & column.
gold_xts <- gold_xts[20:length(gold_xts$Open),]
#checking the number for rows and columns
dim(gold_xts)
#Plotting a subset of the data starting from 2020 in order to visualize the EMAs.
min2020<- min(gold_xts['2020']$Close)
max2020<- max(gold_xts['2020']$Close)
#Plotting the EMAs
chartSeries(gold_xts, subset = "2020-01::",theme = "white")
addEMA( n = 7, col = "purple")
addEMA( n = 20, col = "red")
#Plotting a subset of the data starting from 2020 in order to visualize the EMAs.
#We can clearly see that the ema7 line is more reactive  to the changes compared with the ema20
#The gold price started at around 1300$ in 2018 and moved up the next two years to reach a value of more than 1700$
#We can see that we have one major drop in 2020 which was quickly recovered and the price continue its increase afterwards.
#----------------------------------------------------------------------------------

# 4- Feature Engineering -----------------------------------------------
# we will create each new feature in a separate data frame and then we will combine them all in one.
#the type of the candle for the current day 
CurrentCandle<- data.frame(ifelse(gold_xts$Close > gold_xts$Open, "bull", "bear"))
#We will use lag to get the previous day closing price
#PreviousCandle will have NA as the first row value, we will treat that after creating all the features
PreviousCandle <- data.frame(lag(CurrentCandle$Close, n = 1))
#we will use lead to get the next day price, it is the opposit of lag 
#NextdayCandle 
NextDayCandle <- data.frame(lead(CurrentCandle$Close, n = 1))
#We will set a threshold  of 0.45 where we consider the candle is Doji, 
#doji implies that there is no significant surge or plunge in the price
Doji <- data.frame(ifelse(abs(gold_xts$Close - gold_xts$Open) < 0.45, "yes", "no"))
#Creating an indication for the position of ema7 if it is above or below the price
#ma7Po
PositionToEma7 <- data.frame(ifelse(gold_xts$Close > gold_xts$ema7, "above", "below"))
#Creating an indication for the position of ema20 if it is above or below the price
#price$Ema20Position
PositionToEma20 <- data.frame(ifelse(gold_xts$Close > gold_xts$ema20, "above", "below"))
#EMA to EMA 20 position 
#Ema7toEma20Position
Ema7ToEma20_ <- data.frame(ifelse(gold_xts$ema7 > gold_xts$ema20, "above", "below"))
# to check the profit and loss
DailyReturn <- data.frame(abs(gold_xts$Close - gold_xts$Open))
# prediction of return value for next day
NextDayReturn <- lead(DailyReturn$Close, n = 1)

#Creating the dataframe with all the features as columns
gold_features <- data.frame(CurrentCandle, PreviousCandle,
                            Doji, PositionToEma7, PositionToEma20, Ema7ToEma20_,
                            DailyReturn, NextDayReturn, NextDayCandle)

#Naming the dataframe columns
names(gold_features) <- c("CurrentCandle", "PreviousCandle",
                          "Doji", "PositionToEma7", "PositionToEma20", "Ema7ToEma20_",
                          "DailyReturn", "NextDayReturn", "NextDayCandle")

#Removing the first raw that contains NA
gold_features <- gold_features[2:length(gold_features$CurrentCandle),]

#The summary shows us that we have NA values in two features, these Nas are result of the nextdayreturn and nextdaycandle. and once we look at the table we notice that the NAs are in the last row, we wont include the last line in the test DS.
summary (gold_features)

#Removing the Dataframes
rm("CurrentCandle", "PreviousCandle",
   "Doji", "PositionToEma7", "PositionToEma20", "Ema7ToEma20_",
   "DailyReturn", "NextDayReturn", "NextDayCandle")
#----------------------------------------------------------------------------------


################################# -B- Modeling and testing-################################ 

# 1- Creating train/test datasets---------------------------------------------

#Splitting the data in to train and test datasets
#Training set 70% 
TrainIndicies <- 1:419
#Test set indicies 30%, we will take all not include the last line of the data as it contains NAs
TestIndicies<- 420:579
train <- gold_features[TrainIndicies,]
test <- gold_features[TestIndicies,]
#The summary shows us that there is no NAs, the bear candles outcount the bull by almost 100 times
summary(train)
#The summary shows us that there is no NAs, and the bear and  bull candle count are not very far from each others.
#We can conclude that the market downtrend in the train set is much significant than the test set.
summary(test)
#plotting the data gold price data set based on the train/test indices
#----
# the graph in the train dataset shows a down trend in 2018 and then and upward trend starting 2019..
plot(gold_xts$Close[TrainIndicies,])
# we notice that the prices are higher in the test dataset with a big drop and recovery that occured in march 2020
plot(gold_xts$Close[TestIndicies,])
#----------------------------------------------------------------------------


# 2- Creating function to process the predictions---------------------------------------------
# Creating a function for processing predictions, With this function we are creating three features (pred , prediReturn, cumReturn) that we will include in the dataframe.
predictedReturn <- function(df, pred) { 
  #pred is our predictions from the machine learning model
  df$pred <- pred
  # transforming to negative value if our prediction is wrong 
  df$prediReturn <- ifelse(df$NextDayCandle != df$pred, -df$NextDayReturn, df$NextDayReturn)
  # calculating the cummulative sum 
  df$cumReturn <- cumsum(df$prediReturn)
  return(df)
}
#-------------------------------------------------------------------------------

#3- Models and analysis ------------------------------------------------------------
# -a -Naive bayes model
Model_1 <- naive_bayes(NextDayCandle ~., data = train)
#prediction the outcome on the test set
Model_1_predictions <- suppressWarnings(predict(Model_1,test))
#passing the outcome to the function "predictedReturn" that we have created, in order to prepare the data as we need it then add it to a new Dataset called " nb.test"
naivebayes_ds<-predictedReturn(test, Model_1_predictions)
# plotting the returns in a graph, as we can see from the graph there is no consistance trend, our predictions seems to be very unconsistance in terms of accurancy.
plot(naivebayes_ds$prediReturn, type = "l")  
# plotting the accumulated return over the entire period in a graph  to have a clear picture of the entire equity status.
plot(naivebayes_ds$cumReturn, type = "l")  

#the confusion matrix to check the sensitivity and specificity.
naivebayes_CM <- table(naivebayes_ds$NextDayCandle,naivebayes_ds$pred)
#printing the results
naivebayes_CM 
#calculating accuracy
# we will calculate the error percentage, where the prediction is not match the given values of the next day.
Model1_error = mean(naivebayes_ds$NextDayCandle != naivebayes_ds$pred, na.rm=TRUE)
# using the error we will calculate the accuracy of the predictions
NB_model_accuracy<- 1- Model1_error
# displaying the results
accuracy_results <- data_frame(Method = "Naive bayes Model", Accuracy = NB_model_accuracy)
#--------------------------------------------------------------------------------------------

# - b- Supportvector Model ---------------------------------------------------------------
Model_2<- svm(NextDayCandle ~., data = train, kernel = "radial" )
## gives kernel used: "radial" implying it did some basic transforms.
#fitting our model
Model_2_predictions <-predict(Model_2, test)
#finding the predicted outcome.
Supportvector_ds <- predictedReturn(test, Model_2_predictions)
# plotting the returns
plot(Supportvector_ds$prediReturn, type = "l")
# plotting the accumulated returns
plot(Supportvector_ds$cumReturn, type = "l")

#confusion matrix
Supportvector_CM <- table(Supportvector_ds$NextDayCandle,Supportvector_ds$pred)
#printing the results
Supportvector_CM 

#calculating the accuracy
SVM_error <- mean(Supportvector_ds$NextDayCandle != Supportvector_ds$pred)
SVM_Accuracy <- 1 - SVM_error
accuracy_results <- bind_rows(accuracy_results ,
                              data_frame(Method="Support vector Model",
                                         Accuracy = SVM_Accuracy ))
#--------------------------------------------------------------------------------------------

# - c- Random Forest model---------------------------------------------------------------
#Fitting the random forest model
Model_3 <- randomForest(NextDayCandle ~., data = train)
# bydefault it the number of tree will be 500
# We will create a dataframe that will contain all the errors generated per iteration.
obb.error.data<- data.frame(
  Tress= rep(1:nrow(Model_3$err.rate), times=3),
  Type=  rep(c("OOB","bull", "bear"), each=nrow(Model_3$err.rate)),
  Error= c(Model_3$err.rate[,"OOB"],
           Model_3$err.rate[,"bull"],
           Model_3$err.rate[,"bear"])
)

# plotting the OOB error rate graph, we can observe that the error rate doesnt have significant fluctuations or up/down trend by increasing the number of trees. hence we will keep the default tress number of 500.
ggplot(data=obb.error.data, aes(x=Tress, y= Error)) +
  geom_line(aes(color=Type))
# we will plot the " variable of importance" to identify the variable with the most significant impact on our predictions
# NextDayReturn and the Daily return has the most significant importance among the other features.
varImpPlot(Model_3)
#finding the predicted outcome.
Model_3_predictions <- predict(Model_3, test)
#results
randomForest_ds <- predictedReturn(test, Model_3_predictions)
#Plotting the predition and cummulative returns
plot(randomForest_ds$prediReturn, type = "l")
plot(randomForest_ds$cumReturn, type = "l")
#confusion matrix
randomForest_CM <- table(randomForest_ds$NextDayCandle, randomForest_ds$pred)
randomForest_CM 
#calculating accuracy
RF_error <- mean(randomForest_ds$NextDayCandle != randomForest_ds$pred)
RF_Accuracy <- 1 - RF_error
accuracy_results <- bind_rows(accuracy_results ,
                              data_frame(Method="Random Forest Model",
                                         Accuracy = RF_Accuracy ))
#--------------------------------------------------------------------------------------------

############################## C- Overall performance of the models ##############################  

# 1-visualizing in a table the accuracy of our three models----------------------------------------
accuracy_results 

#Random forest is the only model that exceeded the 50% predictions
#--------------------------------------------------------------------------------------------


#2-Plotting the cummlative returns-----------------------------------------------
#Creating a data frame that includes all the cummulative returns
nb_cumReturn <- naivebayes_ds$cumReturn
svm_cumReturn <- Supportvector_ds$cumReturn
rf_cumReturn <- randomForest_ds$cumReturn
aggregated_cumReturn <- data.frame(c(1:length(nb_cumReturn)), nb_cumReturn, svm_cumReturn, rf_cumReturn)

ggplot() +
  geom_line(aes(aggregated_cumReturn[,1], nb_cumReturn, colour = "nb"))+
  geom_line(aes(aggregated_cumReturn[,1], svm_cumReturn, colour = "svm"))+
  geom_line(aes(aggregated_cumReturn[,1], rf_cumReturn, colour = "rf"))+
  ylab("Cummulative returns (x1000 USD)")
#--------------------------------------------------------------------------------------------


#3-Conclusion----------------------------------------------------
# In this chart we can see the equity curve for the different algorithems , with Naive bais method the risk of losing 200k as we can see in the graph and then end up with 50k on the positive end, we can conclude that it is a very risky and unreliable model in this case. while SVM model for sure will lead to bigger loses. 
#Random forest doesnt look like the perfect model as well however it managed to stay in the positive end (over 0) more than the other two models. in our case it will be our chosen model.
#--------------------------------------------------------------------------------------------





#---------------------------thank you ------------------------------------------








