library(readr)
library(tidyverse)
library(xts)
library(forecast)
library(MLmetrics)

#import data and preprocess----
data <- read_csv("WHO-COVID-19-global-data.csv", col_types = cols(Date_reported = col_date(format = "%Y-%m-%d")))

cleanData <- data %>%
  filter(Country == "Indonesia") %>% #extract Indonesia data only
  select(-c(Country_code, Country, WHO_region)) %>% #remove unnecessary columns
  {.[60:880, ]} #extract the first occurrence of case (2020-03-02) until last day of May (2022-05-31)

#convert to time series data and train-test split----
timeData <- xts(cleanData[, 2:5], order.by = cleanData$Date_reported) #convert to time series data

timeDataTrain <- window(timeData, start = "2020-03-02", end = "2022-04-30") #beginning to most recent month
timeDataTest <- window(timeData, start = "2022-05-01", end = "2022-05-31") #most recent month for testing, since we are forecasting a new month

#split into 2 categories----
newCasesTrain <- timeDataTrain[, 1] #training for new cases
newCasesTest <- timeDataTest[, 1] #testing for new cases
 
newDeathsTrain <- timeDataTrain[, 3] #training for new deaths
newDeathsTest <- timeDataTest[, 3] #testing for new deaths

#model fitting and evaluation----
#naive method for benchmark
newCasesNaive <- naive(newCasesTrain, h=length(newCasesTest)) 
newDeathsNaive <- naive(newDeathsTrain, h=length(newDeathsTest))
newCasesnaiveMape <- MAPE(newCasesNaive$mean, newCasesTest) * 100
newDeathsnaiveMape <- MAPE(newDeathsNaive$mean, newDeathsTest) * 100

#State Space Models 
newCasesEts <- ets(newCasesTrain)
newDeathsEts <- ets(newDeathsTrain)
newCasesEtsMape <- MAPE(as.data.frame(forecast(newCasesEts, h=31))$`Point Forecast`, newCasesTest) * 100
newDeathsEtsMape <- MAPE(as.data.frame(forecast(newDeathsEts, h=31))$`Point Forecast`, newDeathsTest) * 100

#tbats
newCasesTbats <- tbats(newCasesTrain)
newDeathsTbats <- tbats(newDeathsTrain)
newCasesTbatsMape <- MAPE(as.data.frame(forecast(newCasesTbats, h=31))$`Point Forecast`, newCasesTest) * 100
newDeathsTbatsMape <- MAPE(as.data.frame(forecast(newDeathsTbats, h=31))$`Point Forecast`, newDeathsTest) * 100

#summarizing results
data.frame(
  Type = c("New cases", "New deaths","New cases", "New deaths", "New cases", "New deaths"),
  Model = c("Naive", "Naive", "ETS", "ETS", "TBATS", "TBATS"),
  MAPE = c(newCasesnaiveMape, newDeathsnaiveMape, newCasesEtsMape, newDeathsEtsMape, newCasesTbatsMape, newDeathsTbatsMape)
)

#forecast and plotting----
#best model = tbats
newCasesForecastTbats <- tbats(timeData$New_cases) %>% #fit tbats model on whole data for new cases
  forecast(h=31)  #predict 31 days for june

newDeathsForecastTbats <- tbats(timeData$New_deaths) %>% #fit tbats model on whole data for new deaths
  forecast(h=31) #predict 31 days for june

newCasesForecast <- as.data.frame(newCasesForecastTbats)$`Point Forecast` #extract predicted value for new cases
newDeathsForecast <- as.data.frame(newDeathsForecastTbats)$`Point Forecast` #extract predicted value for new deaths

newForecast <- data.frame( 
  Date = seq(as.Date("2022-06-01"), as.Date("2022-07-01"), by = 1), #column for June days
  New_cases = as.integer(newCasesForecast), #column for predicted values of new cases
  New_deaths = as.integer(newDeathsForecast) #column for predicted value of new deaths
  ) %>%
  {.[-31, ]} #remove 2022-07-01 row

#plot for new cases
(newCasesPlot <- ggplot() + #current data
  geom_line(data = cleanData, aes(x = Date_reported, y = New_cases, colour = "black")) +
  geom_line(data = newForecast, aes(x = Date, y = New_cases, colour = "red")) + #predicted data 
  labs(title = "New COVID-19 Cases of Indonesia from 2020-03-02 till 2022-06-30 (Predicted)",
       x = "Date",
       y = "New cases") +
  scale_color_manual(name = "", values = c("black","red"), labels = c("actual", "predicted")))

#zoom in on prediction
newCasesPlot + coord_cartesian(
  xlim = as.Date(c("2022-05-31", "2022-06-30")),
  ylim = c(0, 500))
  
#plot for new deaths
(newDeathsPlot <- ggplot() + #current data
  geom_line(data = cleanData, aes(x = Date_reported, y = New_deaths, colour = "black")) +
  geom_line(data = newForecast, aes(x = Date, y = New_deaths, colour = "red")) + #predicted data
  labs(title = "New COVID-19 Deaths of Indonesia from 2020-03-02 till 2022-06-30 (Predicted)",
       x = "Date",
       y = "New deaths") +
  scale_color_manual(name = "", values = c("black","red"), labels = c("actual", "predicted")))

#zoom in on prediction
newDeathsPlot + coord_cartesian(
  xlim = as.Date(c("2022-05-31", "2022-06-30")),
  ylim = c(0, 10))