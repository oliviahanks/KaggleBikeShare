##
## Bike Share EDA Code
##

## libraries
library(tidyverse)
library(vroom)
library(patchwork)

## Read in the Data
bike <- vroom("./train.csv")

## Columns
# datetime - hourly date + timestamp  
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals

## looking to see what kind of information we can get from slide in class
dplyr::glimpse(bike) #lists the variable type of each column
skimr::skim(bike) # nice overview of the bike
DataExplorer::plot_intro(bike) # visualization of glimpse()
DataExplorer::plot_correlation(bike)
DataExplorer::plot_bar(bike) # bar charts of all discrete variables
DataExplorer::plot_histrograms(bike) # histograms of all numerical variables
DataExplorer::plot_missing(bike) # percent missing in each column
GGally::ggpairs(bike) # 1/2 scatterplot and 1/2 correlation heat map

## looking at a few other graphs
# looking at relationships
bike %>% ggplot(mapping = aes(temp, count)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
bike %>% ggplot(mapping = aes(atemp, count)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
bike %>% ggplot(mapping = aes(humidity, count)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
bike %>% ggplot(mapping = aes(windspeed, count)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# histogram of all columns
ggplot(gather(bike), aes(value)) + 
    geom_histogram(bins = 15) + 
    facet_wrap(~key, scales = 'free_x')

# separating and isolating specific columns
bike_non_cat <- bike %>%
  select(temp, atemp, humidity, windspeed)

bike_cat <- bike %>% 
  select(holiday, season, weather, workingday)

# another look at histograms
ggplot(gather(bike_cat), aes(value)) + 
    geom_histogram(bins = 15) + 
    facet_wrap(~key, scales = 'free_x')

ggplot(gather(bike_non_cat), aes(value)) + 
    geom_histogram(bins = 15) + 
    facet_wrap(~key, scales = 'free_x')

# look at boxplots and outliers
boxplot(bike_non_cat)
boxplot(bike$count)

# checking for near 0 variance
sapply(bike, var)

## Final Plots to submit
plot1 <- DataExplorer::plot_correlation(bike) # correlation
plot2 <- DataExplorer::plot_missing(bike) # missing values
plot3 <- ggplot(gather(bike_cat), aes(value)) + 
    geom_histogram(bins = 15) + 
    facet_wrap(~key, scales = 'free_x') #distribution of categorical variables
plot4 <- bike %>% ggplot(mapping = aes(humidity, count)) + 
  geom_point() + 
  geom_smooth(se = FALSE) # example of outliers in humidity

# 4X4 plot to turn in 
(plot1 + plot2) / (plot3 + plot4)



