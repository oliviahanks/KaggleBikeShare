setwd('C:/Users/olivi/OneDrive/Documents/School2023/KaggleBikeShare')

##
## Bike Share Recipe Code
##

## libraries
library(tidyverse)
library(tidymodels)
library(vroom)

## Read in the Data
bike_train <- vroom("./train.csv")
bike_test <- vroom("./test.csv")

## Data Cleaning
# 
bike_train <- bike_train %>%
  select(-casual,-registered)

## Feature Engineering
my_recipe <- recipe(count ~ ., data=bike_train) %>% # Set model formula and dataset
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("clear","misty","raining"))) %>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_time(datetime, features=c("hour", "minute")) #create time variable
  
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet 
bake(prepped_recipe, new_data = bike_test)

# New Data
new_bike_test <- bake(prepped_recipe, new_data = bike_test)
new_bike_train <- bake(prepped_recipe, new_data = bike_train)


## Linear Regression
my_mod <- linear_reg() %>% #Type of model
set_engine("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_train) # Fit the workflow

bike_predictions <- predict(bike_workflow,new_data=bike_test) # Use fit to predict

# round predictions less than 0 to 0
round_bike_pred <- bike_predictions
round_bike_pred[round_bike_pred < 0] <- 0

SampleSubmission <- bike_test
SampleSubmission$count <- round_bike_pred$.pred
SampleSubmission <- SampleSubmission[c('datetime', 'count')]

# Save File
vroom_write(SampleSubmission, "./SampleSubmission.csv", delim = ',')
