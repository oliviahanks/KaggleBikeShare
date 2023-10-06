setwd('C:/Users/olivi/OneDrive/Documents/School2023/KaggleBikeShare')

##                        ##
## Bike Share Recipe Code ##
##                        ##

## libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(poissonreg)

## Read in the Data
bike_train <- vroom("./train.csv")
bike_test <- vroom("./test.csv")


## Data Cleaning
# remove unecessary columns
bike_train <- bike_train %>%
  select(-casual,-registered)

## Feature Engineering
my_recipe <- recipe(count ~ ., data=bike_train) %>% # Set model formula and dataset
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("clear","misty","raining"))) %>%
  # also changed binary to "Yes" and "No"
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_time(datetime, features=c("hour", "minute")) #create time variable
  
prepped_recipe <- prep(my_recipe) 
bake(prepped_recipe, new_data = bike_test)

<<<<<<< HEAD
=======
# New Data
#new_bike_test <- bake(prepped_recipe, new_data = bike_test)
#new_bike_train <- bake(prepped_recipe, new_data = bike_train)
>>>>>>> 0544ee5d2a016cb1d07b851d1eaf6219fb7b3f3c

### Linear Regression ###
my_mod <- linear_reg() %>% #Type of model
set_engine("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_train) # Fit the workflow

bike_predictions <- predict(bike_workflow,new_data=bike_test) # Use fit to predict

<<<<<<< HEAD
# for more info
=======

>>>>>>> 0544ee5d2a016cb1d07b851d1eaf6219fb7b3f3c
#extract_fit_engine(bike_workflow) %>% tidy()
#extract_fit_engine(bike_workflow) %>% summary()

# round predictions less than 0 to 0
round_bike_pred <- bike_predictions
round_bike_pred[round_bike_pred < 0] <- 0

SampleSubmission <- bike_test
SampleSubmission$count <- round_bike_pred$.pred
SampleSubmission <- SampleSubmission[c('datetime', 'count')]

# Save File
#vroom_write(SampleSubmission, "./SampleSubmission.csv", delim = ',')


<<<<<<< HEAD
### Poisson Regression ###
=======
## Poisson Regression
>>>>>>> 0544ee5d2a016cb1d07b851d1eaf6219fb7b3f3c
pois_mod <- poisson_reg() %>% #Type of model
set_engine("glm") # GLM = generalized linear model

bike_pois_workflow <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(pois_mod) %>%
fit(data = bike_train) # Fit the workflow

bike_predictions <- predict(bike_pois_workflow, new_data=bike_test) # Use fit to predict

test_preds <- bike_predictions %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
#vroom_write(x=test_preds, file="./PoissonSubmission.csv", delim=",")



## Penalized Regression
## Create a recipe
## Feature Engineering

## Transform to log(count) - I can only do this on train set because
## test set does not have count.  Hence, I am doing this outside of recipe
## because I only apply this to the train set
bike_train_log <- bike_train %>%
  mutate(count=log(count))

my_recipe_pen <- recipe(count ~ ., data=bike_train_log) %>% # Set model formula and dataset
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("clear","misty","raining"))) %>%
  # also changed binary to "Yes" and "No"
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_normalize(all_numeric_predictors())

## Penalized regression model
preg_model <- linear_reg(penalty=.1, mixture=.1) %>% #Set model and tuning
set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
add_recipe(my_recipe_pen) %>%
add_model(preg_model) %>%
fit(data=bike_train_log)
predict(preg_wf, new_data=bike_test)

test_preds <- predict(preg_wf, new_data=bike_test) %>%
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
#vroom_write(x=test_preds, file="./PenalizedSubmission.csv", delim=",")



### Tuning Parameters
## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
mixture=tune()) %>% #Set model and tuning
set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe_pen) %>%
add_model(preg_model)

## Grid of values to tune over
tuning_grid <- grid_regular(penalty(),
mixture(),
levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(bike_train_log, v = 5, repeats=1)

## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
grid=tuning_grid,
metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("rmse")

## Finalize the Workflow & fit it
final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=bike_train_log)

## Predict
final_wf %>%
predict(new_data = bike_test)

test_preds <- final_wf %>%
  predict(new_data = bike_test) %>%
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
#vroom_write(x=test_preds, file="./TunePenSubmission.csv", delim=",")


## Growing Trees
my_mod <- decision_tree(tree_depth = tune(),
cost_complexity = tune(),
min_n=tune()) %>% #Type of model
set_engine("rpart") %>% # Engine = What R function to use
set_mode("regression")

## Create a workflow with model & recipe
my_recipe_tree <- recipe(count ~ ., data=bike_train_log) %>% # Set model formula and dataset
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("clear","misty","raining"))) %>%
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>%
  step_rm(datetime) 

tree_wf <- workflow() %>%
add_recipe(my_recipe_tree) %>%
add_model(my_mod)

## Set up grid of tuning values
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n()) ## L^2 total tuning possibilities

## Set up K-fold CV
folds <- vfold_cv(bike_train_log, v = 5, repeats=1)

## Run the CV
CV_results_tree <- tree_wf %>%
tune_grid(resamples=folds,
grid=tuning_grid,
metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Find best tuning parameters
bestTune <- CV_results_tree %>%
select_best("rmse")

## Finalize workflow and predict
final_wf <-
tree_wf %>%
finalize_workflow(bestTune) %>%
fit(data=bike_train_log)

## Predict
final_wf %>%
predict(new_data = bike_test)

test_preds <- final_wf %>%
  predict(new_data = bike_test) %>%
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
#vroom_write(x=test_preds, file="./TreeSubmission.csv", delim=",")



### Random Forest

RF_mod <- rand_forest(mtry = tune(),
min_n=tune(),
trees=500) %>% #Type of model (500 or 1000)
set_engine("ranger") %>% # What R function to use
set_mode("regression")

## Create a workflow with model & recipe
# use my_recipe_tree
RF_wf <- workflow() %>%
add_recipe(my_recipe_tree) %>%
add_model(RF_mod)

## Set up grid of tuning values
RF_tuning_grid <- grid_regular(mtry(range = c(1, 9)),
                               min_n())

## Set up K-fold CV
# use folds from previous example

## Run the CV
CV_results_RF <- RF_wf %>%
tune_grid(resamples=folds,
grid=RF_tuning_grid,
metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Find best tuning parameters
bestTune <- CV_results_RF %>%
select_best("rmse")

## Finalize workflow and predict
final_wf <-
RF_wf %>%
finalize_workflow(bestTune) %>%
fit(data=bike_train_log)

## Predict
final_wf %>%
predict(new_data = bike_test)

test_preds <- final_wf %>%
  predict(new_data = bike_test) %>%
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./RFSubmission.csv", delim=",")


