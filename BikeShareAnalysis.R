setwd('C:/Users/olivi/OneDrive/Documents/School2023/KaggleBikeShare')

##                        ##
## Bike Share Recipe Code ##
##                        ##

## libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(poissonreg)
library(stacks)

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
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_time(datetime, features=c("hour", "minute")) #create time variable
  
prepped_recipe <- prep(my_recipe) 
bake(prepped_recipe, new_data = bike_test)



### Linear Regression ###
my_mod <- linear_reg() %>% #Type of model
set_engine("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_train) # Fit the workflow

bike_predictions <- predict(bike_workflow,new_data=bike_test) # Use fit to predict

# for more info
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



### Poisson Regression ###
pois_mod <- poisson_reg() %>% #Type of model
set_engine("glm") # GLM = generalized linear model

bike_pois_workflow <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(pois_mod) %>%
fit(data = bike_train)

bike_predictions <- predict(bike_pois_workflow, new_data=bike_test) # Use fit to predict

test_preds <- bike_predictions %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV
#vroom_write(x=test_preds, file="./PoissonSubmission.csv", delim=",")



### Penalized Regression ###

## Transform to log(count) - I can only do this on train set because
## test set does not have count.  Hence, I am doing this outside of recipe
## because I only apply this to the train set
bike_train_log <- bike_train %>%
  mutate(count=log(count))

# Create a Recipe/Feature Engineering
my_recipe_pen <- recipe(count ~ ., data=bike_train_log) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("clear","misty","raining"))) %>%
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_normalize(all_numeric_predictors()) # needed for penalized regression

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



### Tuning Parameters ###
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
levels = 5) ## L^2 total tuning possibilities works in a lXl grid

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


### Growing Trees ###
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



### Random Forest ###

RF_mod <- rand_forest(mtry = tune(),
min_n=tune(),
trees=500) %>% #Type of model (500 or 1000) # more is better
set_engine("ranger") %>% # What R function to use
set_mode("regression")

## Create a workflow with model & recipe
# use my_recipe_tree from tree model
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



### Model Stacking ###

# use my_recipe_tree
# use prev folds var

## control settings for stacking models
untunedModel <- control_stack_grid() # need to be tuned
tunedModel <- control_stack_resamples()


## Set up linear model
lin_model_modstack <- linear_reg() %>%
  set_engine("lm")

## Set workflow
linreg_wf_modstack <- workflow() %>%
  add_recipe(my_recipe_tree) %>%
  add_model(lin_model_modstack)

## fit linear to folds
linreg_folds_fit <- linreg_wf_modstack %>%
  fit_resamples(resamples = folds,
                control = tunedModel)


## Penalized Regression
## define the model
pen_reg_model_modstack <- linear_reg(mixture = tune(),
                                     penalty = tune()) %>%
  set_engine("glmnet")

## define a workflow
pen_reg_wf_modstack <- workflow() %>%
  add_recipe(my_recipe_tree) %>%
  add_model(pen_reg_model_modstack)

## define grid of tuning values
pen_reg_tune_grid <- grid_regular(mixture(),
                                  penalty(),
                                  levels = 7)

## fit into folds
pen_reg_fold_fit <- pen_reg_wf_modstack %>%
  tune_grid(resamples = folds,
            grid = pen_reg_tune_grid,
            metrics = metric_set(rmse),
            control = untunedModel)


## Reg Tree
## set up the model for regression trees
regtree_modstack <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # Engine = What R function to use
  set_mode("regression")


## Workflow
regTree_wf_modstack <- workflow() %>%
  add_recipe(my_recipe_tree) %>%
  add_model(regtree_modstack)

## Grid for tuning
regtree_modstack_tunegrid <- grid_regular(tree_depth(),
                                          cost_complexity(),
                                          min_n(),
                                          levels = 5)

## Tune the Model
tree_folds_fit_modstack <- regTree_wf_modstack %>%
  tune_grid(resamples = folds,
            grid = regtree_modstack_tunegrid,
            metrics = metric_set(rmse),
            control = untunedModel)


## Stacking time

bike_stack <- stacks() %>%
  add_candidates(linreg_folds_fit) %>%
  add_candidates(pen_reg_fold_fit) %>%
  add_candidates(tree_folds_fit_modstack)

fitted_bike_stack <- bike_stack %>%
  blend_predictions() %>%
  fit_members()

## Predictions
modstack_preds <- predict(fitted_bike_stack, new_data = bike_test)

## Get Predictions for test set AND format for Kaggle for cross validation
modstack_preds <- modstack_preds %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write predictions to CSV
vroom_write(x=modstack_preds, file="./stackPredictions.csv", delim=",")


### Attemps to get a score < .44
### Random Forest
RF_mod <- rand_forest(mtry = tune(),
min_n=tune(),
trees=1000) %>% #Type of model (500 or 1000)
set_engine("ranger") %>% # What R function to use
set_mode("regression")
 
## Create a workflow with model & recipe
# recipe
my_recipe_tree <- recipe(count ~ ., data=bike_train_log) %>% # Set model formula and dataset
  step_mutate(season=factor(season, levels=1:4, labels=c("spring","summer","fall", "winter"))) %>%
  step_mutate(weather=factor(weather, levels=1:4, labels=c("clear","misty","raining", "snowing"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features=c("hour")) %>% #create time variable
  step_mutate(datetime_hour = factor(datetime_hour)) %>%
  step_date(datetime, features=c("year")) %>% #create year variable
  step_mutate(datetime_year = factor(datetime_year)) %>%
  step_rm(datetime)
 
RF_wf <- workflow() %>%
add_recipe(my_recipe_tree) %>%
add_model(RF_mod)
 
## Set up grid of tuning values
RF_tuning_grid <- grid_regular(mtry(range = c(1, 9)),
                               min_n())
 
## Set up K-fold CV
folds <- vfold_cv(bike_train_log, v = 5, repeats=1)
 
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
vroom_write(x=test_preds, file="./FE3Submission.csv", delim=",") # got .40911


