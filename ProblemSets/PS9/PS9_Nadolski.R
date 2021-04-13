### Problem Set 9
### Karley Nadolski
### April 13, 2021

library(tidyverse)
library(tidymodels)
library(magrittr)

  # Load in UCI Housing data
    housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
    names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
    
    set.seed(123456)
    
  # Split into training data and testing data
      housing_split <- initial_split(housing, prop = 0.8) # proportion of the training data is 80%, test data is 20%
      housing_train <- training(housing_split)
      housing_test  <- testing(housing_split)
      
  # Create a recipe (takes the log of housing value, converts chas to a factor, creates 6th degree polynomials of each of the continuous features)
      housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
        # convert outcome variable to logs
        step_log(all_outcomes()) %>%
        # convert 0/1 chas to a factor
        step_bin2factor(chas) %>%
        # create interaction term between crime and nox
        step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
        # create square terms of some continuous variables
        step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,lstat,dis,nox, degree=6) %>%
        # prep it
        prep()
      
      # Run the recipe
      housing_train_prepped <- housing_recipe %>% juice
      housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)
    
      # What is the dimension of the training data? 
          dim(housing_train_prepped) # The housing_train_prepped data has dimensions 405 x 75
      # How many more x variables do you have than in the original data? 
          # There are 60 more x variables after using the recipe function in the testing data (74 compared to 60)

######################################################################################################################
  # Estimate a LASSO model to predict log median housing value
      # penalty parameter lambda is tuned by 6-fold cross validation
          
          # Cross-validate the lambda
          tune_spec <- linear_reg(
            penalty = tune(), # tuning parameter
            mixture = 1       # 1 = lasso, 0 = ridge
          ) %>% 
            set_engine("glmnet") %>%
            set_mode("regression")
          
          # define a grid of 50 values over which to try different values of lambda -- how refined is lambda going to be? 
          lambda_grid <- grid_regular(penalty(), levels = 50)
          
          # 6-fold cross-validation
          rec_folds <- vfold_cv(housing_train_prepped, v = 6)
          
          # Workflow
          rec_wf <- workflow() %>%
            add_model(tune_spec) %>%
            add_formula(log(medv) ~ .)
          
          # Tuning results
          rec_res <- rec_wf %>%
            tune_grid(
              resamples = rec_folds,
              grid = lambda_grid
            )
          
          top_rmse  <- show_best(rec_res, metric = "rmse")
          best_rmse <- select_best(rec_res, metric = "rmse")
          # Now train with tuned lambda
          final_lasso <- finalize_workflow(rec_wf,
                                           best_rmse
          )
          
          # Print out results
          last_fit <- last_fit(final_lasso,housing_split) %>%
            collect_metrics() %>% print
          best_rmse
          
              # rmse = 0.22 (out-of-sample rmse)
              # rsq = 0.774
              # best penalty = 0.00356
          
#######################################################################################################################
    # Estimate a ridge regression model to predict log median housing value
       # penalty parameter lambda is tuned by 6-fold cross validation
          
          # Cross-validate the lambda
          tune_spec <- linear_reg(
            penalty = tune(), # tuning parameter
            mixture = 0       # 1 = lasso, 0 = ridge
          ) %>% 
            set_engine("glmnet") %>%
            set_mode("regression")
          
          # define a grid of 50 values over which to try different values of lambda -- how refined is lambda going to be? 
          lambda_grid <- grid_regular(penalty(), levels = 50)
          
          # 6-fold cross-validation
          rec_folds <- vfold_cv(housing_train_prepped, v = 6)
          
          # Workflow
          rec_wf <- workflow() %>%
            add_model(tune_spec) %>%
            add_formula(log(medv) ~ .)
          
          # Tuning results
          rec_res <- rec_wf %>%
            tune_grid(
              resamples = rec_folds,
              grid = lambda_grid
            )
          
          # What's the best value of lambda? 
          top_rmse  <- show_best(rec_res, metric = "rmse")
          best_rmse <- select_best(rec_res, metric = "rmse")
          final_ridge <- finalize_workflow(rec_wf,
                                           best_rmse
          )
          
          # Print out results
          last_fit(final_ridge,housing_split) %>%
            collect_metrics() %>% print
          best_rmse
          
          # rmse = 0.218
          # rsq = 0.784
          # best penalty = 0.0233