# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# REGRESSION MODELS ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# LIBRARIES & DATA ----

pkgs <- c("parsnip", "glmnet", "rpart", "rpart.plot", "ranger", "randomForest", "xgboost", "kernlab")
# If any of these packages are not installed, run this: install.packages(pkgs)

# Standard
library(readxl)
library(tidyverse)
library(tidyquant)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Source Scripts
source("00_scripts/separate_bikes_and_outlier_detection.R")

# Read Data
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl %>%
    select(total_price, model, category_2, frame_material) %>%
    
    group_by(model, category_2, frame_material) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(total_sales))

model_sales_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(total_sales, .fun = max) %>% 
               fct_rev()) %>%
    
    ggplot(aes(frame_material, total_sales)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    #coord_flip() +
    facet_wrap(~ category_2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
    theme_tq() +
    labs(
        title = "Total Sales for Each Model",
        x = "Frame Material", y = "Revenue"
    )


# 2.0 TRAINING & TEST SETS ----

bike_features_tbl <- bike_orderlines_tbl %>% 
    
    # choose features that might be predictive
    select(price, model, category_2, frame_material) %>% 
    
    # get unique instances of these combinations
    distinct() %>% 
    
    # add id column to track records during train/test split
    mutate(id = row_number()) %>% 
    
    select(id, everything()) %>% 
    
    # source and use function to add flags
    separate_bike_model(keep_model_column = T, append = T)

bike_features_tbl

# set seed for train/test data split
set.seed(seed = 1113)

# using strata on 'model_base' to ensure the 'training' data has ALL
    # possibilities from that category.
    # this is important b/c it has only 18.
    # worse case is to not have categories in the 'training data' 
        # and then they show up in the 'test data' (will give bad predicitons)
        # b/c the trainied ML algorithm will not have seen this yet
split_obj <- rsample::initial_split(bike_features_tbl, prop = 0.80, strata = "model_base")

# you can see here that 'training data' captured all 18
split_obj %>% training() %>% distinct(model_base)

# here test only has AND thats fine b/c these were in algo training
split_obj %>% testing() %>% distinct(model_base)

# split and save train/test data
train_tbl <- split_obj %>% training()
test_tbl  <- split_obj %>% testing()

# 3.0 LINEAR METHODS ----
?linear_reg
?set_engine
?fit
?predict.model_fit()
?metrics

# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----

# Three Step Process

# 1 Create a model: linear_reg()
model_01_linear_lm_simple <- linear_reg(mode = "regression") %>% 
    
    # 2 Set an engine
    set_engine("lm") %>% 
    
    # 3 Fit the model to data
    fit(price ~ category_2 + frame_material, train_tbl)


#* MODEL METRICS: We calculate model metrics comparing the test
    # data predicitons with the actual values to get a 
    # baseline model performance.


# NOW LETS ANSWER: How different are the actual/predictions (on average)
    # 1) Mean Absolute Error (MAE): Absolute value of residuals
            # generates the magnitude of error.
            # take the average to get the avg. error.
    # 2) Root Mean Squared Error (RMSE): Square the residuals to remove
            # negative sign. Take the average.
            # take the square root of the avg. to return to units
                # of initial error term.

# use model to make predicitons on test-data (long form method)
model_01_linear_lm_simple %>% 
    
    # get predicitons
    predict(new_data = test_tbl) %>% 
    
    # add column with actual price-values
    bind_cols(test_tbl %>% select(price)) %>% 
    
    # get residuals: diff between actual (truth) and
    # prediciton (estimate from the model)
    mutate(residuals = price - .pred) %>% 
    
    # calculate MAE
    summarize(
        mae  = abs(residuals) %>% mean(),
        rmae = mean(residuals^2)^0.5 # squares, takes average, then converts back
    )


# use model to make predicitons on test-data (short-form w/yardstick)
model_01_linear_lm_simple %>% 
    
    # get predicitons
    predict(new_data = test_tbl) %>% 
    
    # add column with actual price-values
    bind_cols(test_tbl %>% select(price)) %>% 
    
    yardstick::metrics(truth = price, estimate = .pred)


# 3.1.2 Feature Importance ----
model_01_linear_lm_simple

model_01_linear_lm_simple$fit %>% class()

model_01_linear_lm_simple$fit %>% 
    broom::tidy() %>% 
    
    # arrange by importance using p-values
    arrange(p.value) %>% 
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    
    # plot feature importance
    ggplot(aes(x = estimate, y = term)) +
    geom_point(size = 3) +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 4) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 01: Simple lm model") +
    theme_tq()
    

# 3.1.3 Function to Calculate Metrics ----

#* here we are creating a funciton to recreate section 3.1.1

model_01_linear_lm_simple %>% 
    # get predicitons
    predict(new_data = test_tbl) %>%
    # add column with actual price-values
    bind_cols(test_tbl %>% select(price)) %>% 
    yardstick::metrics(truth = price, estimate = .pred) 


#* Function to expedite the Metrics we will be Calculating
    # these are the metrics for evaluating ML methods
    # used to fit data and how the compare to each other.
calc_metrics <- function(model, new_data = test_tbl){
    
    model %>% 
        # get predicitons
        predict(new_data = new_data) %>%
        # add column with actual price-values
        bind_cols(new_data %>% select(price)) %>% 
        yardstick::metrics(truth = price, estimate = .pred)
}

model_01_linear_lm_simple %>% calc_metrics(test_tbl)
model_01_linear_lm_simple %>% calc_metrics(train_tbl)


# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

#* Can we reduce our model metrcis: RMSE +/or MAE

# 3.2.1 Model ----
train_tbl

# fit model: price ~ . (the dot tells parsnip to fit a model as a
    # funciton of all the predictor columns)
model_02_linear_complex <- linear_reg("regression") %>% 
    set_engine("lm") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_02_linear_complex %>% calc_metrics(new_data = test_tbl)

# 3.2.2 Feature importance ----

model_02_linear_complex$fit %>% 
    broom::tidy() %>% 
    
    # arrange by importance using p-values
    arrange(p.value) %>% 
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    
    # plot feature importance
    ggplot(aes(x = estimate, y = term)) +
    geom_point(size = 3) +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 4) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 02: Complex lm model") +
    theme_tq()

# 3.3 PENALIZED REGRESSION ----

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

model_03_linear_glmnet <- linear_reg(mode = "regression", penalty = 100, mixture = 0.25) %>% 
    set_engine("glmnet") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_03_linear_glmnet %>% calc_metrics(test_tbl)

# 3.3.2 Feature Importance ----

#* the estimate col is the important one for looking at
    # feature importance. Like regular linear regression,
    # these values in 'estiamte' are interpretable.

model_03_linear_glmnet$fit %>% 
    # arrange by estimate (instead of using p-values)
    broom::tidy() %>% 
    filter(lambda >= 10 & lambda < 11) %>% 
    arrange(desc(abs(estimate))) %>% 
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    
    # plot feature importance
    ggplot(aes(x = estimate, y = term)) +
    geom_point(size = 3) +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 03: GLMNET lm model") +
    theme_tq()


# 4.0 TREE-BASED METHODS ----

#* SOME QUICK NOTES ON TREE-BASED METHODS
    # 1) Naturally incorporate non-linear relationships/interactions
    # 2) Minimal preprocessing required (no standardization, etc.)
    # 3) downside: while decision trees are VERY explainable,
            # Random Forest & XGBoost has low explainability.
    # 4) Random Forest & XGBoost (we can still get feature importance)
            # Pro: High Performance
            # Con: Less Explainability


# Tree-Based Methods
    # Just start with a Question:
        # e.g., 

# 4.1 DECISION TREES ----

# 4.1.1 Model ----
?decision_tree
?rpart::rpart

model_04_decision_tree <- decision_tree(mode = "regression", 
                                        cost_complexity = 0.001, 
                                        tree_depth      = 7, 
                                        min_n           = 10) %>% 
    set_engine("rpart") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_04_decision_tree %>% calc_metrics(test_tbl)

# 4.1.2 Decision Tree Plot ----


# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()
?ranger::ranger

set.seed(1234)
model_05_rand_forest_ranger <- rand_forest(
    mode = "regression", mtry = 8, trees = 2000, min_n = 20) %>% 
    set_engine("ranger", replace = T, splitrule = "extratrees", importance = "impurity") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)

# 4.2.2 ranger: Feature Importance ----

#* We cannot put dollar values to explain what value an important
    # feature has, but we can still get importance (and great accuracy)

model_05_rand_forest_ranger$fit %>% 
    ranger::importance() %>% 
    enframe() %>% 
    arrange(desc(value)) %>% 
    mutate(name = as_factor(name) %>% fct_rev()) %>% 
    
    ggplot(aes(value, name)) +
    geom_point() +
    labs(title = "ranger: Variable Importance",
         subtitle = "Model 05: Ranger Forest Model")

# 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest

set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest("regression") %>% 
    set_engine("randomForest") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_06_rand_forest_randomForest %>% calc_metrics(test_tbl)

# 4.2.4 randomForest: Feature Importance ----

model_06_rand_forest_randomForest$fit %>% 
    randomForest::importance() %>% 
    as_tibble(rownames = "name") %>% 
    arrange(desc(IncNodePurity)) %>% 
    mutate(name = as_factor(name) %>% fct_rev()) %>% 
    
    ggplot(aes(IncNodePurity, name)) +
    geom_point() +
    labs(title = "randomForest: Variable Importance",
         subtitle = "Model 06: Random Forest Model")


# 4.3 XGBOOST ----

# 4.3.1 Model ----
?boost_tree
?xgboost::xgboost

set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
    mode = "regression",
    learn_rate = 0.2) %>% 
    set_engine("xgboost") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)

# 4.3.2 Feature Importance ----





# 5.0 TESTING THE ALGORITHMS OUT ----

bike_features_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(price)) %>%
    
    ggplot(aes(category_2, price)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    coord_flip() +
    facet_wrap(~ frame_material) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    labs(
        title = "Unit Price for Each Model",
        y = "", x = "Category 2"
    )

# 5.1 NEW JEKYLL MODEL ----

new_over_mountain_jekyll <- tibble(
    model = "Jekyll Al 1",
    frame_material = "Aluminum",
    category_2 = "Over Mountain",
    model_base = "Jekyll",
    model_tier = "Aluminum 1",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----



# Tree-Based Methods ----






# 5.2 NEW TRIATHALON MODEL ----

new_triathalon_slice_tbl <- tibble(
    model = "Slice Al 1",
    frame_material = "Aluminum",
    category_2 = "Triathalon",
    model_base = "Slice",
    model_tier = "Ultegra",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----


# Tree-Based Methods ----






# 6.0 ADDITIONAL ADVANCED CONCEPTS ----

# - CLASSIFICATION - Binary & Multi-Class
# - ADVANCED ALGORITHMS
#   - SVMs - svm_poly() and svm_rbf() - Must be normalized
#   - Neural Networks - keras - Must be normalized
#   - Stacking Models 
# - PREPROCESSING - recipes 
# - HYPERPARAMETER TUNING - purrr
# - SAMPLING & CROSS VALIDATION - rsample 
# - AUTOMATIC MACHINE LEARNING - H2O




