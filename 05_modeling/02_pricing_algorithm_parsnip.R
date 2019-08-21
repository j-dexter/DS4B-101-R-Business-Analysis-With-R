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


# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----



# 3.1.2 Feature Importance ----


# 3.1.3 Function to Calculate Metrics ----



# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----


# 3.2.2 Feature importance ----



# 3.3 PENALIZED REGRESSION ----

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet




# 3.3.2 Feature Importance ----




# 4.0 TREE-BASED METHODS ----

# 4.1 DECISION TREES ----

# 4.1.1 Model ----
?decision_tree
?rpart::rpart



# 4.1.2 Decision Tree Plot ----







# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()
?ranger::ranger



# 4.2.2 ranger: Feature Importance ----




# 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest



# 4.2.4 randomForest: Feature Importance ----




# 4.3 XGBOOST ----

# 4.3.1 Model ----
?boost_tree
?xgboost::xgboost



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




