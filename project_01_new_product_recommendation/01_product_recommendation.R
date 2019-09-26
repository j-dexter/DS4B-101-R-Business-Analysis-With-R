# DS4B 101-R ----
# PRODUCT RECOMMENDATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(parsnip)
library(plotly)

source("00_scripts/separate_bikes_and_outlier_detection.R")

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

models_tbl <- read_rds("00_models/parsnip_models_tbl.rds")


# 2.0 BIKE FEATURES ----

get_bike_features <- function() {
    
    
    
}

get_bike_features()

plot_bike_features <- function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
}

plot_bike_features()
plot_bike_features(interactive = FALSE)


# 3.0 SAVE FUNCTIONS ----

function_names <- c("get_bike_features", "plot_bike_features")

dump(function_names, file = "00_scripts/plot_product_recommendation.R")
