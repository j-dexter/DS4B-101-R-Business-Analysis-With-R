# DS4B 101-R ----
# CUSTOMER SEGMENTATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)
library(plotly)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 2.0 HEAT MAP PLOT ----
# - Refer to Chapter 4: Advanced Heat Map
plot_customer_heatmap <- function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}

plot_customer_heatmap()
plot_customer_heatmap(interactive = FALSE)

# 3.0 CUSTOMER SEGMENTATION PLOT ----
get_customer_segments <- function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    
    
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    
    
    # 3.0 UMAP
    
    
    # 4.0 COMBINE UMAP & K-MEANS
    
    
}

plot_customer_segments <- function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}

plot_customer_segments(k = 4, seed = 123, interactive = TRUE)
plot_customer_segments(k = 4, seed = 123, interactive = FALSE)


# 4.0 VISUALIZE CUSTOMER BEHAVIOR ----

plot_customer_behavior_by_cluster <- function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}

plot_customer_behavior_by_cluster(top_n_products = 10, 
                                  k = 4, seed = 123,
                                  interactive = TRUE)

plot_customer_behavior_by_cluster(top_n_products = 10, 
                                  k = 4, seed = 123,
                                  interactive = FALSE)

# 5.0 SAVE FUNCTIONS ----

function_names <- c("get_customer_segments", "plot_customer_segments",
                    "plot_customer_heatmap", "plot_customer_behavior_by_cluster")

dump(function_names, file = "00_scripts/plot_customer_segmentation.R")
