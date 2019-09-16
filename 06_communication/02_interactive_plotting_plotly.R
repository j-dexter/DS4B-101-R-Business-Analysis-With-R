# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT


# LIBRARIES & DATA ----

# Main
library(tidyverse)
library(lubridate)

# Visualization
library(tidyquant)
library(plotly)


bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 1.0 TOTAL SALES BY MONTH ----

# 1.1 Preparing Time Series Data ----


# Formatting Dates
# - strftime: https://devhints.io/strftime



# 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature


# Step 2: Use ggplotly()




# 1.3 Plot Total Sales Function ----



# 1.4 Test Our Function ----






# 2.0 CATEGORY 2 SALES BY MONTH ----

# 2.1 Preparing Time Series Data ----


# 2.2 Interactive Plot ----



# Step 2: Use ggplotly()



# 2.3 Plot Categories Function ----



# 2.4 Test Our Function ----




# 3.0 SAVE FUNCTIONS ----






