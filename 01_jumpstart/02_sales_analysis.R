# DS4B 101-R R FOR BUSSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel files
library(readxl)
library(writexl)


# 2.0 Importing Files ----

?read_excel()

bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----

bikes_tbl

glimpse(bikes_tbl)

bikeshops_tbl

orderlines_tbl # connective tissue between bikes and bikeshops
