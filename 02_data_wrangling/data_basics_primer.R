# DATA BASICS ----

library(tidyverse)


# Data Types ----

?c # used to create vectors

a <- c(1, 2, 3)

a %>% class()

b <- c("low", "medium", "high")

b %>% class()

# Common Data Types:
    # 1) Numeric (interger, double)
    # 2) Character (text)
    # 3) Factor (categorical)
    # 4) Boolean (true/false)

# Data Structures ----

#* think of these as the scafholding that holds the data together.

# tibble: a modern version of a df that holds vectors of various data types
    # in a format designed for analysis.

# store our data currently in seperate vectors in a tibble data structure
ab_tbl <- tibble(a, b)

# KEY CONCEPT
# tibble is a special data structure that holds data of different data types.

read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

