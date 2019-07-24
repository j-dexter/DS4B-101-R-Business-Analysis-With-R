# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths_tbl <- fs::dir_info("00_data/bike_sales/data_raw/")

paths_chr <- excel_paths_tbl %>% 
    pull(path)

# What Not To Do: Don't use for loops
excel_list_1 <- list()

for (path in paths_chr) {
    excel_list_1[[path]] <- read_excel(path)
}

excel_list_1

# What to Do: Use map()
#* here we are replicating what the for-loop did
?map

# Method 1: using function name - read_excel()
excel_list_2 <- paths_chr %>% 
    map(read_excel) %>% 
    set_names(paths_chr)

# Method 2: using Anonymous function
#* anonymous functions just provide shorthand methods to create funcitons w/out typing 'function(x)'
paths_chr %>% 
    map(~ read_excel(.)) %>% 
    set_names(paths_chr)

# Method 3: Function specified with function(
paths_chr %>% 
    map(function(x) read_excel(path = x)) %>% 
    set_names(paths_chr)

# Reading Excel Sheets

excel_sheets("00_data/bike_sales/data_raw/bikes.xlsx") %>% 
    map(~ read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx", sheet = .)) 


# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----

#* a tibble is a LIST + so we can iterate over it

bike_orderlines_tbl %>% 
    map(~ class(.)[1]) # using [1] b/c date has two classes

# 2.2 Map Variants ----

#* the output that I want, determines the 'map variant' i use
#* E.g. the 'character map' (map_chr) converts the output to a chr vector
#* whereas the 'data frame map' (map_df) converts output to a df/tibble

?map

# Character map
#* named character vector
bike_orderlines_tbl %>% 
    map_chr(~ class(.)[1])

# Data Frame map
#* tibble format
bike_orderlines_tbl %>% 
    map_df(~ class(.)[1]) %>% 
    gather()

bike_orderlines_tbl %>% 
    map_df(~ sum(is.na(.)) / length(.)) %>% 
    gather()


# 2.3 Row-wise Map ----

# use paths to data to read in ALL data into a nested tibble
#* this puts all excel tables into a tibble
excel_tbl <- excel_paths_tbl %>% 
    select(path) %>% 
    mutate(data = path %>% map(read_excel))

# view excel tables in a list
excel_list_1

# view excel tables nested into a tibble
excel_tbl

# 3.0 NESTED DATA ----

# Unnest
excel_tbl

excel_tbl$data

excel_tbl$data[[2]]

# unnest(): unnests a nested df converting tibbles burried w/in
    # list-columns to a sing level tibble.

# Unnest DFs into Single Level Tbl
excel_tbl_unnested <- excel_tbl %>% 
    unnest(data, .id = "ID")

# Nest
excel_tbl_nested <- excel_tbl_unnested %>% 
    group_by(ID, path) %>% 
    nest()

excel_tbl_nested$data

# Mapping Nested List Columns

# repeat NA 5x
x <- rep(NA, 5)
x

# Are all values true?
is.na(x) %>% all()
!is.na(x) %>% all()

#* apply to one tbl/row
excel_tbl_nested$data[[1]] %>% 
    
    # select columns that are NOT full of NA values
    select_if(~ !is.na(.) %>% all())

#* Now lets make it work on ALL rows: scaling the process

# Method 1: Creating a function outside of purr:map()

data <- excel_tbl_nested

# Step 1: Create function that can be mapped to ONE element
# get process to work on ONE ELEMENT
select_non_na_columns <- function(data) {
    
    data %>% 
        select_if(~ !is.na(.) %>% all())
    
}

# Step 2: Extract an element, and test function
#* we now have a function that works on one element
#* it takes 1 nested tbl AND drops cols with 100% NAs
excel_tbl_nested$data[[3]] %>% 
    select_non_na_columns()

# Step 3: Scale function - Using mutate() + map() 
# fix nested tbls and put in new col (these will have NO 100% NA cols)
excel_tbl_nested_fixed <- excel_tbl_nested %>% 
    mutate(data_fixed = data %>% map(select_non_na_columns))

# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black", size = 0.5) +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))


# 4.2 Modeling Primer ----

# Data Preparation

sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>% 
    
    filter(category_2 == "Cross Country Race") %>% 
    
    select(month_end, total_price) %>% 
    
    mutate(month_end_num = as.numeric(month_end))

sales_by_m_cross_country_tbl %>% 
    
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.2, se = F)

# Making a loess model
?loess

# fit loess model to our data
fit_loess_cross_country <- sales_by_m_cross_country_tbl %>% 
    loess(total_price ~ month_end_num, data = ., span = 0.2)

fit_loess_cross_country

# Working With Broom (to extract info from fitted model)

#* here we want to get fitted results in tidy format
#* augment(): returns model fitted values, residuals, and standard errors
    #* in data frame format
fit_loess_cross_country %>% 
    broom::augment() %>% 
    
    # Visualizing results
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_line(aes(y = .fitted, color = "blue"))


# 4.3 Step 1: Function To Return Fitted Results ----

#* protip: when making functions, save some testable data as each
    # argument so you can interactively test the funciton
    # while you build it.

#* now we need to apply 3 step process for building functions

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>% 
    group_by(category_1, category_2) %>% 
    nest()

data <- rolling_avg_3_tbl_nested$data[[1]]

tidy_loess <- function(data, span = 0.2) {
    
    data_formatted <- data %>% 
        select(month_end, total_price) %>% 
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num,
                       data    = data_formatted,
                       span    = span)
    
    output_tbl <- fit_loess %>% 
        broom::augment() %>% 
        select(.fitted)
    
    return(output_tbl)
    
}


# 4.4 Step 2: Test Function on Single Element ----

#* now lets do some testing to be sure we can
    #* plug in various values to get alternate resutls.
    #* once we can do that, then we know we can iterate =)
rolling_avg_3_tbl_nested$data[[2]] %>% 
    tidy_loess()

# 4.5 Step 3: Map Function to All Categories ----

#* time to scale our function

# Map Functions

loess_tbl_nested <- rolling_avg_3_tbl_nested %>% 
    
    #* apply function to fit model BY CATEGORY
    #* apply our function using map()
    mutate(fitted = data %>% map(tidy_loess))

#* get JUST the fitted values from nested table
loess_tbl_nested$fitted[[1]]

#* now, unnest ALL nested tbls to get a merged tbl w/fitted results
#* b/c the cols/vectors are of same length, they are merged by default - Mind Blown!!!
loess_tbl_nested %>% 
    unnest()

# Visualize Results

loess_tbl_nested %>% 
    
    unnest() %>% 
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = .fitted), color = "blue", size = 2) +
    geom_smooth(method = "loess", span = 0.2) +
    facet_wrap(~ category_2, scales = "free_y")
    

