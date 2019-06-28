# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----

library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

#* Most Important Skill for a Data Scientist: Cleaning & Preperation of Data (aka: wrangling)
#* dplyr cheatsheet: for data transformation AND most important in course b/c of wrangling power.

bikes_tbl

orderlines_tbl

bike_orderlines_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----

#* reduce columns to focus (use case 1)
#* reorder columns to group like features (use case 2)

# Basic select
bike_orderlines_tbl %>% 
    select(order_date, order_id, order_line)

bike_orderlines_tbl %>% 
    select(1:3)

bike_orderlines_tbl %>% 
    select(starts_with("order_"))

# Reduce columns (hmm... what do i need to visualize)
bike_orderlines_tbl %>% 
    select(order_date, total_price, category_1, category_2)

# Rearange columns
bike_orderlines_tbl %>% 
    select(bikeshop_name:state, everything())

# Select helpers

?starts_with

bike_orderlines_tbl %>% 
    select(starts_with("price"))

# pull() extracts contents of a columns (as vector)
bike_orderlines_tbl %>% 
    pull(total_price) %>% 
    mean()

# select_if() are scoped variants of select()

#* used to grab columns by data types

bike_orderlines_tbl %>% 
    select_if(is.character)

bike_orderlines_tbl %>% 
    select_if(is.numeric)

bike_orderlines_tbl %>% 
    select_if(~ !is.character(.))


# 2.0 Arranging with arrange() and desc() ----

bikes_tbl %>% 
    select(model, price) %>% 
    arrange(desc(price))


# 3.0 Filtering Rows with filter() ----

#* filter() extracts rows that meet logical criteria

# 3.1 filter(): formula filtering ----

# basic filter
bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > mean(price))

# filter with Or pipe |
bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 500 | (price < 1000)) %>% 
    arrange(desc(price))

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 6000,
           model %>% str_detect("Supersix"))

# use %in% for more than one category
bike_orderlines_tbl %>% 
    filter(category_2 %in% c("Over Mountain", "Trail", "Endurance Road"))

bike_orderlines_tbl %>% 
    filter(category_2 == "Over Mountain")

# negate specific categories with the ! character
bike_orderlines_tbl %>% 
    filter(!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))


# 3.2 slice(): filtering with row number(s) ----

#* slice() returns rows using row numberr

bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice(1:5)

bikes_tbl %>% 
    arrange(price) %>% 
    slice(1:5)

# retrieving the last rows
bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice((nrow(.)-4):nrow(.))

# 3.3 distinct(): unique values

bike_orderlines_tbl %>%
    distinct(category_1)

bike_orderlines_tbl %>% 
    distinct(category_1, category_2) %>% 
    arrange(category_1)

# get distinct custoemrs names and their locations
bike_orderlines_tbl %>% 
    distinct(bikeshop_name, city, state)
    
    
# 4.0 Adding columns with mutate() ----

bike_orderlines_prices <- bike_orderlines_tbl %>% 
    select(order_date, model, quantity, price) %>% 
    mutate(total_price = quantity * price)

bike_orderlines_prices

# Overwrite column
bike_orderlines_prices %>% 
    mutate(total_price = log(total_price))
           
# Transformations
bike_orderlines_prices %>% 
    mutate(total_price_log  = log(total_price),
           total_price_sqrt = total_price*0.5)

# Adding flag and filtering on flag
bike_orderlines_prices %>% 
    mutate(is_supersix = model %>% str_to_lower %>% str_detect("supersix")) %>% 
    filter(is_supersix)

# Binning with ntile()

#* binning is useful for grouping into cohorts and detecting relationshops w/in cont. variables.

bike_orderlines_prices %>% 
    mutate(total_priced_binned = ntile(total_price, 4))

# case_when() - more flexible binning w/if-else framework

#* progressively uses an if/then approach

# numeric to categorical
bike_orderlines_prices %>% 
    mutate(total_price_binned = ntile(total_price, 3)) %>% 
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price, 0.66) ~ "High",  
        total_price > quantile(total_price, 0.33) ~ "Medium",
        TRUE ~ "Low")) # catch all for others

# text to categorical
bike_orderlines_prices %>% 
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))

# 5.0 Grouping & Summarizing with group_by() and summarize() ----

#* aggregating data: to be effective, you must be able to aggregate
    # and to group on categories in a data set. this is at the hear
    # of working w/transactional data.

#* summarize() enables an aggregation function to be applied to a column

# summarize() basics

# low granulariy summary
bike_orderlines_tbl %>% 
    summarise(
        revenue = sum(total_price)
    )

# slightly higher granularity b/c more detail present
bike_orderlines_tbl %>% 
    group_by(category_1) %>% 
    summarise(revenue = sum(total_price))
    
# even higher granularity by adding additional grouping
bike_orderlines_tbl %>% 
    group_by(category_1, category_2) %>% 
    summarise(revenue = sum(total_price)) %>% 
    arrange(desc(revenue))

# get more business insights by adding a new grouping variable
bike_orderlines_tbl %>% 
    group_by(category_1, category_2, frame_material) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(revenue))

# Summary Functions

# get count/frequency/mean/median/etc
bike_orderlines_tbl %>% 
    group_by(category_1, category_2) %>% 
    summarize(
        count = n(),
        avg   = mean(total_price),  # compare w/median to assess skewness/variance
        med   = median(total_price), # allows us to quickly see if data is skewed 
        min   = min(total_price),
        max   = max(total_price)
    ) %>% 
    ungroup() %>% 
    arrange(desc(count))

# summarize_all() - detect missing values

#* one of the scoped variants for applying operations to selected columns

bike_orderlines_missing <- bike_orderlines_tbl %>% 
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

#* most data sets are dirty: you need to be able to detect the
    # number of missing values and percentage of missing values.
    # summarize_all() is great for this

#* is.na() returns a vector of TRUE/FALSE, TRUE if a value is missing (i.e. is NA)

#* Key Concept: binary vectors (TRUE/FALSE) can be summed to 
    # count the number of TRUE values.

# use anonymous function to count NAs in each column
bike_orderlines_missing %>%
    summarize_all(~ sum(is.na(.)))

#* Breaking down ~ sum(is.na(.))
    # a) this is an anonymous function used commonly in dplyr.
    # b) ~ is.na(.) would apply is.na() to all columns, 
            # returning 4 TRUE values and 16,640 FALSE values.
    # c) ~ sum(is.na(.)) - counts the number of TRUE values returning 4.
            # This is b/c TRUE = 1 and FALSE = 0 in R

#* length() returns a single value that is the length of the vector

# use anonymous function to get proportion missing in each column
bike_orderlines_missing %>% 
    summarise_all(~ sum(is.na(.)) / length(.) )

#* options for handling missing values
    # 1) filter() - remove
    # 2) tidyr package (import cheatsheet pg 2)
            # a) drop_na() - remove (filter shortcut)
            # b) fill() - replace up/down
            # c) replace_na() - replace by specifying
    # 3) advanced topic
            # a) impute - programmatically replace
                    # using 'recipes' package in 201 course

# removing NA using filter on specific column
bike_orderlines_missing %>% 
    filter(!is.na(total_price))

# 6.0 Renaming Columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----

bikeshop_revenue_tbl <- bike_orderlines_tbl %>% 
    select(bikeshop_name, category_1, total_price) %>% 
    
    group_by(bikeshop_name, category_1) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    arrange(desc(sales))

# rename() columns individually
bikeshop_revenue_tbl %>% 
    rename(
        `Bikshop Name`     = bikeshop_name,
        `Primary Category` = category_1,
        Sales              = sales
    )

# 6.2 set_names(): All columns at once ----

# manually change all names
bikeshop_revenue_tbl %>% 
    set_names(c("Bikeshop Name", "Primary Category", "Sales"))

# programmatically change all names
bikeshop_revenue_tbl %>% 
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())


# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

 
# 7.1 spread(): Long to Wide ----

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>% 
    spread(key = category_1, value = sales) %>% 
    arrange(desc(Mountain)) %>% 
    rename(`Bikeshop Name` = bikeshop_name) %>% 
    
    mutate(Mountain = scales::dollar(Mountain),
           Road     = scales::dollar(Road)
           )

bikeshop_revenue_formatted_tbl

# 7.2 gather(): Wide to Long ----

#* Data type for analysis: when you want to analyze data, think
    # about what format it should be in.
#* Typically Two Formats:
    # 1) Numeric (dbl) OR
    # 2) Categorical (factor)

# my way
bikeshop_revenue_formatted_tbl %>% 
    gather(key = "category_1", value = "sales", Mountain, Road) %>% 
    mutate(sales = str_remove_all(sales, pattern = "[$,]"),
           sales = as.numeric(sales))
    
# matts way (escaping special character dollar sign)
bikeshop_revenue_formatted_tbl %>% 
    gather(key = "category_1", value = "sales", Mountain, Road) %>% 
    mutate(sales = str_remove_all(sales, pattern = "\\$|,"),
           sales = as.numeric(sales)) %>% 
    arrange(desc(sales))


# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel)

orderlines_tbl
bikes_tbl

?left_join

orderlines_tbl %>% 
    left_join(y = bikes_tbl, by = c("product.id" = "bike.id"))


# 9.0 Binding Data by Row or by Column with bind_rows() and bind_cols() ----

#* Protip: drop cols, rows as needed + add back later once needed (common practice)

# 9.1 bind_cols()

bike_orderlines_tbl %>% 
    select(-contains("order")) %>% 
    
    bind_cols(
        bike_orderlines_tbl %>% select(order_id)
    )

# 9.2 bind_rows()

train_tbl <- bike_orderlines_tbl %>% 
    slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>% 
    slice((nrow(.)/2 + 1):nrow(.))

test_tbl
train_tbl

# bind two data sets back together
train_tbl %>% 
    bind_rows(test_tbl)


# 10.0 Seperate & Unite ----

bike_orderlines_tbl %>% 
    select(order_date) %>% 
    mutate(order_date = as.character(order_date)) %>% 
    
    # seperate
    separate(col = order_date, 
             into = c("year", "month", "day"),
             sep = "-", 
             remove = FALSE) %>% 
    
    mutate(
        year  = as.numeric(year),
        month = as.numeric(month),
        day   = as.numeric(day)
    ) %>% 
    
    # unite
    unite(order_date_united,
          year, month, day,
          sep = "-",
          remove = FALSE) %>% 
    mutate(order_date_united = as.Date(order_date_united))




