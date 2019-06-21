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

orderlines_tbl #* connective tissue between bikes and bikeshops


# 4.0 Joining Data

orderlines_tbl 
bikes_tbl

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

#* dplyr is the workhorse here which is really a grammar of data manipulation.
#* dplyr contains many functions/etc., but most imp are the five verbs.
#* think of wrangling data as the 'manipulation and cleaning' stages of analysis.

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
    
    # Seperate description into category.1, category.2, and frame.material
    separate(description,
             into   = c("category.1", "category.2", "frame.material"),
             sep    = " - ",
             remove = TRUE) %>% 
    
    # Seperate location into city and state
    separate(location,
             into   = c("city", "state"),
             sep    = ", ",
             remove = FALSE) %>% 
    
    # Price extended
    mutate(total.price = price * quantity) %>% 
    
    # Reorganize
    select(-...1, -location) %>% 
    select(-ends_with(".id")) %>% 
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
    
    # Reorder columns
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price,
           everything()) %>% 
    
    # Renaming columns
    rename(order_date = order.date) %>% 
    set_names(names(.) %>% str_replace_all("\\.", "_")) #* useful for renaming mult cols programatically
    # \\ used as regex to escape '.' character that does something elsempse()

bike_orderlines_wrangled_tbl %>% glimpse()
    
#* names() returns all the col names of a tibble as a character vector.
#* PRO TIP: Using the dot(.) enables passing the incoming tibble to multiple spots in the function.
#* dot(.) is used in dplyr pipes to supply the incoming df/tbl in another part of function.
    #* i get it now: the dot allows me to reuse the tbl as an argument in other places
        # besides just the 1st argment of the function it's fed into. AWESOME!!!
