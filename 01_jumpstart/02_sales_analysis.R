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


# 6.0 Business Insights ---- 

# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Selecting columns to focus on Adding a year column
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    
    # Grouping by year, and summarizing sales
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # $ Format Text
    mutate(sales_text = scales::dollar(sales))

# Step 2 - Visualize

#* PRO TIP: visualization is one of the most important skills a DS needs to know.
#* ggplots: are built by adding successive layers including geometries/formatting elements
#* ggplots: grammar of graphics (whereas dplyr is the grammar of data manipulation)
#* access hexcodes: palette_light()
palette_light()

sales_by_year_tbl %>% 
    
    # Setup canvas with year (x-axis) and sales (y-axis)
    ggplot(aes(x = year, y = sales)) +
    
    # Geometries
    geom_col(fill = "#2c3e50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward Trend",
        x = "",
        y = "Revenue"
    )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Select columns and add a year column
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>% 
    
    # Groupby and summarize on the year and category 2
    group_by(year, category_2) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # Format $ Text
    mutate(sales_text = scales::dollar(sales))

sales_by_year_cat_2_tbl

# Step 2 - Visualize

#* Fill: we can map cols to control the fill by categorical or continuous variables
#* Pro-tip: use facet_wrap w/scales = "free_y" to show trend. 
#* Pro-tip: use facet_wrap w/out scales to show magnitude. 
#* As soon as a map a col to something like 'fill', then I can use that
    # in things like 'scale_fill_tq'

sales_by_year_cat_2_tbl %>% 
    
    # Set up x, y, fill
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    
    # Geometries
    geom_col() +
    geom_smooth(method = "lm", se = F) +
    
    # Facet: splits plots into multiple plots by a categorical feature
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    # Formatting
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )

#* DATA VISUALIZATION PROCESS 
    # notes from Matts explanation video on the 
    # natural structure to making visualizations (see pdf).

#* The Process
    # 1) Collecting data from 1 or more sources
    # 2) Join (data_joined_tbl) and clean the data (data_wrangled_tbl)
    # 3) Visualize using Two-Step process
            # a) Manipulate data-wrangled (data_by_group_tbl)
            # b) Visualize

#* Most Important Step: Cleaned Data
    # Why? All data manipulation/visualization branches from here.
    # protip: save cleaned data to save time/repetive work 

#* The process for joining can also be saved: Data Processing Pipeline

# 7.0 Writing Files ----

fs::dir_create("00_data/bike_sales/data_wrangled_student")

# 7.1 Excel ----

#* Excel is great when others may want access to your data that are excel users.
    # e.g., many bus. intelligence analyst use Excel not R.

bike_orderlines_wrangled_tbl %>% 
    write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")

# 7.2 CSV ----

#* CSV is a good option when others may use diff. languages such as pyton, java, or C++

bike_orderlines_wrangled_tbl %>% 
    write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----

#* Use RDS when you want to save ANY OBJECT. Not just tabular data.
    # Save models, plots, anything! It's fast and preserves the object
    # structure exactly (unlike Excel & CSV)

bike_orderlines_wrangled_tbl %>% 
    write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")
dd

