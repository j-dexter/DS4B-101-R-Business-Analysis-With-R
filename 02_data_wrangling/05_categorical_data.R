# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl


# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data
#* special data type that contains both character features and numeric features

# Why do we want factors? 
#* allows us to work w/categorical data 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much easier (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type of categorical data.

# 2.0 Motivational Example ----

# Manipulation
sales_by_cat_2_tbl <- bike_orderlines_tbl %>% 
    
    select(category_2, total_price) %>% 
    
    group_by(category_2) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    arrange(desc(sales)) %>% 
    mutate(category_2 = category_2 %>% as_factor() %>% fct_rev())

#* why did that work?
    # the factor functions set the order by labeling the categorical 
    # with a numeric feature (in the background). this locks in the order.

# Plotting
sales_by_cat_2_tbl %>% 
    ggplot(aes(x = sales, y = category_2)) +
    geom_point(size = 5) +
    labs(title = "Sales by Category 2") +
    scale_x_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    expand_limits(x = 0)

#* always good to make a quick function if we'll be reusing a workflow
#* matt: let's do that!

# Make Function
plot_sales <- function(data) {
    data %>% 
        ggplot(aes(x = sales, y = category_2)) +
            geom_point(size = 5) +
            labs(title = "Sales by Category 2") +
            scale_x_continuous(labels = scales::dollar_format()) +
            theme_tq() +
            expand_limits(x = 0)
}

# Test Function
sales_by_cat_2_tbl %>% 
    plot_sales()


# 3.0 Forcats Basics ----

# 3.1 Inspecting Factors ----

# Vector
sales_by_cat_2_tbl %>% pull(category_2) %>% levels()

sales_by_cat_2_tbl %>% pull(category_2) %>% as.numeric()

# Tibble
sales_by_cat_2_tbl %>%
    mutate(category_2 = category_2 %>% fct_rev()) %>% 
    mutate(
        label = category_2 %>% as.character(),
        value = category_2 %>% as.numeric()
    )

#* key takeaway so far: factors HAVE both a label (seen) and a value (hidden)

# 3.2 Creating Factors: as_factor() vs as.factor() ----

#* forcats::as_factor() assigns factor values BASED ON ORDER in the vector
#* base::as.factor() assigns factor values BASED ON ALPHABETICAL ORDER in the vector

sales_by_cat_2_tbl %>% 
    mutate(
        category_2           = as.character(category_2),
        category_2_as_factor = as_factor(category_2) %>% as.numeric(),
        category_2_as.factor = as.factor(category_2) %>% as.numeric()
    )


# 3.3 Reordering Factors: fct_reorder() and fct_rev() ----

#* simply using as_factor() is like saving a word doc as pdf - it freezes the current 'arrangement'

sales_by_cat_2_tbl %>% 
    arrange(desc(sales)) %>% 
    mutate(sales_negative = -sales) %>% 
    mutate(
        category_2 = category_2 %>% fct_reorder(sales_negative),
        values  = category_2 %>% as.numeric()) %>% 
    
    plot_sales()


# 3.4 Time-Based Reordering: fct_reordeer2() ----

sales_by_cat_2_q_tbl <- bike_orderlines_tbl %>% 
    
    mutate(order_date = order_date %>% floor_date("quarter") %>% ymd()) %>% 
    
    group_by(category_2, order_date) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup()

# change from alpha ordering to desired order
sales_by_cat_2_q_tbl %>% 
    
    mutate(category_2 = category_2 %>% fct_reorder2(order_date, sales)) %>% 
    
    ggplot(aes(x = order_date, y = sales, color = category_2)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ category_2) +
    
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))


# 3.5 Creating "Other" Category - fct_lump() & fct_relevel() ----

sales_by_cat_2_tbl %>% 
    
    mutate(category_2 = category_2 %>% fct_lump(n = 6,
                                                w = sales,
                                                other_level = "All Other Bike Categories")) %>% 
    group_by(category_2) %>% 
    summarize(sales = sum(sales)) %>% 
    
    # manually reorder factors
    mutate(category_2 = category_2 %>% fct_relevel("All Other Bike Categories", after = 0)) %>% 
    
    plot_sales()



# 










#