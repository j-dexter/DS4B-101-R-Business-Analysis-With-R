# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation

order_value_tbl <- bike_orderlines_tbl %>%
    
    select(order_id, order_line, total_price, quantity) %>%
    
    group_by(order_id) %>% 
    summarize(
        total_quantity = sum(quantity),
        total_price    = sum(total_price)
    ) %>% 
    ungroup()

# Scatter Plotssss
order_value_tbl %>% 
    
    ggplot(aes(x = total_quantity, y = total_price)) +
    
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = FALSE)


# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation

revenue_by_month <- bike_orderlines_tbl %>% 
    
    select(order_date, total_price) %>% 
    
    mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>% 
    
    group_by(year_month) %>% 
    summarize(revenue = sum(total_price)) %>% 
    ungroup()

# Line Plot

#* protip: geom smooth uses underlying algorithms. using the help documentation,
    # you can drill into the algorithm parameters and find out which parameters can be adjusted.

revenue_by_month %>% 
    
    ggplot(aes(year_month, revenue)) +
    
    geom_line(size = 0.5) +
    geom_smooth(method = "loess", span = 0.2)


# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>% 
    
    select(category_2, total_price) %>% 
    
    group_by(category_2) %>%
    summarise(revenue = sum(total_price)) %>% 
    ungroup()

# Bar Plot
revenue_by_category_2_tbl %>% 
    
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>% 
    
    ggplot(aes(category_2, revenue)) +
    
    geom_col(fill = "#2c3e50") +
    coord_flip()


# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable

# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl %>% 
    
    distinct(model, price) %>% 
    
    ggplot(aes(price)) +
    
    geom_histogram(bins = 25, fill = "#2c3e50", color = "white")

# Goal: Unit price of bicylce, segmenting by frame material
# Histogram
bike_orderlines_tbl %>% 
    
    distinct(price, model, frame_material) %>% 
    
    ggplot(aes(price, fill = frame_material)) +
    
    geom_histogram() +
    
    facet_wrap(~ frame_material, ncol = 1) +
    
    scale_fill_tq() + 
    theme_tq()

# Density
bike_orderlines_tbl %>% 
    
    distinct(price, model, frame_material) %>% 
    
    ggplot(aes(price, fill = frame_material)) +
    
    geom_density(alpha = 0.5) +
    
    scale_fill_tq() +
    theme_tq() +
    theme(legend.position = "bottom")


# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions

# Goal: Unit price of models, segmenting by category 2

# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>% 
    
    select(category_2, model, price) %>% 
    distinct() %>% 
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>% 
    
    ggplot(aes(category_2, price)) +
    
    geom_boxplot() +
    coord_flip() +
    theme_tq()

# Violin Plot & Jitter Plot
unit_price_by_cat_2_tbl %>% 
    
    ggplot(aes(category_2,price)) +
    
    geom_jitter(width = 0.15, color = "#2c3e50") +
    geom_violin(alpha = 0.5) +
    
    coord_flip() +
    theme_tq()


# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>% 
    
    select(order_date, total_price) %>% 
    
    mutate(year = year(order_date)) %>% 
    
    group_by(year) %>% 
    summarize(revenue = sum(total_price)) %>% 
    ungroup()

# Adding text to bar chart

revenue_by_year_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    
    geom_col(fill = palette_light()[1]) +
    geom_smooth(method = "lm", se = FALSE) +
    
    geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")),
                  vjust = 1.5, color = "white") +
    
    geom_label(label = "Major Demand This Year",
               vjust = -0.5,
               size  = 5,
               fill  = palette_light()[6],
               color = "white",
               fontface = "italic",
               data  = revenue_by_year_tbl %>% filter(year == 2013)) +
    
    theme_tq() +
    expand_limits(y= 2e7)

# Filtering labels to highlight a point





