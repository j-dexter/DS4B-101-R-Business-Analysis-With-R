# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl



# 1.0 Working with Colors ----

# 1.1 Color Conversion ----

# Named Colors
colors()

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    
    geom_col(fill = rgb(44, 62, 80, maxColorValue = 255) )

# To RGB
col2rgb("slateblue")

col2rgb("#2c3e50")

# To HEX
rgb(44, 62, 80, maxColorValue = 255) 

# 1.2 Color Palettes ----

# tidyquant
tidyquant::palette_light()

palette_light()[2] %>% col2rgb()

# Brewer
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info

# Grab specific palettes
RColorBrewer::brewer.pal(n = 8, name = "Blues")

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    
    geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])

# Viridis
viridisLite::viridis((n = 20))

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    
    geom_col(fill = viridisLite::viridis((n = 20))[1])

# 2.0 Aesthetic Mappings ----

# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects

#* defining locally (in geom) vs. defining aestethics globabolly (ggplot)

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    geom_line(aes(color = category_2)) +
    geom_point(color = "dodgerblue", size = 3)


# 2.2 Fill  -----
# - Used with fill of rectangular objects 

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    geom_col(aes(fill = category_2))

# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue)) +
    geom_line(aes(color = category_2), size = 2) +
    geom_point(aes(size = revenue))


# 3.0 Faceting ----
# - Great way to tease out variation by category #*

# Goal: Sales annual sales by category 2

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(color = "black") +
    geom_smooth(method = "lm", se = F) +
    
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    expand_limits(y = 0)


# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars

sales_by_year_category_2_tbl %>% 
    
    ggplot(aes(year, revenue, fill = category_2)) +
    # geom_col(position = "stack")
    # geom_col(position = "dodge")
    geom_col(position = position_dodge(width = 0.9), color = "white")

# Stacked Area




# 5.0 Scales (Colors, Fills, Axis) ----

# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale


# Plot 2: Faceted Plot, Color = Discrete Scale


# Plot 3: Stacked Area Plot




# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)




# Color by Category 2 (Discrete Scale)



# Fill by Category 2

 


# 5.3 Axis Scales ----





# 6.0 Labels ----




# 7.0 Themes  ----




# 8.0 Putting It All Together ----







