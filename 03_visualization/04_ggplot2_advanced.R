# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <-6

# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>% 
    
    select(bikeshop_name, total_price) %>% 
    
    mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump(n = n, w = total_price)) %>% 
    
    group_by(bikeshop_name) %>% 
    summarize(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>% 
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>% 
    
    arrange(desc(bikeshop_name)) %>% 
    
    # Revenue Text
    mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) %>% 
    
    # Cumulative Perecent
    mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>% 
    mutate(cum_pct_text = scales::percent(cum_pct)) %>% 
    
    # Rank
    mutate(rank = row_number()) %>% 
    mutate(rank = case_when(
        rank == max(rank) ~ NA_integer_,
        TRUE ~ rank
    )) %>% 
    
    # Label text
    mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))
    
# Data Visualization
top_customers_tbl %>% 
    
    ggplot(aes(revenue, bikeshop_name)) +
    
    # Geometries
    geom_segment(aes(xend = 0, yend = bikeshop_name), 
                 color = palette_light()[1],
                 size = 1) +
    geom_point(aes(size = revenue),
               color = palette_light()[1]) +
    geom_label(aes(label = label_text),
               hjust = "inward",
               size = 2.7,
               color = palette_light()[1]) +
    
    # Formatting
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
        title = str_glue("Top {n} Customers"),
        subtitle = str_glue("Start: {year(min(bike_orderlines_tbl$order_date))}
                             End: {year(max(bike_orderlines_tbl$order_date))}"),
        caption = str_glue("Top 6 customers contribute
                           51% of purchasing power."),
        x = "Revenue ($M)",
        y = "Customer"
    ) +
    
    theme_tq() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))

# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing prefernce?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation


# Data Visualization




