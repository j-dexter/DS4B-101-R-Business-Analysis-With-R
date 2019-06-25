# TIDY DATA EXAMPLE ----

library(tidyverse)
library(readxl)

bikeshop_revenue_tbl <- read_excel("00_data/bikeshop_revenue_formatted_wide.xlsx")

# Wide Format ----

# good for tables in business reports, bad for analysis

bikeshop_revenue_tbl


# Long Format ----

# allows us to analyze sales as a function of category_1 (not possible in wide-format)
bikeshop_revenue_long_tbl <- bikeshop_revenue_tbl %>% 
    select(-Total) %>% 
    gather(key = "category_1", value = "sales", Mountain, Road)

# Analyze 

model <- lm(sales ~., data = bikeshop_revenue_long_tbl)
