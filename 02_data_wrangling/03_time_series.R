# DS4B 1-1-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# 1.0 Date & Lubridate Basics ----

# 1.1 Character vs Date/Datetime

bike_orderlines_tbl

order_date_tbl <- bike_orderlines_tbl %>% 
    select(order_date) 

order_date_tbl %>% 
    pull(order_date) %>% 
    class()

# 1.2 Date Classes

order_date_tbl %>% 
    mutate(order_date_chr = as.character(order_date)) %>% 
    mutate(order_date_chr2 = order_date_chr %>% str_c(" 00:00:00")) %>% 
    
    mutate(order_date_date = order_date_chr %>% ymd()) %>% 
    mutate(order_date_dttm = order_date_chr2 %>% ymd_hms())

# 1.3 Lubridate Functions

# Conversion

"06/01/18" %>% mdy() %>% class()

"06/01/18 12:30:15" %>% mdy_hms() %>% class() # date-time class

"January 1, 1985" %>% mdy()


# Extractor

"2011-01-01" %>% ymd() %>% year()

"2011-01-01" %>% ymd() %>% month(label = T, abbr = F)

"2011-01-01" %>% ymd() %>% wday(label = T, abbr = F)

"2011-01-01" %>% ymd() %>% day()


# Helpers

now() # gives exact time and date for right now.

today() # gives todays date


# Periods & Durations: used to add/subtract time

#* Periods: Time spans that take into account daylight savings time & leap year

#* Durations: Time spans that are physical time spans w/out time irregularities

today() + days(12)

today() + ddays(12)

#* So why use period over duration?

# accounts for leap year using period
today() + years(4) # this is a period (accounts for irregularities)

# off by one day b/c leap year not accounted for
today() + dyears(4) # this is a duration (+d @beginning of year: dyear())


# Intervals - Calculates time-based distance

#* Intervals: used to convert 2 timestamps to a "duration"

#* Intervals caputre 2 slices of time as a difference
    # that can be converted to a duration (a physical time difference)

?interval

# set up inteval object using two time-stamps
i <- interval(today(), today() + ddays(12)) 

# get difference in days between these intervals
i / ddays(1) # interval / days = how many days in interval

# get minutes
i / dminutes(1) 

#* Key Takeaway: Divide Interval by some 'Duration' to get into time-unit I want

# to solidify
order_date_tbl %>% 
    mutate(today = today()) %>% 
    mutate(diff_days = interval(order_date, today) / ddays(1))


# 2.0 Time-Based Data Grouping ----

bike_sales_y_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(order_date = ymd(order_date)) %>% 
    mutate(year = year(order_date)) %>% 
    
    # group_by + summarize
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup()

bike_sales_y_tbl

bike_sales_m_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(order_date = ymd(order_date)) %>% 
    mutate(
        year = year(order_date),
        month = month(order_date, label = T, abbr = T)
    ) %>% 
    
    # group_by + summarize
    group_by(year, month) %>% # 5y * 12m = 60 groups
    summarize(sales = sum(total_price)) %>% 
    ungroup()

bike_sales_m_tbl

# Floor Date

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(order_date = ymd(order_date)) %>% 
    mutate(year_month = floor_date(order_date, unit = "month")) %>% 
    
    # group_by + summarize
    group_by(year_month) %>% 
    summarize(sales = sum(total_price))


# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

#* get to % change from one year to the next

#* Lag: aligning past observations (lagging) w/future observations (present)
#* lag() shifts a time series by n lags. Useful for comparing previous values in a vector.

# prep so we can calculate delta between sales and it's lag
bike_sales_y_tbl %>% 
    mutate(sales_lag_1 = lag(sales, n = 1)) %>% 
    
    # Handle NA
    # fill(sales_lag_1, .direction = "up") %>% 
    mutate(sales_lag_1 = case_when(
        is.na(sales_lag_1) ~ sales,
        TRUE ~ sales_lag_1
    )) %>% 
    
    # Diff's & Pct Diffs
    mutate(diff_1 = sales - sales_lag_1) %>%
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>%
    mutate(pct_diff_1_chr = scales::percent(pct_diff_1))

#* protip: always divide differences by your reference point.
    # the reference point for comparisons is the previous year (lag 1)

#* ProTip: The art of data analysis is using these data manipulations
    # to detect interesting features. When we understand differences,
    # we can see that sales in 2013 was abnormally large.

#* protip: It's never a good idea to copy-and-paste code.
    # CREATE A FUNCTION

# Function Basics: 
    # 1) Create a name to bind the function to (make it a VERB)
    # 2) Create arguments for the inputs
    # 3) Create a body to process the inputs and return results

calculate_pct_diff <- function(data) {
    
    data %>% 
        mutate(sales_lag_1 = lag(sales, n = 1)) %>% 
        
        # Handle NA
        fill(sales_lag_1, .direction = "up") %>% 
        
        # Diff's & Pct Diffs
        mutate(diff_1 = sales - sales_lag_1) %>%
        mutate(pct_diff_1 = diff_1 / sales_lag_1) %>%
        mutate(pct_diff_1_chr = scales::percent(pct_diff_1))
}

#* limited b/c only works if column is called 'sales'
#* solution: use Tidy Eval framework to make flexible function for any col name

bike_sales_m_tbl %>% 
    calculate_pct_diff()

# 3.2 Difference from first observation ----

bike_sales_y_tbl %>% 
    mutate(sales_2011 = first(sales)) %>% 
    mutate(diff_2011  = sales - sales_2011) %>% 
    mutate(pct_diff_2011 = diff_2011 / sales_2011) %>% 
    mutate(pct_diff_2011_chr = scales::percent(pct_diff_2011))

#* key concept: grouped mutates (vector operations on grouped data)
    # these are applied to each group independtly - Very Useful!

# comparing % change to Jan of each year
bike_sales_m_tbl %>% 
    
    group_by(year) %>% 
    mutate(sales_jan = first(sales)) %>%
    mutate(
        diff_jan = sales - sales_jan,
        pct_diff_jan = diff_jan / sales_jan,
        pct_diff_jan_chr = scales::percent(pct_diff_jan)
    ) %>% 
    ungroup()

# 4.0 Cumulative Calcualations ----

# cumulative sales percent - yearly
bike_sales_y_tbl %>% 
    mutate(cumulative_sales = cumsum(sales)) %>% 
    mutate(cumulative_sales_pct = cumulative_sales / sum(sales))

# cumulative sales percent - monthly
bike_sales_m_tbl %>% 
    
    group_by(year) %>% 
    mutate(cumulative_sales = cumsum(sales)) %>% 
    mutate(cumulative_sales_pct = cumulative_sales / sum(sales)) %>% 
    mutate(cumulative_sales_pct_chr = scales::percent(cumulative_sales_pct)) %>% 
    ungroup() 
    

# 5.0 Rolling Calculations ----

#* rolling mean: a rolling mean is a simple way to expose the trend in a time series.
#* by taking an avg. of several values, we can reduce the effect of outliers
    # enabling the analyst to visualize a trend.

#* typically use 3-4-5-6 values to get a MOVING AVERAGE

#* window calculation (used in calculating rolling avgs.)
    # a rolling calculation is often called a window calculation.
    # why? b/c you can imagine a window sliding from top to bottom
        # calculating only on observations in the window.

# monthly version of rolling average
bike_sales_m_tbl %>% 
    
    # align pushes the window down
    mutate(roll_mean_3 = rollmean(sales, k = 3, na.pad = T, align = "right", fill = 0)) %>% 
    
    mutate(roll_mean_6 = rollmean(sales, k = 6, na.pad = T, align = "right", fill = NA))

#* protip: you don't want to group moving avgs (i think he is referring to grouping by time here).
    #* grouping by things like product category would be totes find.
#* moving avgs. are meant to detect trend. difficult to do if you group.

# between()
bike_orderlines_tbl %>% 
    
    mutate(order_date = ymd(order_date)) %>% 
    
    filter(order_date %>% between(left = ymd("2012-01-01"), right = ymd("2013-12-31")))

#* protip: filter() performs intermediate calculations. as long as the calculation
    # returns a TRUE/FALSE vector, then you can use it to filter() the rows

# using TRUE/FALSE vector to filter - just like in PANDAS: boolean filtering
bike_orderlines_tbl %>% 
    
    mutate(order_date = ymd(order_date)) %>% 
    
    filter(year(order_date) %in% c(2012, 2013))

#



