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















