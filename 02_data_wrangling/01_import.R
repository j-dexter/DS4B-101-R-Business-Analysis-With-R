# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----

# 1.0 Load libraries ----

# Cotains readr
library(tidyverse)

# Excel connection
library(readxl)
library(writexl)

# Database connection
library(odbc)
library(RSQLite)


# 2.0 readr ----

#* readr tries to guess data types held in each column using the col_...() functions.
#* really cool part of 'readr' is the readr::problems function to identify problematic rows.

# 2.1 CSV ----

bike_orders_csv_tbl <- readr::read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv")

bike_orders_csv_tbl %>% 
    slice(7916)

# 2.2 RDS ----

#* saves exactly what we left off with 
#* protip: matt saves files as rds 99% of the time

bike_orders_rds_tbl <- readr::read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orders_rds_tbl %>% 
    slice(7916)


# 3.0 Excel ----

bike_orders_excel_tbl <- readxl::read_excel("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx",
                   sheet = "Sheet1")

readxl::excel_sheets("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")


# 4.0 Databases

con <- RSQLite::dbConnect(drv = SQLite(), dbname = "00_data/chinook/Chinook_Sqlite.sqlite")

dbListTables(con)

album_tbl <- tbl(con, "Album") %>% collect()

# still connected to db
con

# disconnecting from db (best-practice)
dbDisconnect(con)

