# DS4B 101-R: $ FOR BUSINESS ANALYSIS ----
# PACKAGE INSTALLATION ----

# CRAN Packages ----
pkgs_cran <- c(
    # File System
    "fs",          # working with the file system
    
    # Import
    "readxl",      # reading exel files
    "writexl",     # saving data as excel files
    "odcb",        # connecting to SQLite databases
    
    # Tidy, Transform, & Visualize
    "tidyverse",   # dplyr, ggplot2, tibble, tidyr, readr, purr, stringr, forcats
    "lubridate",   # working with dates and times
    "tidyquants",  # used mainly for the ggplot plotting theme
    
    # Model
    "tidymodels",  # installs broom, infer, recipes, rsample, & yardstick
    "umap",        # used for visualizing clusters
    
    # Other
    "devtools"     # used to install non-CRAN packages
)

install.packages("fs")
install.packages(pkgs_cran)
