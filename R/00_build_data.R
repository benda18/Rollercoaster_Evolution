# Build Data
library(dplyr)
library(janitor)
#library(here)  # project-oriented workflow // '??here'
library(rvest)
library(xml2)
library(glue)
library(lubridate)
#library(ggplot2)
library(readr)


rm(list=ls());cat('\f');gc()

# FUNS ----

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"


# VARS ----

# BUILD ----
# Build selected_parks.csv

# Build crosswalk_pr.csv

# Build park_inventory.csv

# Build ride_inventory.csv

# Build ride_specs.csv

# Build ride_status.csv