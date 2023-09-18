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

setwd(wd$data)

selected_parks <- data.frame(park_url = c("https://rcdb.com/4540.htm", 
                                         "https://rcdb.com/4593.htm", 
                                         "https://rcdb.com/4734.htm", 
                                         "https://rcdb.com/4736.htm", 
                                         "https://rcdb.com/4546.htm", 
                                         "https://rcdb.com/4552.htm", 
                                         "https://rcdb.com/4533.htm", 
                                         "https://rcdb.com/4588.htm", 
                                         "https://rcdb.com/4578.htm", 
                                         "https://rcdb.com/4541.htm", 
                                         "https://rcdb.com/4539.htm", 
                                         "https://rcdb.com/4542.htm", 
                                         "https://rcdb.com/4544.htm", 
                                         "https://rcdb.com/4529.htm", 
                                         "https://rcdb.com/4532.htm", 
                                         "https://rcdb.com/4534.htm", 
                                         "https://rcdb.com/4530.htm",
                                         "https://rcdb.com/4531.htm", 
                                         "https://rcdb.com/4545.htm", 
                                         "https://rcdb.com/4535.htm", 
                                         "https://rcdb.com/4538.htm", 
                                         "https://rcdb.com/4565.htm", 
                                         "https://rcdb.com/4570.htm", 
                                         "https://rcdb.com/4548.htm", 
                                         "https://rcdb.com/4711.htm", 
                                         "https://rcdb.com/4536.htm", 
                                         "https://rcdb.com/4558.htm", 
                                         "https://rcdb.com/4543.htm", 
                                         "https://rcdb.com/9250.htm", 
                                         "https://rcdb.com/4581.htm",
                                         "https://rcdb.com/4746.htm",
                                         "https://rcdb.com/4553.htm", 
                                         "https://rcdb.com/4579.htm", 
                                         "https://rcdb.com/4564.htm", 
                                         "https://rcdb.com/4599.htm", 
                                         "https://rcdb.com/4560.htm", 
                                         "https://rcdb.com/4576.htm", 
                                         "https://rcdb.com/4601.htm", 
                                         "https://rcdb.com/4703.htm", 
                                         "https://rcdb.com/4574.htm", 
                                         "https://rcdb.com/5320.htm", 
                                         "https://rcdb.com/4554.htm",
                                         "https://rcdb.com/4563.htm", 
                                         "https://rcdb.com/4596.htm", 
                                         "https://rcdb.com/4557.htm", 
                                         "https://rcdb.com/4584.htm", 
                                         "https://rcdb.com/18828.htm", 
                                         "https://rcdb.com/4683.htm", 
                                         "https://rcdb.com/4682.htm", 
                                         "https://rcdb.com/4597.htm", 
                                         "https://rcdb.com/4547.htm",
                                         "https://rcdb.com/15593.htm", 
                                         "https://rcdb.com/4575.htm", 
                                         "https://rcdb.com/4559.htm", 
                                         "https://rcdb.com/4646.htm", 
                                         "https://rcdb.com/4561.htm"))


selected_parks

# Build crosswalk_pr.csv

# Build park_inventory.csv

# Build ride_inventory.csv

# Build ride_specs.csv

# Build ride_status.csv