# roller coaster evolution

library(dplyr)
library(readr)

rm(list=ls());cat('\f');gc()

# DIRS ----
wd        <- list()
wd$data   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/data"
wd$R      <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/R"
wd$output <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution/output"
wd$home   <- "C:/Users/bende/Documents/R/play/rollercoaster_evolution"


# park_inventory.csv fixes----
setwd(wd$data)

# load data
park_inventory <- read_csv("park_inventory.csv")

# change data
park_inventory[park_inventory$Name == "Super Coaster" & park_inventory$park_name == "Cedar Point",]$Closed <- "19641111"
park_inventory[park_inventory$Name == "Super Coaster" & park_inventory$park_name == "Cedar Point",]$yr_closed <- 1964

park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Dorney Park", park_inventory$park_name),]$Closed <- "19801111"
park_inventory[park_inventory$Name == "Kiddie Coaster" & grepl("Dorney Park", park_inventory$park_name),]$yr_closed <- 1980

# write data
write_csv(park_inventory, 
          file = "park_inventory.csv")